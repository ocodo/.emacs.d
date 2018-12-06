;;; cabledolphin.el --- capture Emacs network traffic  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>
;; Keywords: comm
;; Package-Version: 20160204.938
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (seq "1.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Cabledolphin captures network traffic to and from Emacs Lisp
;; processes, and writes it into a PCAPNG file, which can be read by
;; tools such as tcpdump and Wireshark.
;;
;; Since Cabledolphin extracts the data on the Emacs Lisp level, it
;; writes the packet capture in cleartext even if the connection is
;; TLS-encrypted.
;;
;; While it doesn't get hold of actual packet headers, it synthesises
;; TCP/IP headers to the minimum extent required to keep Wireshark
;; happy.
;;
;; Available commands:
;;
;; - `cabledolphin-trace-new-connections': start capturing packets for
;;   any new connections whose name matches a certain regexp.
;;
;; - `cabledolphin-trace-existing-connection': start capturing packets
;;   for an existing connection.
;;
;; - `cabledolphin-set-pcap-file': change the file that data is
;;   written to.
;;
;; - `cabledolphin-stop': stop capturing, and stop matching new
;;   connections.
;;
;; If you prefer output in "classic" PCAP format, set
;; `cabledolphin-output-type' to `pcap' before calling
;; `cabledolphin-set-pcap-file'.

;;; Code:

(require 'bindat)
(require 'seq)

(defvar cabledolphin-pcap-file nil
  "File to which captured data is appended.")

(defvar cabledolphin-output-type 'pcapng
  "The file format to output.  Either `pcap' or `pcapng'.")

(defvar cabledolphin-connection-name-regexps ()
  "Trace new connections whose name matches one of these regexps.
See `cabledolphin-trace-new-connections'.")

(defvar cabledolphin--dns-names (make-hash-table :test 'equal))

;;; Pcap bindat specs
;; See pcap file format spec at
;; https://wiki.wireshark.org/Development/LibpcapFileFormat
(defconst cabledolphin--pcap-header-bindat-spec
  '(
    ;; Give magic number as a vector, so this works on 32-bit Emacsen.
    (magic-number vec 4 u8)
    (version-major u16)
    (version-minor u16)
    ;; thiszone is actually signed, but bindat doesn't support signed
    ;; integers.  Doesn't matter: we set it to 0.
    (thiszone u32)
    (sigfigs u32)
    (snaplen u32)
    (network u32))
  "Bindat spec for big-endian pcap file header.")

(defconst cabledolphin--pcap-packet-header-bindat-spec
  '(
    ;; Specify seconds in two 16-bit parts, for compatibility with 32-bit Emacsen.
    (ts-sec-high u16)
    (ts-sec-low u16)
    (ts-usec u32)
    (incl-len u32)
    (orig-len u32))
  "Bindat spec for big-endian pcap packet header.")

;;; Pcap-ng bindat specs
;; See pcap-ng spec at:
;; https://github.com/pcapng/pcapng

(defconst cabledolphin--pcapng-block-bindat-spec
  '(
    ;; Give block type as a vector, so this works on 32-bit Emacsen.
    (type vec 4 u8)
    (total-length u32)
    (body vec (eval (- last 12)) u8)
    ;; The block length appears twice in the block.
    (total-length u32))
  "Bindat spec for pcapng block.")

(defconst cabledolphin--pcapng-section-header-block-type
  [#x0a #x0d #x0d #x0a])

(defconst cabledolphin--pcapng-section-header-bindat-spec
  '((byte-order-magic vec 4 u8)
    (major-version u16)
    (minor-version u16)
    (section-length vec 8 u8))
  "Bindat spec for pcapng section header block, without options.
This generates big-endian output, so the \"byte order magic\"
should be set appropriately.")

(defconst cabledolphin--pcapng-option-bindat-spec
  '((code u16)
    (length u16)
    (value vec (length) u8)
    (align 4))
  "Bindat spec for pcapng option.")

(defconst cabledolphin--pcapng-interface-description-block-type
  [0 0 0 1])

(defconst cabledolphin--pcapng-interface-description-bindat-spec
  '((link-type u16)
    (reserved u16)
    (snap-len u32))
  "Bindat spec for pcapng interface description block, without options.")

(defconst cabledolphin--pcapng-enhanced-packet-block-type
  [0 0 0 6])

(defconst cabledolphin--pcapng-enhanced-packet-bindat-spec
  '((interface-id u32)
    ;; The timestamp is a 64-bit value, saved as two 32-bit words.
    ;; To make this work on 32-bit Emacsen, let's split it into
    ;; four 16-bit integers.
    (timestamp vec 4 u16)
    (captured-packet-length u32)
    (original-packet-length u32)
    (packet-data vec (captured-packet-length) u8)
    (align 4))
  "Bindat spec for pcapng enhanced packet block, without options.")

(defconst cabledolphin--pcapng-name-resolution-block-type
  [0 0 0 4])

(defconst cabledolphin--pcapng-name-resolution-record-bindat-spec
  '((type u16)
    (value-length u16)
    (value vec (value-length) u8)
    (align 4))
  "Bindat spec for a name resolution record.
Concatenate several of these to create a name resolution block.")

;;; network packet bindat specs

(defconst cabledolphin--ipv4-bindat-spec
  '((version-and-header-length u8)
    (dscp-ecn u8)
    (total-length u16)
    (identification u16)
    (flags-and-fragment-offset u16)
    (ttl u8)
    (protocol u8)
    (header-checksum u16)
    (src-addr vec 4 u8)
    (dest-addr vec 4 u8))
  "Bindat spec for IPv4 packet header, without options.")

(defconst cabledolphin--ipv6-bindat-spec
  '((version-etc u8)
    (fill 3)
    (payload-length u16)
    (next-header u8)
    (hop-limit u8)
    (src-addr vec 8 u16)
    (dest-addr vec 8 u16))
  "Bindat spec for IPv6 packet header.")

(defconst cabledolphin--tcp-bindat-spec
  '((src-port u16)
    (dest-port u16)
    (seq u32)
    (ack u32)
    (data-offset-and-reserved u8)
    (flags bits 1)
    (window-size u16)
    (checksum u16)
    (urgent-pointer u16))
  "Bindat spec for TCP header, without options.")

;;; Functions

;;;###autoload
(defun cabledolphin-set-pcap-file (file)
  "Set the file where captured network data is written to.

If the file doesn't exist, or is empty, a PCAP file header will
be written to it.  Otherwise, any new data will be appended to
the file."
  (interactive "FWrite data to pcap file: ")
  (setq cabledolphin-pcap-file file)

  ;; If the file doesn't exist, or is empty, we need to write a pcap
  ;; header.
  (let ((attributes (file-attributes file)))
    (when (or (null attributes)
	      (zerop (nth 7 attributes)))
      (with-temp-buffer
	(cl-ecase cabledolphin-output-type
	  (pcap
	   (insert (bindat-pack cabledolphin--pcap-header-bindat-spec
				'((magic-number . [#xa1 #xb2 #xc3 #xd4])
				  (version-major . 2)
				  (version-minor . 4)
				  (thiszone . 0)
				  (sigfigs . 0)
				  (snaplen . 65535)
				  ;; 101 is LINKTYPE_RAW, for raw IPv4/IPv6
				  (network . 101)))))
	  (pcapng
	   (let ((section-header
		  (bindat-pack
		   cabledolphin--pcapng-section-header-bindat-spec
		   '((byte-order-magic . [#x1a #x2b #x3c #x4d])
		     (major-version . 1)
		     (minor-version . 0)
		     (section-length . [#xff #xff #xff #xff #xff #xff #xff #xff]))))
		 (interface-description
		  (bindat-pack
		   cabledolphin--pcapng-interface-description-bindat-spec
		   ;; 101 is LINKTYPE_RAW, for raw IPv4/IPv6
		   '((link-type . 101)
		     (reserved . 0)
		     ;; According to the spec, snap-len 0 means "unlimited",
		     ;; but tcpdump doesn't like that.  Set it to 65536 instead.
		     (snap-len . 65536)))))
	     (insert (cabledolphin--pcapng-block
		      cabledolphin--pcapng-section-header-block-type
		      section-header
		      ;; shb_os
		      3 (emacs-version)
		      ;; shb_userappl
		      4 "cabledolphin"))
	     (insert (cabledolphin--pcapng-block
		      cabledolphin--pcapng-interface-description-block-type
		      interface-description
		      ;; if_description
		      3 "virtual interface, synthetic TCP/IP packets")))))

	(let ((coding-system-for-write 'binary))
	  (write-region (point-min) (point-max) file nil :silent))
	(clrhash cabledolphin--dns-names)))))

;;;###autoload
(defun cabledolphin-trace-existing-connection (process)
  "Start capturing network data for an existing connection."
  (interactive
   (list
    (get-process
     (completing-read
      "Capture network traffic for: "
      (mapcar
       'process-name
       (cl-remove-if-not 'listp (process-list) :key 'process-contact))))))
  (unless cabledolphin-pcap-file
    (call-interactively 'cabledolphin-set-pcap-file))
  (process-put process :cabledolphin-traced t)
  (process-put process :cabledolphin-seq-in 0)
  (process-put process :cabledolphin-seq-out 0)
  (add-function :before (process-filter process) 'cabledolphin--filter)
  (advice-add 'set-process-filter :filter-args 'cabledolphin--set-process-filter)
  (advice-add 'process-send-string :before 'cabledolphin--process-send-string)
  (advice-add 'process-send-region :before 'cabledolphin--process-send-region))

;;;###autoload
(defun cabledolphin-trace-new-connections (regexp)
  "Capture data for any new connections matching REGEXP.
Matching is done against the process name."
  (interactive "sCapture network traffic for new connections matching regexp: ")
  (unless cabledolphin-pcap-file
    (call-interactively 'cabledolphin-set-pcap-file))
  (push regexp cabledolphin-connection-name-regexps)
  (advice-add 'make-network-process :filter-return 'cabledolphin--maybe-trace-new))

(defun cabledolphin-stop ()
  "Stop all tracing."
  (interactive)
  (advice-remove 'make-network-process 'cabledolphin--maybe-trace-new)
  (advice-remove 'set-process-filter 'cabledolphin--set-process-filter)
  (advice-remove 'process-send-string 'cabledolphin--process-send-string)
  (advice-remove 'process-send-region 'cabledolphin--process-send-region)
  (dolist (process (process-list))
    (remove-function (process-filter process) 'cabledolphin--filter)))

(defun cabledolphin--set-process-filter (args)
  (let ((process (car args))
	(filter-function (cadr args)))
    (when (process-get process :cabledolphin-traced)
      (when (symbolp filter-function)
	(setq filter-function (symbol-function filter-function)))

      (unless (advice-function-member-p 'cabledolphin--filter filter-function)
	(add-function :before (var filter-function) 'cabledolphin--filter)))
    (list process filter-function)))

(defun cabledolphin--maybe-trace-new (process)
  ;; This is a filter-return function, but we never want to change the
  ;; return value.  Take care of that with `prog1'.
  (prog1 process
    (let ((name (process-name process))
	  (contact (process-contact process)))
      (when (and
	     ;; First ensure it's actually a network process.
	     (listp contact)
	     ;; Then check the regexps.
	     (cl-some
	      (lambda (regexp) (string-match-p regexp name))
	      cabledolphin-connection-name-regexps))
	(cabledolphin-trace-existing-connection process)))))

(defun cabledolphin--filter (process data)
  (when (process-get process :cabledolphin-traced)
    (cabledolphin--write-packet
     process data
     :seq-key :cabledolphin-seq-in
     :from :remote
     :to :local)))

(defun cabledolphin--process-send-region (process start end)
  (when (process-get process :cabledolphin-traced)
    (cabledolphin--process-send-string process (buffer-substring start end))))

(defun cabledolphin--process-send-string (process data)
  (when (process-get process :cabledolphin-traced)
    (cabledolphin--write-packet
     process data
     :seq-key :cabledolphin-seq-out
     :from :local
     :to :remote)))

(defun cabledolphin--pcapng-block (block-type block-data &rest option-codes-values)
  (let (options)
    (while option-codes-values
      (let ((code (pop option-codes-values))
	    (value (pop option-codes-values)))
	(push
	 (bindat-pack
	  cabledolphin--pcapng-option-bindat-spec
	  `((code . ,code)
	    (length . ,(length value))
	    (value . ,value)))
	 options)))
    (when options
      ;; If there are options, the last option must be opt_endofopt.
      (push
       (bindat-pack
	cabledolphin--pcapng-option-bindat-spec
	'((code . 0)
	  (length . 0)
	  (value . "")))
       options)
      (setq options (nreverse options))
      (setq block-data (apply 'vconcat block-data options))))
  (bindat-pack
   cabledolphin--pcapng-block-bindat-spec
   `((type . ,block-type)
     (total-length . ,(+ 12 (length block-data)))
     (body . ,block-data))))

(cl-defun cabledolphin--write-packet
    (process data &key seq-key ((:from from-key)) ((:to to-key)))
  ;; Ensure that data is binary.  This is idempotent.
  (setq data (encode-coding-string data 'binary t))
  (with-temp-buffer
    (let* ((time (current-time))
	   (time-high (nth 0 time))
	   (time-low (nth 1 time))
	   (time-usec (nth 2 time))
	   (len (length data))
	   (contact (process-contact process t))
	   (from (plist-get contact from-key))
	   (to (plist-get contact to-key))
	   (seq (process-get process seq-key))
	   (tcp-ip-packet (cabledolphin--tcp-ip-packet from to seq data))
	   (total-len (length tcp-ip-packet)))
      
      (cl-ecase cabledolphin-output-type
	(pcap
	 (insert (bindat-pack cabledolphin--pcap-packet-header-bindat-spec
			      `((ts-sec-high . ,time-high)
				(ts-sec-low . ,time-low)
				(ts-usec . ,time-usec)
				(incl-len . ,total-len)
				(orig-len . ,total-len)))
		 tcp-ip-packet))

	(pcapng
	 ;; If we know of a mapping between a hostname and an IP
	 ;; address, write a name resolution block if we haven't
	 ;; before.
	 (let ((remote-address-port (plist-get contact :remote))
	       (host (plist-get contact :host)))
	   ;; Check that host is not just an IP address.
	   (unless (string-match-p "^[0-9.:]+$" host)
	     (let* ((remote-address
		     (seq-take remote-address-port (1- (length remote-address-port))))
		    (hostz (vconcat host [0]))
		    (key (cons remote-address host)))
	       (unless (gethash key cabledolphin--dns-names)
		 (insert
		  (cabledolphin--pcapng-block
		   cabledolphin--pcapng-name-resolution-block-type
		   (vconcat
		    (cl-ecase (length remote-address)
		      (4
		       (let ((value (vconcat remote-address hostz)))
			 (bindat-pack
			  cabledolphin--pcapng-name-resolution-record-bindat-spec
			  ;; nrb_record_ipv4 = 1
			  `((type . 1)
			    (value-length . ,(length value))
			    (value . ,value)))))
		      (8
		       (let ((value (vconcat (cl-mapcan
					      (lambda (sixteen-bits)
						(list (ash sixteen-bits -8)
						      (logand sixteen-bits #xff)))
					      remote-address)
					     hostz)))
			 (bindat-pack
			  cabledolphin--pcapng-name-resolution-record-bindat-spec
			  ;; nrb_record_ipv6 = 2
			  `((type . 2)
			    (value-length . ,(length value))
			    (value . ,value))))))
		    (bindat-pack
		     cabledolphin--pcapng-name-resolution-record-bindat-spec
		     ;; nrb_record_end = 0
		     '((type . 0)
		       (value-length . 0)
		       (value . ""))))))
		 (puthash key t cabledolphin--dns-names)))))

	 ;; For pcapng, the timestamp is the number of microseconds
	 ;; since the epoch, as a 64-bit integer.  Need to do some
	 ;; conversion here.  Let's use calc, as it has bignum
	 ;; support.

	 (let* ((total-usec
		 (calc-eval
		  '("or(or(lsh($, 16), $$) * 1000000, $$$)" calc-word-size 64)
		  'raw
		  time-high time-low time-usec))
		(total-usec-words
		 (calc-eval
		  '("[rsh($, 48), and(rsh($, 32), 16#ffff), and(rsh($, 16), 16#ffff), and($, 16#ffff)]" calc-word-size 64)
		  'rawnum
		  total-usec))
		(timestamp (apply 'vector (cdr total-usec-words))))
	   (insert
	    (cabledolphin--pcapng-block
	     cabledolphin--pcapng-enhanced-packet-block-type
	     (bindat-pack cabledolphin--pcapng-enhanced-packet-bindat-spec
			  `((interface-id . 0)
			    (timestamp . ,timestamp)
			    (captured-packet-length . ,total-len)
			    (original-packet-length . ,total-len)
			    (packet-data . ,tcp-ip-packet)))
	     ;; epb_flags - inbound or outbound?
	     ;; The spec says that inbound/outbound is contained
	     ;; in the two most significant bits, but that's not
	     ;; how Wireshark interprets it.  Let's side with
	     ;; Wireshark for now.
	     2 (cl-ecase from-key
		 (:local
		  [#x00 #x00 #x00 #x02])
		 (:remote
		  [#x00 #x00 #x00 #x01])))))))

      ;; Insert our sequence counter.
      (process-put process seq-key (+ seq len))

      (let ((coding-system-for-write 'binary))
	(write-region (point-min) (point-max) cabledolphin-pcap-file t :silent)))))

(defun cabledolphin--tcp-ip-packet (from to seq data)
  (let ((ip-version
	 (if (= 9 (length from))
	     6
	   4))
	(len-with-tcp
	 (+ (length data) (bindat-length cabledolphin--tcp-bindat-spec ()))))
    (vconcat
     ;; Create a fake IP header.
     (cl-ecase ip-version
       (4
	(let ((total-len
	       (+ len-with-tcp (bindat-length cabledolphin--ipv4-bindat-spec ()))))
	  (bindat-pack cabledolphin--ipv4-bindat-spec
		       `((version-and-header-length . #x45)
			 (total-length . ,total-len)
			 (identification . 0)
			 (flags-and-fragment-offset . 0)
			 (ttl . 128)
			 ;; protocol 6 for TCP
			 (protocol . 6)
			 (header-checksum . 0)
			 (src-addr . ,(seq-take from 4))
			 (dest-addr . ,(seq-take to 4))))))
       (6
	(bindat-pack cabledolphin--ipv6-bindat-spec
		     `((version-etc . #x60)
		       (payload-length . ,len-with-tcp)
		       ;; protocol 6 for TCP
		       (next-header . 6)
		       (hop-limit . 128)
		       (src-addr . ,(seq-take from 8))
		       (dest-addr . ,(seq-take to 8))))))

     ;; Create a fake TCP header.
     (bindat-pack cabledolphin--tcp-bindat-spec
		  `((src-port . ,(elt from (1- (length from))))
		    (dest-port . ,(elt to (1- (length to))))
		    (seq . ,seq)
		    (ack . 0)
		    (data-offset-and-reserved . #x50)
		    ;; set SYN and PSH
		    (flags . (3 4))
		    (window-size . 16384)
		    (checksum . 0)
		    (urgent-pointer . 0)))

     ;; Finally insert the actual data.
     data)))

(provide 'cabledolphin)
;;; cabledolphin.el ends here
