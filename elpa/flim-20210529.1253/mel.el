;;; mel.el --- A MIME encoding/decoding library.  -*- lexical-binding: t -*-

;; Copyright (C) 1995,1996,1997,1998,1999,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Created: 1995/6/25
;; Keywords: MIME, Base64, Quoted-Printable, uuencode, gzip64

;; This file is part of FLIM (Faithful Library about Internet Message).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'mime-def)
(require 'alist)
(require 'pces)

(defcustom mime-encoding-list
  '("7bit" "8bit" "binary" "base64" "quoted-printable")
  "List of Content-Transfer-Encoding.  Each encoding must be string."
  :group 'mime
  :type '(repeat string))

(defun mime-encoding-list (&optional service)
  "Return list of Content-Transfer-Encoding.
If SERVICE is specified, it returns available list of
Content-Transfer-Encoding for it."
  (if service
      (let (dest)
	(mapatoms (lambda (sym)
		    (or (eq sym nil)
			(setq dest (cons (symbol-name sym) dest))))
		  (symbol-value (intern (format "%s-obarray" service))))
	(let ((rest mel-encoding-module-alist)
	      pair)
	  (while (setq pair (car rest))
	    (let ((key (car pair)))
	      (or (member key dest)
		  (<= (length key) 1)
		  (setq dest (cons key dest))))
	    (setq rest (cdr rest))))
	dest)
    mime-encoding-list))

(defun mime-encoding-alist (&optional service)
  "Return table of Content-Transfer-Encoding for completion."
  (mapcar #'list (mime-encoding-list service)))

(defsubst mel-use-module (name encodings)
  (while encodings
    (set-alist 'mel-encoding-module-alist
	       (car encodings)
	       (cons name (cdr (assoc (car encodings)
				      mel-encoding-module-alist))))
    (setq encodings (cdr encodings))))

(defsubst mel-find-function (service encoding)
  (mel-find-function-from-obarray
   (symbol-value (intern (format "%s-obarray" service))) encoding))

(defun mel-prompt-for-encoding (&optional service)
  (completing-read "Encoding: (default base64) "
		   (mime-encoding-alist service) nil t nil nil "base64"))

;;; @ setting for modules
;;;

(defun 8bit-insert-encoded-file (filename)
  "Insert file FILENAME encoded by \"7bit\" format."
  (let ((coding-system-for-read 'raw-text)
	format-alist)
    ;; Returns list of absolute file name and length of data inserted.
    (insert-file-contents filename)))

(defun 8bit-write-decoded-region (start end filename)
  "Decode and write current region encoded by \"8bit\" into FILENAME."
  (let ((coding-system-for-write 'no-conversion)
	format-alist)
    (write-region start end filename)))

(mel-define-backend "8bit")
(mel-define-method-function (mime-encode-string string (nil "8bit"))
			    'identity)
(mel-define-method-function (mime-decode-string string (nil "8bit"))
			    'identity)
(mel-define-method mime-encode-region (_start _end (nil "8bit")))
(mel-define-method mime-decode-region (_start _end (nil "8bit")))
(mel-define-method-function (mime-insert-encoded-file filename (nil "8bit"))
			    '8bit-insert-encoded-file)
(mel-define-method-function (mime-write-decoded-region
			     start end filename (nil "8bit"))
			    '8bit-write-decoded-region)


(defalias '7bit-insert-encoded-file '8bit-insert-encoded-file)
(defalias '7bit-write-decoded-region '8bit-write-decoded-region)

(mel-define-backend "7bit" ("8bit"))


(defun binary-write-decoded-region (start end filename)
  "Decode and write current region encoded by \"binary\" into FILENAME."
  (defvar jam-zcat-filename-list)
  (let ((coding-system-for-write 'binary)
	jka-compr-compression-info-list jam-zcat-filename-list)
    (write-region start end filename)))

(defalias 'binary-insert-encoded-file 'insert-file-contents-literally)

(defun binary-find-file-noselect (filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but don't code and format conversion."
  (let ((coding-system-for-read 'binary)
	format-alist)
    (find-file-noselect filename nowarn rawfile)))

(defun binary-funcall (name &rest args)
  "Like `funcall', q.v., but read and write as binary."
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary))
    (apply name args)))

(defun binary-to-text-funcall (coding-system name &rest args)
  "Like `funcall', q.v., but write as binary and read as text.
Read text is decoded as CODING-SYSTEM."
  (let ((coding-system-for-read coding-system)
	(coding-system-for-write 'binary))
    (apply name args)))

(mel-define-backend "binary")
(mel-define-method-function (mime-encode-string string (nil "binary"))
			    'identity)
(mel-define-method-function (mime-decode-string string (nil "binary"))
			    'identity)
(mel-define-method mime-encode-region (_start _end (nil "binary")))
(mel-define-method mime-decode-region (_start _end (nil "binary")))
(mel-define-method-function (mime-insert-encoded-file filename (nil "binary"))
			    'binary-insert-encoded-file)
(mel-define-method-function (mime-write-decoded-region
			     start end filename (nil "binary"))
			    'binary-write-decoded-region)

(defvar mel-b-builtin t)

(defcustom mel-b-builtin-garbage-strategy 'asis
"When non-nil, base64 decoder functions handle non-encoded
garbage.  When value is asis decoders keep garbage and when value
is discard decoders delete garbage."
  :group 'mime
  :type '(choice (const :tag "Keep as is" asis)
		 (const :tag "Discard" discard)
		 (const :tag "Not handled" nil)))

(defvar mel-b-builtin-encoded-line-regexp "^[A-Za-z0-9+/]+=*[\t ]*\r?\n?")

(mel-define-backend "base64")
(mel-define-method-function (mime-encode-string string (nil "base64"))
			      'base64-encode-string)
(defun mel-b-builtin-decode-string (string)
  "Decode base64 encoded STRING with garbage handling.  Garbage handling strategy is decided by `mel-b-builtin-garbage-strategy'.  Return decoded string."
  (if (null mel-b-builtin-garbage-strategy)
      (base64-decode-string string)
    (condition-case error
	(base64-decode-string string)
      (error
       (if (string-match mel-b-builtin-encoded-line-regexp string)
	   (let ((start (match-beginning 0))
		 end)
	     (message "Base64 encoded string has garbage")
	     (while (and (< (setq end (match-end 0)) (length string))
			 (eq end
			     (and (string-match
				   mel-b-builtin-encoded-line-regexp
				   string end)
				  (match-beginning 0)))))
	     (if (eq mel-b-builtin-garbage-strategy 'discard)
		 (base64-decode-string (substring string start end))
	       (concat (substring string 0 start)
		       (base64-decode-string (substring string start end))
		       (substring string end))))
	 (signal (car error) (cdr error)))))))
(mel-define-method-function (mime-decode-string string (nil "base64"))
			    'mel-b-builtin-decode-string)
(mel-define-method-function (mime-encode-region start end (nil "base64"))
			    'base64-encode-region)
(defun mel-b-builtin-decode-region (start end)
  "Decode base64 encoded region between START and END with garbage handling.  Garbage handling strategy is decided by `mel-b-builtin-garbage-strategy'."
  (if (null mel-b-builtin-garbage-strategy)
      (base64-decode-region start end)
    (condition-case error
	(base64-decode-region start end)
      (error
       (save-excursion
	 (let ((start (min start end))
	       (end (max start end))
	       base64-start)
	   (goto-char start)
	   (if (re-search-forward mel-b-builtin-encoded-line-regexp end t)
	       (progn
		 (message "Base64 encoded region contains garbage")
		 (setq base64-start (match-beginning 0))
		 (while (eq (point)
			    (and (re-search-forward
				  mel-b-builtin-encoded-line-regexp end t)
				 (match-beginning 0))))
		 (when (eq mel-b-builtin-garbage-strategy 'discard)
		   (delete-region (match-end 0) end))
		 (base64-decode-region base64-start (point))
		 (when (eq mel-b-builtin-garbage-strategy 'discard)
		   (delete-region start base64-start)))
	     (signal (car error) (cdr error)))))))))
(mel-define-method-function (mime-decode-region start end (nil "base64"))
			    'mel-b-builtin-decode-region)  
(mel-define-method mime-insert-encoded-file (filename (nil "base64"))
  "Encode contents of file FILENAME to base64, and insert the result."
  (interactive "*fInsert encoded file: ")
  ;; No need to make buffer unibyte if binary-insert-encoded-file only
  ;; inserts single-byte characters.
  (save-restriction
    (narrow-to-region (point) (point))
    (binary-insert-encoded-file filename)
    (base64-encode-region (point-min) (point-max))
    (goto-char (point-max)))
  (or (bolp) (insert ?\n)))
(mel-define-method mime-write-decoded-region (start end filename
						    (nil "base64"))
  "Decode the region from START to END and write out to FILENAME."
  (interactive "*r\nFWrite decoded region to file: ")
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buffer start end)
      (mel-b-builtin-decode-region (point-min) (point-max))
      (write-region-as-binary (point-min) (point-max) filename))))

;; (mel-define-method-function (encoded-text-encode-string string (nil "B"))
;;                             'base64-encode-string)
(mel-define-method encoded-text-decode-string (string (nil "B"))
  (if (string-match (eval-when-compile
		      (concat "\\`" B-encoded-text-regexp "\\'"))
		    string)
      (base64-decode-string string)
    (error "Invalid encoded-text %s" string)))


(mel-use-module 'mel-q '("quoted-printable" "Q"))
(mel-use-module 'mel-g '("x-gzip64"))
(mel-use-module 'mel-u '("x-uue" "x-uuencode"))

(declare-function module-installed-p "path-util"
		  (module &optional paths))

(defvar mel-q-ccl-module
  (progn
    (require 'path-util)
    (module-installed-p 'mel-q-ccl)))

(when mel-q-ccl-module
  (mel-use-module 'mel-q-ccl '("quoted-printable" "Q")))


;;; @ region
;;;

;;;###autoload
(defun mime-encode-region (start end encoding)
  "Encode region START to END of current buffer using ENCODING.
ENCODING must be string."
  (interactive
   (list (region-beginning)(region-end)
	 (mel-prompt-for-encoding)))
  (funcall (mel-find-function 'mime-encode-region encoding) start end))


;;;###autoload
(defun mime-decode-region (start end encoding)
  "Decode region START to END of current buffer using ENCODING.
ENCODING must be string."
  (interactive
   (list (region-beginning)(region-end)
	 (mel-prompt-for-encoding 'mime-decode-region)))
  (funcall (mel-find-function 'mime-decode-region encoding)
	   start end))


;;; @ string
;;;

;;;###autoload
(defun mime-decode-string (string encoding)
  "Decode STRING using ENCODING.
ENCODING must be string.  If ENCODING is found in
`mime-string-decoding-method-alist' as its key, this function decodes
the STRING by its value."
  (let ((f (mel-find-function 'mime-decode-string encoding)))
    (if f
	(funcall f string)
      string)))


(mel-define-service encoded-text-encode-string)
(defun encoded-text-encode-string (string encoding &optional mode)
  "Encode STRING as encoded-text using ENCODING.
ENCODING must be string.
Optional argument MODE allows `text', `comment', `phrase' or nil.
Default value is `phrase'."
  (if (string= encoding "B")
      (base64-encode-string string 'no-line-break)
    (let ((f (mel-find-function 'encoded-text-encode-string encoding)))
      (if f
	  (funcall f string mode)
	string))))

(mel-define-service encoded-text-decode-string (string encoding)
  "Decode STRING as encoded-text using ENCODING.  ENCODING must be string.")

(defun base64-encoded-length (string)
  (* (/ (+ (length string) 2) 3) 4))

(defsubst Q-encoding-printable-char-p (chr mode)
  (and (not (memq chr '(?= ?? ?_)))
       (<= ?\  chr)(<= chr ?~)
       (cond ((eq mode 'text) t)
	     ((eq mode 'comment)
	      (not (memq chr '(?\( ?\) ?\\))))
	     (t
	      (string-match "[A-Za-z0-9!*+/=_---]" (char-to-string chr))))))

(defun Q-encoded-text-length (string &optional mode)
  (let ((l 0)(i 0)(len (length string)) chr)
    (while (< i len)
      (setq chr (aref string i))
      (if (or (Q-encoding-printable-char-p chr mode)
	      (eq chr ?\s))
	  (setq l (+ l 1))
	(setq l (+ l 3)))
      (setq i (+ i 1)))
    l))


;;; @ file
;;;

;;;###autoload
(defun mime-insert-encoded-file (filename encoding)
  "Insert file FILENAME encoded by ENCODING format."
  (interactive
   (list (read-file-name "Insert encoded file: ")
	 (mel-prompt-for-encoding)))
  (funcall (mel-find-function 'mime-insert-encoded-file encoding)
	   filename))


;;;###autoload
(defun mime-write-decoded-region (start end filename encoding)
  "Decode and write current region encoded by ENCODING into FILENAME.
START and END are buffer positions."
  (interactive
   (list (region-beginning)(region-end)
	 (read-file-name "Write decoded region to file: ")
	 (mel-prompt-for-encoding 'mime-write-decoded-region)))
  (funcall (mel-find-function 'mime-write-decoded-region encoding)
	   start end filename))


;;; @ end
;;;

(provide 'mel)

;;; mel.el ends here.
