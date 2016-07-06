;;; mime-pgp.el --- mime-view internal methods for EasyPG.

;; Copyright (C) 1995,1996,1997,1998,1999,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;;	Daiki Ueno <ueno@unixuser.org>
;;	Kazuhiro Ito <kzhr@d1.dion.ne.jp>
;; Created: 1995/12/7
;;	Renamed: 1997/2/27 from tm-pgp.el
;;	Renamed: 2010/11/27 from mime-pgp.el in emiko-epg
;;	Renamed: 2012/10/05 from mime-epg.el
;; Keywords: PGP, security, MIME, multimedia, mail, news

;; This file is part of SEMI (Secure Emacs MIME Interface).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'mime-parse)
(require 'mime-play)
(require 'epg)
(require 'epa)

(eval-when-compile (require 'mmgeneric))

;;; @ Internal functions

(defun mime-pgp-decrypt-string (context cipher)
  (let ((string (epg-decrypt-string context cipher)))
    ;; Remove CRs if header contains CR.
    (if (string-match "\\`.*\r\n" string)
	(let (inhibit-eol-conversion)
	  (decode-coding-string string 'raw-text-dos))
      string)))

;;; @ Internal method for multipart/signed

(defun mime-verify-multipart/signed (entity situation)
  "Internal method to verify multipart/signed."
  (mime-play-entity
   (nth 1 (mime-entity-children entity)) ; entity-info of signature
   (list (assq 'mode situation)) ; play-mode
   ))

;;; @ Internal method for multipart/encrypted

(defun mime-display-multipart/pgp-encrypted (entity situation)
  (let ((original-major-mode-cell (assq 'major-mode situation))
	(default-situation
	  (cdr (assq 'childrens-situation situation)))
	child-situation)
    (unless mime-pgp-decrypt-when-preview
      (insert "This part is encrypted.\n"))
    (when original-major-mode-cell
      (setq default-situation
	    (cons original-major-mode-cell default-situation)))
    (mapc
     (lambda (child)
       (setq child-situation
	     (mime-find-entity-preview-situation child default-situation))
       (if (and (eq (cdr (assq 'type child-situation)) 'application)
		(eq (cdr (assq 'subtype child-situation)) 'pgp-encrypted))
	   (mime-display-entity
	    child (put-alist 'body mime-pgp-decrypt-when-preview
			     (copy-alist child-situation)))
	 (when mime-view-multipart/related-show-all-children
	   (mime-display-entity
	    child (put-alist 'body 'invisible (copy-alist child-situation))))))
     (mime-entity-children entity))))

;;; @ Internal method for application/*-signature

(defun mime-verify-application/*-signature-internal (entity situation)
  (let* ((mother (mime-entity-parent entity))
	 (orig-entity (car (mime-entity-children mother)))
	 (protocol (cdr (assoc "protocol" (mime-entity-parameters mother))))
	 context)
    (if (null protocol)
	"No protocol is specified."
      (setq context
	    (epg-make-context
	     (cond 
	      ((equal protocol "application/pgp-signature")
	       'OpenPGP)
	      ((string-match
		"\\`application/\\(x-\\)?pkcs7-signature\\'" protocol)
	       'CMS))))
      (if (null context)
	  (format "Unknown protocol: %s." protocol)
	(epg-verify-string context
			   (mime-entity-content entity)
			   (with-temp-buffer
			     (if (fboundp 'set-buffer-multibyte)
				 (set-buffer-multibyte nil))
			     (mime-insert-entity orig-entity)
			     (let (inhibit-eol-conversion)
			       (encode-coding-string (buffer-string)
						     'raw-text-dos))))
	(epg-context-result-for context 'verify)))))

(defun mime-verify-application/*-signature (entity situation)
  (let ((verify-result
	 (mime-verify-application/*-signature-internal entity situation))
	window)
    (cond
     ((stringp verify-result)
      (mime-show-echo-buffer verify-result))
     ((> (length verify-result) 1)
      (mime-show-echo-buffer (epg-verify-result-to-string verify-result)))
     (verify-result
      (epa-display-info (epg-verify-result-to-string verify-result))
	(when (and epa-popup-info-window
		   (setq window (get-buffer-window epa-info-buffer)))
	  (select-window window))))))

(defun mime-preview-application/*-signature (entity situation)
  (let ((verify-result
	 (condition-case error
	     (mime-verify-application/*-signature-internal entity situation)
	   (error (format "Verification failed, %s" error))))
	string)
    (if (stringp verify-result)
	(insert verify-result)
      (setq string (epg-verify-result-to-string verify-result))
      (when (> (length string) 0)
	(unless (string-equal (substring string -1) "\n")
	  (setq string (concat string "\n")))
	(insert string)))))

;;; @ Internal method for application/pgp-encrypted

(defun mime-decrypt-application/pgp-encrypted (entity situation)
  (let* ((mother (mime-entity-parent entity))
	 (encrypted-entity (nth 1 (mime-entity-children mother)))
	 (p-win (or (get-buffer-window (current-buffer))
		    (get-largest-window)))
	 (new-name
	  (format "%s-%s" (buffer-name) (mime-entity-number entity)))
	 (mother (current-buffer))
	 (preview-buffer (concat "*Preview-" (buffer-name) "*"))
	 representation-type message-buf context plain verify-result)
    (set-buffer (setq message-buf (get-buffer-create new-name)))
    (erase-buffer)
    (mime-insert-entity encrypted-entity)
    (goto-char (point-min))
    (setq context (epg-make-context)
	  plain (decode-coding-string
		 (mime-pgp-decrypt-string
		  context
		  (buffer-substring (point-min)(point-max)))
		 'raw-text))
    (delete-region (point-min)(point-max))
    (insert plain)
    (setq representation-type 'binary
	  major-mode 'mime-show-message-mode)
    (save-window-excursion
      (mime-view-buffer nil preview-buffer mother
			nil representation-type)
      (make-local-variable 'mime-view-temp-message-buffer)
      (setq mime-view-temp-message-buffer message-buf))
    (set-window-buffer p-win preview-buffer)))

(defvar mime-pgp-decrypted-buffers nil)

(defun mime-pgp-kill-decrypted-buffers ()
  (mapc (lambda (buffer)
	  (when (bufferp buffer)
	    (kill-buffer buffer)))
	mime-pgp-decrypted-buffers))

;; Imported and modified from Wanderlust.
(defun mime-preview-application/pgp-encrypted (entity situation)
  (let ((p (point-max))
	beg end buffer decrypted-entity failed)
    (goto-char p)
    (save-restriction
      (narrow-to-region p p)
      (setq buffer (generate-new-buffer (concat mime-temp-buffer-name "PGP*")))
      (with-current-buffer buffer
	(mime-insert-entity
	 (nth 1 (mime-entity-children (mime-entity-parent entity))))
	(setq beg (point-min)
	      end (point-max))
	(condition-case error
	    (insert (prog1
			(decode-coding-string
			 (mime-pgp-decrypt-string
			  (epg-make-context) (buffer-substring beg end))
			 'raw-text)
		      (delete-region beg end)))
	  (error (setq failed error)))
	(unless failed
	  (setq decrypted-entity
		(mime-parse-message
		 (mm-expand-class-name 'buffer)
		 nil entity (mime-entity-node-id entity))
		buffer-read-only t)))
      (if failed
	  (progn
	    (insert (format "%s" (cdr failed)))
	    (kill-buffer buffer))
	(add-hook 'kill-buffer-hook 'mime-pgp-kill-decrypted-buffers nil t)
	(make-local-variable 'mime-pgp-decrypted-buffers)
	(add-to-list 'mime-pgp-decrypted-buffers buffer)
	(mime-display-entity
	 decrypted-entity nil '((header . visible)
				(body . visible)
				(entity-button . invisible)))))))


;;; @ Internal method for application/pgp-keys

(defun mime-add-application/pgp-keys (entity situation)
  (when (y-or-n-p "Do you want to import PGP keys? ")
    (let ((context (epg-make-context))
	  result window)
      (epg-import-keys-from-string context (mime-entity-content entity))
      (when (setq result (epg-context-result-for context 'import))
	(epa-display-info (epg-import-result-to-string result))
	(when (and epa-popup-info-window
		   (setq window (get-buffer-window epa-info-buffer)))
	  (select-window window))))))


;;; @ Internal method for application/pkcs7-mime

(defun mime-view-application/pkcs7-mime (entity situation)
  (let* ((p-win (or (get-buffer-window (current-buffer))
		    (get-largest-window)))
	 (new-name
	  (format "%s-%s" (buffer-name) (mime-entity-number entity)))
	 (mother (current-buffer))
	 (preview-buffer (concat "*Preview-" (buffer-name) "*"))
	 (context (epg-make-context 'CMS))
	 message-buf)
    (when (memq (or (cdr (assq 'smime-type situation)) 'enveloped-data)
		'(enveloped-data signed-data))
      (set-buffer (setq message-buf (get-buffer-create new-name)))
      (let ((inhibit-read-only t)
	    buffer-read-only)
	(erase-buffer)
	(insert (mime-pgp-decrypt-string context (mime-entity-content entity))))
      (setq major-mode 'mime-show-message-mode)
      (save-window-excursion
	(mime-view-buffer nil preview-buffer mother
			  nil 'binary)
	(make-local-variable 'mime-view-temp-message-buffer)
	(setq mime-view-temp-message-buffer message-buf))
      (set-window-buffer p-win preview-buffer))))

(defun mime-preview-application/pkcs7-mime (entity situation)
  (when (memq (or (cdr (assq 'smime-type situation)) 'enveloped-data)
	      '(enveloped-data signed-data))
    (let ((p (point-max))
	  beg end buffer decrypted-entity failed)
      (goto-char p)
      (save-restriction
	(narrow-to-region p p)
	(setq buffer (generate-new-buffer
		      (concat mime-temp-buffer-name "PKCS7*")))
	(with-current-buffer buffer
	  (insert (mime-entity-content entity))
	  ;; (mime-insert-entity entity)
	  (setq beg (point-min)
		end (point-max))
	  (condition-case error
	      (insert (prog1
			  (decode-coding-string
			   (mime-pgp-decrypt-string
			    (epg-make-context 'CMS) (buffer-substring beg end))
			   'raw-text)
			(delete-region beg end)))
	    (error (setq failed error)))
	  (unless failed
	    (setq decrypted-entity
		  (mime-parse-message
		   (mm-expand-class-name 'buffer)
		   nil entity (mime-entity-node-id-internal entity))
		  buffer-read-only t)))
	(if failed
	    (progn
	      (insert (format "%s" (cdr failed)))
	      (kill-buffer buffer))
	  (add-hook 'kill-buffer-hook 'mime-pgp-kill-decrypted-buffers nil t)
	  (make-local-variable 'mime-pgp-decrypted-buffers)
	  (add-to-list 'mime-pgp-decrypted-buffers buffer)
	  (mime-display-entity
	   decrypted-entity nil '((header . visible)
				  (body . visible)
				  (entity-button . invisible))))))))


;;; @ end
;;;

(provide 'mime-pgp)

(run-hooks 'mime-pgp-load-hook)

;;; mime-pgp.el ends here
