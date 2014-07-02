;;; semi-setup.el --- setup file for MIME-View.

;; Copyright (C) 1994,95,96,97,98,99,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: mail, news, MIME, multimedia, multilingual, encoded-word

;; This file is part of SEMI (Setting for Emacs MIME Interfaces).

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

;;; Code:

(require 'semi-def)
(require 'path-util)

(defun call-after-loaded (module func &optional hook-name)
  "If MODULE is provided, then FUNC is called.
Otherwise func is set to MODULE-load-hook.
If optional argument HOOK-NAME is specified,
it is used as hook to set."
  (if (featurep module)
      (funcall func)
    (or hook-name
	(setq hook-name (intern (concat (symbol-name module) "-load-hook")))
	)
    (add-hook hook-name func)
    ))


;; for image/*
(defvar mime-setup-enable-inline-image
  (and window-system
       (or (featurep 'xemacs)(featurep 'mule)))
  "*If it is non-nil, semi-setup sets up to use mime-image.")

(if mime-setup-enable-inline-image
    (eval-after-load "mime-view"
      '(require 'mime-image)))

;; for text/html
(defvar mime-html-previewer-alist
  (delq nil `((w3m mime-w3m-preview-text/html "mime-w3m")
	      ,(and (fboundp 'libxml-parse-html-region)
		    '(shr mime-shr-preview-text/html "mime-shr"))
	      (w3 mime-preview-text/html "mime-w3")))
"*Alist for text/html part previewer.
Each element is a list consists of required module, previewer function and autoload file for previewer function.")

(defvar mime-setup-enable-inline-html
  (let ((alist mime-html-previewer-alist))
    (while (and alist (null (module-installed-p (caar alist))))
      (setq alist (cdr alist)))
    (caar alist))
  "*If it is a symbol, semi-setup sets up to use html previewer according to `mime-html-previewer-alist'.
If it is other non-nil value, semi-setup tries to set up for mime-w3.")

(when mime-setup-enable-inline-html
  (let ((table (cdr (or (assq mime-setup-enable-inline-html
			      mime-html-previewer-alist)
			(assq 'w3 mime-html-previewer-alist)))))
    (when table
      (eval-after-load "mime-view"
	`(progn
	   (mime-add-condition
	    'preview '((type . text)(subtype . html)
		       (body . visible)
		       (body-presentation-method . ,(car table)))
	    'strict ,(cadr table))

	   (set-alist 'mime-view-type-subtype-score-alist
		      '(text . html) 3)
	   )))))

;; for text/x-vcard
(defvar mime-setup-enable-vcard
  (module-installed-p 'vcard)
  "*If it is non-nil, semi-setup sets up to use mime-vcard.")

(eval-after-load "mime-view"
  '(when mime-setup-enable-vcard
     (mime-add-condition
      'preview 
      '((type . text)(subtype . x-vcard)
	(body . visible)
	(body-presentation-method . mime-display-text/x-vcard))
      'strict "mime-vcard")

     (set-alist 'mime-view-type-subtype-score-alist
		'(text . x-vcard) 3)
     ))

;; for PGP
(defvar mime-setup-enable-epg (module-installed-p 'epg)
  "*If it is non-nil, semi-setup sets up to use mime-pgp.")

(eval-after-load "mime-view"
  '(when mime-setup-enable-epg
     (mime-add-condition
      'preview '((type . application)(subtype . pgp)
		 (message-button . visible)))
	 
     (mime-add-condition
      'action '((type . multipart)(subtype . signed)
		(method . mime-verify-multipart/signed))
      'strict "mime-pgp")
	 
     (mime-add-condition
      'action
      '((type . application)(subtype . pgp-signature)
	(method . mime-verify-application/*-signature))
      'strict "mime-pgp")
	 
     (mime-add-condition
      'action
      '((type . application)(subtype . pgp-encrypted)
	(method . mime-decrypt-application/pgp-encrypted))
      'strict "mime-pgp")
	 
     (mime-add-condition
      'action
      '((type . application)(subtype . pgp-keys)
	(method . mime-add-application/pgp-keys))
      'strict "mime-pgp")

     (mime-add-condition
      'action
      '((type . application)(subtype . pkcs7-signature)
	(method . mime-verify-application/*-signature))
      'strict "mime-pgp")

     (mime-add-condition
      'action
      '((type . application)(subtype . x-pkcs7-signature)
	(method . mime-verify-application/*-signature))
      'strict "mime-pgp")
	 
     (mime-add-condition
      'action
      '((type . application)(subtype . pkcs7-mime)
	(method . mime-view-application/pkcs7-mime))
      'strict "mime-pgp")

     (mime-add-condition
      'action
      '((type . application)(subtype . x-pkcs7-mime)
	(method . mime-view-application/pkcs7-mime))
      'strict "mime-pgp")

     (mime-add-condition
      'preview
      '((type . application)
	(subtype . pgp-signature)
	(body . visible)
	(body-presentation-method . mime-preview-application/*-signature))
      'strict "mime-pgp")

     (mime-add-condition
      'preview
      '((type . multipart)
	(subtype . encrypted)
	("protocol" . "application/pgp-encrypted")
	(body . visible)
	(body-presentation-method . mime-display-multipart/pgp-encrypted))
      'strict "mime-pgp")

     (mime-add-condition
      'preview
      '((type . application)
	(subtype . pgp-encrypted)
	(body . visible)
	(body-presentation-method . mime-preview-application/pgp-encrypted))
      'strict "mime-pgp")
     )
  )


;;; @ for mime-edit
;;;

;; (defun mime-setup-decode-message-header ()
;;   (save-excursion
;;     (save-restriction
;;       (goto-char (point-min))
;;       (narrow-to-region
;;        (point-min)
;;        (if (re-search-forward
;;             (concat "^" (regexp-quote mail-header-separator) "$")
;;             nil t)
;;            (match-beginning 0)
;;          (point-max)
;;          ))
;;       (mime-decode-header-in-buffer)
;;       (set-buffer-modified-p nil)
;;       )))

;; (add-hook 'mime-edit-mode-hook 'mime-setup-decode-message-header)


;;; @@ variables
;;;

(defvar mime-setup-use-signature nil
  "If it is not nil, mime-setup sets up to use signature.el.")

(defvar mime-setup-default-signature-key "\C-c\C-s"
  "*Key to insert signature.")

(defvar mime-setup-signature-key-alist '((mail-mode . "\C-c\C-w"))
  "Alist of major-mode vs. key to insert signature.")


;;; @@ for signature
;;;

(defun mime-setup-set-signature-key ()
  (let ((keymap (current-local-map)))
    (if keymap
	(let ((key
	       (or (cdr (assq major-mode mime-setup-signature-key-alist))
		   mime-setup-default-signature-key)))
	  (define-key keymap key (function insert-signature))
	  ))))

(when mime-setup-use-signature
  (autoload 'insert-signature "signature" "Insert signature" t)
  (add-hook 'mime-edit-mode-hook 'mime-setup-set-signature-key)
  ;; (setq message-signature nil)
  )


;;; @ for mu-cite
;;;

;; (add-hook 'mu-cite/pre-cite-hook 'eword-decode-header)


;;; @ for Mac OS X
;;;

(when (eq system-type 'darwin)
  (eval-after-load "mime-view"
    '(progn
       (mime-add-condition
	'action
	'((type . application)
	  (method . mime-mac-save-and-play-with-open))
	'with-default
	"mime-mac")
       )))


;;; @ end
;;;

(provide 'semi-setup)

;;; semi-setup.el ends here
