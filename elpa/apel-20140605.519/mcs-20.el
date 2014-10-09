;;; mcs-20.el --- MIME charset implementation for Emacs 20 and XEmacs/mule

;; Copyright (C) 1997,1998,1999,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: emulation, compatibility, Mule

;; This file is part of APEL (A Portable Emacs Library).

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

;;    This module requires Emacs 20.0.93, XEmacs 20.3-b5 (with mule)
;;    or later.

;;; Code:

(require 'custom)
(eval-when-compile (require 'wid-edit))

(if (featurep 'xemacs)
    (require 'mcs-xm)
  (require 'mcs-e20))


;;; @ MIME charset
;;;

(defcustom mime-charset-coding-system-alist
  (let ((rest
	 '((us-ascii      . raw-text)
	   (gb2312	  . cn-gb-2312)
	   (cn-gb	  . cn-gb-2312)
	   (iso-2022-jp-2 . iso-2022-7bit-ss2)
	   (iso-2022-jp-3 . iso-2022-7bit-ss2)
	   (tis-620	  . tis620)
	   (windows-874	  . tis-620)
	   (cp874	  . tis-620)
	   (x-ctext       . ctext)
	   (unknown       . undecided)
	   (x-unknown     . undecided)
	   ))
	dest)
    (while rest
      (let ((pair (car rest)))
	(or (find-coding-system (car pair))
	    (setq dest (cons pair dest))
	    ))
      (setq rest (cdr rest))
      )
    dest)
  "Alist MIME CHARSET vs CODING-SYSTEM.
MIME CHARSET and CODING-SYSTEM must be symbol."
  :group 'i18n
  :type '(repeat (cons symbol coding-system)))

(defcustom mime-charset-to-coding-system-default-method
  nil
  "Function called when suitable coding-system is not found from MIME-charset.
It must be nil or function.
If it is a function, interface must be (CHARSET LBT CODING-SYSTEM)."
  :group 'i18n
  :type '(choice function (const nil)))

(defun mime-charset-to-coding-system (charset &optional lbt)
  "Return coding-system corresponding with CHARSET.
CHARSET is a symbol whose name is MIME charset.
If optional argument LBT (`CRLF', `LF', `CR', `unix', `dos' or `mac')
is specified, it is used as line break code type of coding-system."
  (if (stringp charset)
      (setq charset (intern (downcase charset)))
    )
  (let ((cs (cdr (assq charset mime-charset-coding-system-alist))))
    (unless (or (null cs) (find-coding-system cs))
      (message
       "Invalid coding system: %s.  Confirm mime-charset-coding-system-alist."
       cs)
      (setq cs nil))
    (unless cs (setq cs charset))
    (if lbt
	(setq cs (intern (format "%s-%s" cs
				 (cond ((eq lbt 'CRLF) 'dos)
				       ((eq lbt 'LF) 'unix)
				       ((eq lbt 'CR) 'mac)
				       (t lbt)))))
      )
    (or (find-coding-system cs)
	(if mime-charset-to-coding-system-default-method
	    (funcall mime-charset-to-coding-system-default-method
		     charset lbt cs)
	  ))))

(defalias 'mime-charset-p 'mime-charset-to-coding-system)

(defvar widget-mime-charset-prompt-value-history nil
  "History of input to `widget-mime-charset-prompt-value'.")

(define-widget 'mime-charset 'coding-system
  "A mime-charset."
  :format "%{%t%}: %v"
  :tag "MIME-charset"
  :prompt-history 'widget-mime-charset-prompt-value-history
  :prompt-value 'widget-mime-charset-prompt-value
  :action 'widget-mime-charset-action)

(defun widget-mime-charset-prompt-value (widget prompt value unbound)
  ;; Read mime-charset from minibuffer.
  (intern
   (completing-read (format "%s (default %s) " prompt value)
		    (mapcar (function
			     (lambda (sym)
			       (list (symbol-name sym))))
			    (mime-charset-list)))))

(defun widget-mime-charset-action (widget &optional event)
  ;; Read a mime-charset from the minibuffer.
  (let ((answer
	 (widget-mime-charset-prompt-value
	  widget
	  (widget-apply widget :menu-tag-get)
	  (widget-value widget)
	  t)))
    (widget-value-set widget answer)
    (widget-apply widget :notify widget event)
    (widget-setup)))

(defcustom default-mime-charset 'x-unknown
  "Default value of MIME-charset.
It is used when MIME-charset is not specified.
It must be symbol."
  :group 'i18n
  :type 'mime-charset)

(cond ((featurep 'utf-2000)
;; for CHISE Architecture
(defun mcs-region-repertoire-p (start end charsets &optional buffer)
  (save-excursion
    (if buffer
	(set-buffer buffer))
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (catch 'tag
	(let (ch)
	  (while (not (eobp))
	    (setq ch (char-after (point)))
	    (unless (some (lambda (ccs)
			    (encode-char ch ccs))
			  charsets)
	      (throw 'tag nil))
	    (forward-char)))
	t))))

(defun mcs-string-repertoire-p (string charsets &optional start end)
  (let ((i (if start
	       (if (< start 0)
		   (error 'args-out-of-range string start end)
		 start)
	     0))
	ch)
    (if end
	(if (> end (length string))
	    (error 'args-out-of-range string start end))
      (setq end (length string)))
    (catch 'tag
      (while (< i end)
	(setq ch (aref string i))
	(unless (some (lambda (ccs)
			(encode-char ch ccs))
		      charsets)
	  (throw 'tag nil))
	(setq i (1+ i)))
      t)))

(defun detect-mime-charset-region (start end)
  "Return MIME charset for region between START and END."
  (let ((rest charsets-mime-charset-alist)
	cell)
    (catch 'tag
      (while rest
	(setq cell (car rest))
	(if (mcs-region-repertoire-p start end (car cell))
	    (throw 'tag (cdr cell)))
	(setq rest (cdr rest)))
      default-mime-charset-for-write)))

(defun detect-mime-charset-string (string)
  "Return MIME charset for STRING."
  (let ((rest charsets-mime-charset-alist)
	cell)
    (catch 'tag
      (while rest
	(setq cell (car rest))
	(if (mcs-string-repertoire-p string (car cell))
	    (throw 'tag (cdr cell)))
	(setq rest (cdr rest)))
      default-mime-charset-for-write)))
)

((eval-when-compile (and (boundp 'mule-version)
			 (null (string< mule-version "6.0"))))
;; for Emacs 23 and later
(defcustom detect-mime-charset-from-coding-system nil
  "When non-nil, `detect-mime-charset-region' and `detect-mime-charset-string' functions decide charset by encodability in destination coding system.

In that case, each car of `charsets-mime-charset-alist' element is ignored."
  :group 'i18n
  :type 'boolean)

(defun detect-mime-charset-list (chars)
  "Return MIME charset for the list of characters CHARS."
  (catch 'found
    (mapc (lambda (cons)
	    (catch 'next
	      (mapc (lambda (ch) (unless (char-charset ch (car cons))
				   (throw 'next nil)))
		    chars)
	      (throw 'found (cdr cons))))
	  charsets-mime-charset-alist)
    default-mime-charset-for-write))

(defun detect-mime-charset-from-coding-system (start end &optional string)
  "Return MIME charset for the region between START and END, deciding by encodability in destination coding system.

Optional 3rd argument STRING is non-nil, detect MIME charset from STRING.  In that case, START and END are indexes of the string."
  (let ((alist charsets-mime-charset-alist)
	result)
    (while alist
      (if (unencodable-char-position
	   start end (mime-charset-to-coding-system (cdar alist)) nil string)
	  (setq alist (cdr alist))
	(setq result (cdar alist)
	      alist nil)))
    (or result default-mime-charset-for-write)))

(defun detect-mime-charset-string (string)
  "Return MIME charset for STRING.

When `detect-mime-charset-from-coding-system' is non-nil, each car of `charsets-mime-charset-alist' element is ignored."
  (if detect-mime-charset-from-coding-system
      (detect-mime-charset-from-coding-system 0 (length string) string)
    (let (list)
      (mapc (lambda (ch) (unless (memq ch list)
			   (setq list (cons ch list))))
	    string)
      (detect-mime-charset-list list))))

(defun detect-mime-charset-region (start end)
  "Return MIME charset for region between START and END.

When `detect-mime-charset-from-coding-system' is non-nil, each car of `charsets-mime-charset-alist' element is ignored."
  (if detect-mime-charset-from-coding-system
      (detect-mime-charset-from-coding-system start end)
    (let ((point (min start end))
	  list)
      (setq end (max start end))
      (while (< point end)
	(unless (memq (char-after point) list)
	  (setq list (cons (char-after point) list)))
	(setq point (1+ point)))
      (detect-mime-charset-list list)))))

(t
;; for legacy Mule
(defun detect-mime-charset-region (start end)
  "Return MIME charset for region between START and END."
  (find-mime-charset-by-charsets (find-charset-region start end)
				 'region start end))
))

(defun write-region-as-mime-charset (charset start end filename
					     &optional append visit lockname)
  "Like `write-region', q.v., but encode by MIME CHARSET."
  (let ((coding-system-for-write
	 (or (mime-charset-to-coding-system charset)
	     'binary)))
    (write-region start end filename append visit lockname)))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'mcs-20) (require 'apel-ver))

;;; mcs-20.el ends here
