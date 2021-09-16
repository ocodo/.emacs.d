;;; mmgeneric.el --- MIME generic entity module  -*- lexical-binding: t -*-

;; Copyright (C) 1995,96,97,98,99,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: definition, MIME, multimedia, mail, news

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'mcharset)
(require 'std11)
(require 'luna)
(require 'eword-decode)


;;; @ MIME entity
;;;

(autoload 'mime-entity-content-type "mime")
(autoload 'mime-parse-multipart "mime-parse")
(autoload 'mime-parse-message "mime-parse")
;; (autoload 'mime-parse-encapsulated "mime-parse")
;; (autoload 'mime-parse-external "mime-parse")
(autoload 'mime-entity-content "mime")

(eval-and-compile
  (luna-define-class mime-entity ()
		     (location
		      content-type children parent
		      node-id
		      content-disposition encoding
		      ;; for other fields
		      original-header parsed-header))

  (luna-define-internal-accessors 'mime-entity))

(defalias 'mime-entity-representation-type-internal 'luna-class-name)
(defalias 'mime-entity-set-representation-type-internal 'luna-set-class-name)

(luna-define-method mime-entity-fetch-field ((entity mime-entity)
					     field-name)
  (or (symbolp field-name)
      (setq field-name (intern (capitalize field-name))))
  (cdr (assq field-name
	     (mime-entity-original-header-internal entity))))

(luna-define-method mime-insert-text-content ((entity mime-entity))
  (insert
   (decode-mime-charset-string (mime-entity-content entity)
			       (or (mime-content-type-parameter
				    (mime-entity-content-type entity)
				    "charset")
				   default-mime-charset)
			       'CRLF)))


;;; @ for mm-backend
;;;

(defmacro mm-expand-class-name (type)
  `(intern (format "mime-%s-entity" ,type)))

(defmacro mm-define-backend (type &optional parents)
  `(luna-define-class ,(mm-expand-class-name type)
		      ,(nconc (mapcar (lambda (parent)
					(mm-expand-class-name parent))
				      parents)
			      '(mime-entity))))

(defmacro mm-define-method (name args &rest body)
  (or (eq name 'initialize-instance)
      (setq name (intern (format "mime-%s" name))))
  (let ((spec (car args)))
    (setq args
	  (cons (list (car spec)
		      (mm-expand-class-name (nth 1 spec)))
		(cdr args)))
    `(luna-define-method ,name ,args ,@body)))

(put 'mm-define-method 'lisp-indent-function 'defun)

(def-edebug-spec mm-define-method
  (&define name ((arg symbolp)
		 [&rest arg]
		 [&optional ["&optional" arg &rest arg]]
		 &optional ["&rest" arg])
	   def-body))


;;; @ header filter
;;;

;; [tomo] We should think about specification of better filtering
;; mechanism.  Please discuss in the emacs-mime mailing lists.

(defun mime-visible-field-p (field-name visible-fields invisible-fields)
  (let ((case-fold-search t))
    (catch 'found
      (while visible-fields
	(when (string-match (car visible-fields) field-name)
	  (throw 'found t))
	(setq visible-fields (cdr visible-fields)))
      (while invisible-fields
	(when (string-match (car invisible-fields) field-name)
	  (throw 'found nil))
	(setq invisible-fields (cdr invisible-fields)))
      t)))

(defun mime-insert-header-from-buffer (buffer start end
					      &optional invisible-fields
					      visible-fields)
  (let ((mode-obj (mime-find-field-presentation-method 'wide))
	field-decoder f-b p field-name field-body result)
    (with-current-buffer buffer
      (goto-char start)
      (while (re-search-forward std11-field-head-regexp end t)
	(setq f-b (match-beginning 0)
	      p (match-end 0)
	      field-name (buffer-substring f-b p))
	(when (mime-visible-field-p field-name
				    visible-fields invisible-fields)
	  (setq	field-body (buffer-substring p (std11-field-end end))
		field-decoder
		(mime-find-field-decoder-internal
		 (intern (capitalize
			  (buffer-substring-no-properties f-b (1- p))))
		 mode-obj)
		result
		(cons "\n"
		      (cons (if field-decoder
				(funcall field-decoder
					 field-body
					 (string-width field-name))
			      ;; Don't decode
			      field-body)
			    (cons field-name result)))))))
    (when result
      (apply #'insert (nreverse result)))))


;;; @ end
;;;

(provide 'mmgeneric)

;;; mmgeneric.el ends here
