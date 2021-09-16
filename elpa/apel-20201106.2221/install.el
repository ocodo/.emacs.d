;;; install.el --- Emacs Lisp package install utility  -*- lexical-binding: t -*-

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2006
;; 	Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Created: 1996/08/18
;; Keywords: install, byte-compile, directory detection

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'path-util)			; default-load-path


;;; @ compile Emacs Lisp files
;;;

(defun compile-elisp-module (module &optional path every-time)
  (setq module (expand-file-name (symbol-name module) path))
  (let ((el-file (concat module ".el"))
	(elc-file (concat module ".elc")))
    (if (or every-time
	    (file-newer-than-file-p el-file elc-file))
	(byte-compile-file el-file))))

(defun compile-elisp-modules (modules &optional path every-time)
  (mapcar
   (lambda (module)
     (compile-elisp-module module path every-time))
   modules))


;;; @ install files
;;;

(defvar install-overwritten-file-modes (+ (* 64 6)(* 8 4) 4)) ; 0644

(defun install-file (file src dest &optional move overwrite just-print)
  (if just-print
      (princ (format "%s -> %s\n" file dest))
    (let ((src-file (expand-file-name file src)))
      (if (file-exists-p src-file)
	  (let ((full-path (expand-file-name file dest)))
	    (if (and (file-exists-p full-path) overwrite)
		(delete-file full-path))
	    (copy-file src-file full-path t t)
	    (set-file-modes full-path install-overwritten-file-modes)
	    (if move
		(catch 'tag
		  (while (and (file-exists-p src-file)
			      (file-writable-p src-file))
		    (condition-case err
			(progn
			  (delete-file src-file)
			  (throw 'tag nil))
		      (error (princ (format "%s\n" (nth 1 err))))))))
	    (princ (format "%s -> %s\n" file dest)))))))

(defun install-files (files src dest &optional move overwrite just-print)
  (or just-print
      (file-exists-p dest)
      (make-directory dest t))
  (mapcar
   (lambda (file)
     (install-file file src dest move overwrite just-print))
   files))


;;; @@ install Emacs Lisp files
;;;

(defun install-elisp-module (module src dest &optional just-print del-elc)
  (let (el-file elc-file)
    (let ((name (symbol-name module)))
      (setq el-file (concat name ".el"))
      (setq elc-file (concat name ".elc")))
    (let ((src-file (expand-file-name el-file src)))
      (if (not (file-exists-p src-file))
	  nil 
	(if just-print
	    (princ (format "%s -> %s\n" el-file dest))
	  (let ((full-path (expand-file-name el-file dest)))
	    (if (file-exists-p full-path)
		(delete-file full-path))
	    (copy-file src-file full-path t t)
	    (set-file-modes full-path install-overwritten-file-modes)
	    (princ (format "%s -> %s\n" el-file dest)))))
      (setq src-file (expand-file-name elc-file src))
      (if (not (file-exists-p src-file))
	  (let ((full-path (expand-file-name elc-file dest)))
	    (if (and del-elc (file-exists-p full-path))
		(if just-print
		    (princ (format "%s -> to be deleted\n" full-path))
		  (delete-file full-path)
		  (princ (format "%s -> deleted\n" full-path)))))
	(if just-print
	    (princ (format "%s -> %s\n" elc-file dest))
	  (let ((full-path (expand-file-name elc-file dest)))
            (if (file-exists-p full-path)
                (delete-file full-path))
	    (copy-file src-file full-path t t)
	    (set-file-modes full-path install-overwritten-file-modes)
	    (catch 'tag
	      (while (file-exists-p src-file)
		(condition-case err
		    (progn
		      (delete-file src-file)
		      (throw 'tag nil))
		  (error (princ (format "%s\n" (nth 1 err)))))))
	    (princ (format "%s -> %s\n" elc-file dest))))))))

(defun install-elisp-modules (modules src dest &optional just-print del-elc)
  (or just-print
      (file-exists-p dest)
      (make-directory dest t))
  (mapcar
   (lambda (module)
     (install-elisp-module module src dest just-print del-elc))
   modules))


;;; @ detect install path
;;;

;; install to shared directory (maybe "/usr/local")
(defvar install-prefix
  (if (and (eq system-type 'windows-nt) ; for NTEmacs
	   ;; Exclude the case that built by running the same
	   ;; configure script as on all other platforms.
	   (equal (file-name-nondirectory
		   (expand-file-name "." exec-directory))
		  "bin"))
      (expand-file-name "../../.." exec-directory)
    (expand-file-name "../../../.." data-directory)))

(defvar install-elisp-prefix "site-lisp")

;; Avoid compile warning.
(eval-when-compile (autoload 'replace-in-string "subr"))

(defun install-detect-elisp-directory (&optional prefix elisp-prefix
						 allow-version-specific)
  (or prefix
      (setq prefix install-prefix))
  (or elisp-prefix
      (setq elisp-prefix install-elisp-prefix))
  (or (catch 'tag
	(let ((rest (delq nil (copy-sequence default-load-path)))
	      (regexp
	       (concat "^"
		       (regexp-quote (file-name-as-directory
				      (expand-file-name prefix)))
		       ".*/"
		       (regexp-quote elisp-prefix)
		       "/?$"))
	      dir)
	  (while rest
	    (setq dir (car rest))
	    (if (string-match regexp dir)
		(if (or allow-version-specific
			(not (string-match (format "/%d\\.%d"
						   emacs-major-version
						   emacs-minor-version)
					   dir)))
		    (throw 'tag (car rest))))
	    (setq rest (cdr rest)))))
      (expand-file-name (concat "share/emacs/" elisp-prefix)
			prefix)))

(defvar install-default-elisp-directory
  (install-detect-elisp-directory))


;;; @ for XEmacs package system
;;;

(defun install-get-default-package-directory ()
  ;; Dummy function.  Do nothing.
  nil)

(defun install-update-package-files (_package _dir &optional _just-print)
  ;; Dummy function.  Do nothing.
  nil)


;;; @ Other Utilities
;;;

(defun install-just-print-p ()
  (let ((flag (getenv "MAKEFLAGS"))
	(case-fold-search nil))
    (princ (format "%s\n" flag))
    (if flag
	(string-match "^\\(\\(--[^ ]+ \\)+-\\|[^ =-]\\)*n" flag))))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'install) (require 'apel-ver))

;;; install.el ends here
