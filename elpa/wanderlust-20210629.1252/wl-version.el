;;; wl-version.el --- Version information for Wanderlust.  -*- lexical-binding: t -*-

;; Copyright (C) 2000-2001 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 2000-2001 TAKAHASHI Kaoru <kaoru@kaisei.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	TAKAHASHI Kaoru <kaoru@kaisei.org>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;
;; Put the following lines to each file of Wanderlust package.
;;
;; (require 'product)
;; (product-provide (provide FEATURE) (require 'wl-version))

;;; Code:
;;
(require 'product)
(require 'elmo-version)			; product-version-as-string
(provide 'wl-version)			; before product-provide

;; product-define in the first place
(product-provide 'wl-version
  (product-define
   "Wanderlust" nil
   (eval-when-compile
     (product-version (product-find 'elmo-version))) ; equals to ELMO version.
   "Almost Unreal"))

(defconst wl-version-status nil
  "Wanderlust verstion status.  For override default rule.
If nil, use default rule.")


;; set version-string
(product-version-as-string 'wl-version)

(defun wl-version ()
  "Print Wanderlust version.
Don't support insert string at-point (C-u M-x wl-version).
For bug report, use `wl-generate-user-agent-string-1' instead.
When non-interactive, use `product-string-1' instead."
  (interactive)
  (let ((product-info (product-string-1 'wl-version t)))
    (if (called-interactively-p 'interactive)
	(message "%s" product-info)
      product-info)))

(defun wl-version-status ()
  "Return version status string.
If variable `wl-version-status' is non-nil, override default rule."
  (or wl-version-status
      (if (zerop (% (nth 1 (product-version (product-find 'wl-version))) 2))
	  "stable"
	"beta")))

;; avoid compile warnings
(defvar emacs-beta-version)
(defvar mime-edit-insert-user-agent-field)
(defvar mime-edit-user-agent-value)
(defvar mime-editor/version)
(defvar mime-editor/codename)

(defun wl-generate-user-agent-string ()
  "A candidate of `wl-generate-mailer-string-function'.
Insert User-Agent field instead of X-Mailer field."
  (wl-generate-user-agent-string-1
   ;; for backward compatibility
   mime-edit-insert-user-agent-field))

(defun wl-generate-user-agent-string-1 (&optional verbose)
  "Return User-Agent field value.
If VERBOSE return with SEMI, FLIM and APEL version."
  (cond
   ;; Don't use `product-string-verbose' for short User-Agent field value.
   ((not verbose)
    (concat (product-string-1 'wl-version t) " "
	    (wl-extended-emacs-version3 "/" t)))
   ;; SEMI (verbose)
   (mime-edit-user-agent-value
    (concat (product-string-verbose 'wl-version) " "
	    mime-edit-user-agent-value))))

;; from gnus
(defun wl-extended-emacs-version (&optional _with-codename)
  "Stringified Emacs version.
WITH-CODENAME is ignored."
  (cond
   ((string-match "^\\([0-9]+\\.[0-9]+\\)\\.[.0-9]+$" emacs-version)
    (concat "Emacs " (match-string 1 emacs-version)
	    (when (boundp 'mule-version) (concat "/Mule " mule-version))))
   ((string-match "\\([A-Z]*[Mm][Aa][Cc][Ss]\\)[^(]*\\(\\((beta.*)\\|'\\)\\)?"
		  emacs-version)
    (concat (match-string 1 emacs-version)
	    (format " %d.%d" emacs-major-version emacs-minor-version)
	    (when (and (boundp 'emacs-beta-version) emacs-beta-version)
	      (format "b%d" emacs-beta-version))))
   (t emacs-version)))

(defun wl-extended-emacs-version2 (&optional delimiter _with-codename)
  "Stringified Emacs version.
Separate DELIMITER (default is \" \").
WITH-CODENAME is ignored."
  (cond
   ((and (boundp 'mule-version)
	 (string-match "\\([0-9]+\.[0-9]+\\)\\(.*$\\)" mule-version))
    (format "Mule%s%s@%d.%d"
	    (or delimiter " ")
	    (match-string 1 mule-version)
	    emacs-major-version
	    emacs-minor-version))
   ((string-match "^\\([0-9]+\\.[0-9]+\\)\\.[.0-9]+$" emacs-version)
    (concat "Emacs" (or delimiter " ")
	    (match-string 1 emacs-version)))
   ((string-match "\\([A-Z]*[Mm][Aa][Cc][Ss]\\)[^(]*\\(\\((beta.*)\\|'\\)\\)?"
		  emacs-version)
    (concat (match-string 1 emacs-version)
	    (or delimiter " ")
	    (format "%d.%d" emacs-major-version emacs-minor-version)
	    (when (and (boundp 'emacs-beta-version) emacs-beta-version)
	      (format "b%d" emacs-beta-version))))
   (t emacs-version)))

(defun wl-extended-emacs-version3 (&optional delimiter _with-codename)
  "Stringified Emacs version.
Separate DELIMITER (default is \" \").
WITH-CODENAME is ignored."
  (cond
   ((and (boundp 'mule-version)
	 (string-match "\\([0-9]+\.[0-9]+\\)\\(.*$\\)" mule-version))
    (format "Emacs%s%d.%d Mule%s%s"
	    (or delimiter " ")
	    emacs-major-version
	    emacs-minor-version
	    (or delimiter " ")
	    (match-string 1 mule-version)))
   ((string-match "^\\([0-9]+\\.[0-9]+\\)\\.[.0-9]+$" emacs-version)
    (concat "Emacs" (or delimiter " ")
	    (match-string 1 emacs-version)))
   ((string-match "\\([A-Z]*[Mm][Aa][Cc][Ss]\\)[^(]*\\(\\((beta.*)\\|'\\)\\)?"
		  emacs-version)
    (concat (match-string 1 emacs-version)
	    (or delimiter " ")
	    (format "%d.%d" emacs-major-version emacs-minor-version)
	    (when (and (boundp 'emacs-beta-version) emacs-beta-version)
	      (format "b%d" emacs-beta-version))))
   (t emacs-version)))


;; for backward compatibility
(defconst wl-appname (product-name (product-find 'wl-version)))
(make-obsolete-variable
 'wl-appname
 "use (product-name (product-find 'wl-version)) insteaed." "10 Oct 2000")

(defconst wl-version (product-version-string (product-find 'wl-version)))
(make-obsolete-variable
 'wl-version
 "use (product-version-string (product-find 'wl-version)) instead."
 "10 Oct 2000")

(defconst wl-codename (product-code-name (product-find 'wl-version)))
(make-obsolete-variable
 'wl-codename
 "use (product-code-name (product-find 'wl-version)) instead." "10 Oct 2000")

;;; wl-version.el ends here
