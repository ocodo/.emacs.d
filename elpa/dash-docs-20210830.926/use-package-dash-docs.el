;;; use-package-dash-docs.el --- Adds :dash keyword to use-package macro

;; Copyright (C) 2019-2021 Damien Merenne

;; Author: Damien Merenne <dam@cosinux.org>
;; Created:  Sep 2018
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (use-package "2.4"))
;; Keywords: convenience extensions tools
;; URL: https://github.com/dash-docs-el/dash-docs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provides support for the :dash keyword, which is made available by
;; default by requiring `use-package'.

;;; Code:

(require 'use-package-core)

;;;###autoload
(defun use-package-normalize/:dash (name-symbol keyword arg)
  "Normalize use-package customize keyword."
  (let ((error-msg (format  "%s :%s wants a \"docset\" definition or (\"docset\" \"docset\" ...) list or a (mode-name \"docset\" \"docset\" ...) list or a list of these." name-symbol keyword)))
    (unless (listp arg)
      (use-package-error error-msg))
    (seq-map
     (lambda (def)
       (cond ((stringp def) (cons name-symbol arg))
             ((listp def) (if (symbolp (car def)) def
                            (cons name-symbol def)))
             (t (use-package-error error-msg))))
     arg)))

;;;###autoload
(defun use-package-handler/:dash (name keyword args rest state)
  "Generate use-package customize keyword code."
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (seq-map (lambda (def)
                (let ((mode (intern (format "%s-hook" (car def))))
                      (hook (intern (format "use-package-dash-setup-%s" (car def))))
                      (docsets (cdr def)))
                  `(progn
                     (defun ,hook ,()
                       (unless (boundp 'dash-docs-docsets)
                         (setq-local dash-docs-docsets nil))
                       (seq-do (lambda (docset)
                                 (add-to-list 'dash-docs-docsets docset)
                                 (dash-docs-ensure-docset-installed docset))
                               (quote ,docsets)))
                     (add-hook (quote ,mode) (function ,hook)))))
              args)
     body)))

(add-to-list 'use-package-keywords :dash t)

(provide 'use-package-dash-docs)
;;; use-package-dash-docs.el ends here
