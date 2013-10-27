;;; pkg-info.el --- Information about packages       -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/pkg-info.el
;; Keywords: convenience
;; Version: 20131020.1746
;; X-Original-Version: 0.4-cvs
;; Package-Requires: ((dash "1.6.0") (epl "0.1"))

;; This file is not part of GNU Emacs.

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

;; This library extracts information from installed packages.

;;;; Functions:

;; `pkg-info-library-version' extracts the version from the header of a library.
;;
;; `pkg-info-defining-library-version' extracts the version from the header of a
;;  library defining a function.
;;
;; `pkg-info-package-version' gets the version of an installed package.
;;
;; `pkg-info-format-version' formats a version list as human readable string.

;;; Code:

(require 'epl)
(require 'dash)

(require 'find-func)


;;;; Version information
(defun pkg-info-format-version (version)
  "Format VERSION as human-readable string.

Return a human-readable string representing VERSION."
  ;; XXX: Find a better, more flexible way of formatting?
  (package-version-join version))

(defsubst pkg-info--show-version-and-return (version show)
  "Show and return VERSION.

When SHOW is non-nil, show VERSION in minibuffer.

Return VERSION."
  (when show
    (message (pkg-info-format-version version)))
  version)

;;;###autoload
(defun pkg-info-library-version (library &optional show)
  "Get the version in the header of LIBRARY.

LIBRARY is either a symbol denoting a named feature, or a library
name as string..

When SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no proper header.

See Info node `(elisp)Library Headers' for more information
about library headers."
  (interactive
   (list (->> (completing-read "Load library: "
                               (apply-partially 'locate-file-completion-table
                                                load-path
                                                (get-load-suffixes)))
           find-library-name)
         t))
  (let* ((library-name (if (symbolp library) (symbol-name library) library))
         (source (find-library-name library-name))
         (version (epl-package-version (epl-package-from-file source))))
    (pkg-info--show-version-and-return version show)))

;;;###autoload
(defun pkg-info-defining-library-version (function &optional show)
  "Get the version of the library defining FUNCTION.

When SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION (as by
`pkg-info-locate-library-version').  Signal an error if FUNCTION
is not a valid function, if its defining library was not found,
or if the library had no proper version header."
  (interactive
   (let ((input (completing-read "Function: " obarray #'boundp :require-match)))
     (list (if (string= input "") nil (intern input)) t)))
  (unless (functionp function)
    (signal 'wrong-type-argument (list 'functionp function)))
  (let ((library (symbol-file function 'defun)))
    (unless library
      (error "Can't find definition of %s" function))
    (pkg-info-library-version library show)))

;;;###autoload
(defun pkg-info-package-version (package &optional show)
  "Get the version of an installed PACKAGE.

When SHOW is non-nil, show the version in the minibuffer.

Return the version as list, or nil if PACKAGE is not installed."
  (interactive
   (let* ((installed (epl-installed-packages))
          (names (-sort #'string<
                        (--map (symbol-name (epl-package-name it)) installed)))
          (default (car names))
          (reply (completing-read "Installed package: " names nil 'require-match
                                  nil nil default)))
     (list reply t)))
  (let* ((name (if (stringp package) (intern package) package))
         (package (epl-find-installed-package name)))
    (unless package
      (error "Can't find installed package %s" name))
    (pkg-info--show-version-and-return (epl-package-version package) show)))

(provide 'pkg-info)

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:

;;; pkg-info.el ends here
