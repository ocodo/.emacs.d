;;; flycheck-cython.el --- Support Cython in flycheck

;; Copyright (C) 2016 Lorenzo Bolla <lbolla@gmail.com>
;;
;; Author: Lorenzo Bolla <lbolla@gmail.com>
;; Created: 26 March 2016
;; Version: 1.0
;; Package-Version: 20160327.1228
;; Package-Requires: ((flycheck "0.25"))

;;; Commentary:

;; This package adds support for cython to flycheck.  It requires
;; cython>=0.23.0.

;; To use it, add to your init.el:

;; (require 'flycheck-cython)
;; (add-hook 'cython-mode-hook 'flycheck-mode)

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'flycheck)

(flycheck-def-option-var flycheck-cython-cplus nil cython
  "Whether to run Cython in C++ code.

Passes `--cplus' to cython if set."
  :type 'boolean
  :safe #'booleanp)

(flycheck-def-option-var flycheck-cython-include-dir nil cython
  "A list of include directories for Cython.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of Cython."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p)

(flycheck-define-checker cython
  "Cython checker."
  :command ("cython"
            "-Wextra"
            (option-flag "--cplus" flycheck-cython-cplus)
            (option-list "--include-dir" flycheck-cython-include-dir)
            "-o" temporary-file-name
            source-original)
  :error-patterns
  ((warning line-start
            "warning: "
            (file-name)
            ":"
            line
            ":"
            column
            ": "
            (message) line-end)
   (error line-start
          (file-name)
          ":"
          line
          ":"
          column
          ": "
          (message) line-end))
  :modes cython-mode)

(add-to-list 'flycheck-checkers 'cython t)

(provide 'flycheck-cython)
;;; flycheck-cython.el ends here
