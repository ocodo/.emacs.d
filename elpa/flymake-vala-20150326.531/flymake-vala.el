;;; flymake-vala.el --- A flymake handler for vala-mode files

;; Copyright (C) 2015 Daniel Lawrence

;; Author: Daniel Lawrence <dannyla@linux.com>
;; URL: https://github.com/daniellawrence/flymake-vala
;; Package-Version: 20150326.531
;; Version: DEV
;; Keywords: convenience, vala
;; Package-Requires: ((flymake-easy "0.1"))

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

;;;
;;; Commentary:
;; Usage:
;;   (require 'flymake-vala)
;;   (add-hook 'vala-mode-hook 'flymake-vala-load)
;;
;; Uses flymake-easy, from https://github.com/purcell/flymake-easy

;;; Code:

(require 'flymake-easy)

;; Example error
;; hello.vala:3.2-3.14: error: Return: Cannot convert from `string' to `int'
;;	return "foo";
;;	^^^^^^^^^^^^^
;; Compilation failed: 1 error(s), 0 warning(s)


(defconst flymake-vala-err-line-patterns
 '(("^\\(.*\.vala\\):\\([0-9]+\\)\.\\(.*\\)$" nil 2 nil 3)))

(defvar flymake-vala-executable "vala"
  "The vala executable to use for syntax checking.")

;; Invoke vala with '-c' to get syntax checking
(defun flymake-vala-command (filename)
  "Construct a command that flymake can use to check vala source."
  (list flymake-vala-executable "-c" filename))

;;;###autoload
(defun flymake-vala-load ()
  "Configure flymake mode to check the current buffer's vala syntax."
  (interactive)
  (flymake-easy-load 'flymake-vala-command
                     flymake-vala-err-line-patterns
                     'tempdir
                     "vala"))

(provide 'flymake-vala)
;;; flymake-vala.el ends here
