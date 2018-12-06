;;; flymake-haskell-multi.el --- Syntax-check haskell-mode using both ghc and hlint

;; Copyright (c) 2013-2017 Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/flymake-haskell-multi
;; Package-Version: 0
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

;;; Commentary:

;; Usage:
;;   (require 'flymake-haskell-multi)
;;   (add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)
;;
;; Uses flymake-easy, from https://github.com/purcell/flymake-easy

;;; Code:

(require 'flymake-easy)
(require 'find-func)

(defconst flymake-haskell-multi-err-line-patterns
  '(("^\\(.*\.\\(?:l?[gh]s\\|hi\\)\\):\\([0-9]+\\):\\([0-9]+\\):\n +\\(\\(?:.+\\)\\(?:\n +.+\\)*\\)" 1 2 3 4) ; ghc
    ("^\\(.*\.\\(?:l?[gh]s\\|hi\\)\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\(?:\n.+\\)+\\)" 1 2 3 4) ; hlint
    ))

(defvar flymake-haskell-multi-executable "haskell_multi"
  "The executable to use for syntax checking.")

(defun flymake-haskell-multi-command (filename)
  "Construct a command that flymake can use to check haskell source in FILENAME."
  (list (expand-file-name
         flymake-haskell-multi-executable
         (file-name-as-directory
          (file-name-directory
           (find-library-name "flymake-haskell-multi")))) filename))

;;;###autoload
(defun flymake-haskell-multi-load ()
  "Configure flymake mode to check the current buffer's hlint syntax."
  (interactive)
  (flymake-easy-load 'flymake-haskell-multi-command
                     flymake-haskell-multi-err-line-patterns
                     'inplace
                     "hs"))

(provide 'flymake-haskell-multi)
;;; flymake-haskell-multi.el ends here
