;;; flymake-rust.el --- A flymake handler for rust-mode files
;;
;;; Author: Joao Oliveira <joaoxsouls@gmail.com>
;;; URL: https://github.com/joaoxsouls/flymake-rust
;; Package-Version: 20170729.2139
;;; Version: DEV
;;; Package-Requires: ((flymake-easy "0.1"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.;;;

;;; Commentary:
;; Usage:
;;   (require 'flymake-rust)
;;   (add-hook 'rust-mode-hook 'flymake-rust-load)
;;
;; If you want to use rustc compiler, you must add following string:
;;   (setq flymake-rust-use-cargo 1)
;;
;; Uses flymake-easy, from https://github.com/purcell/flymake-easy

;;; Code:

(require 'flymake-easy)

(defconst flymake-rust-err-line-patterns
  '(("^\\(.*\\)\n   --> \\(.*.rs\\):\\([0-9]+\\):\\([0-9]+\\)$" 2 3 4 1)
    ("^\\(.*.rs\\):\\([0-9]+\\):[0-9]+: [0-9]+:[0-9]+ [a-z]+: \\(.*\\)$" 1 2 nil 3)
    ("^\\(.*.rs\\):\\([0-9]+\\) \\(.*\\)$" 1 2 nil 3)))

(setq-default flymake-rust-use-cargo 1)

(if flymake-rust-use-cargo
    (defvar flymake-rust-executable "cargo"
      "The rust executable to use for syntax checking.")
    (defvar flymake-rust-executable "rustc"
      "The rust executable to use for syntax checking.")
)

;; Invoke rust "--parse-only" to get syntax checking
(defun flymake-rust-command (filename)
  "Construct a command that flymake can use to check rust source."
  (if flymake-rust-use-cargo
      (list flymake-rust-executable "build")
      (list flymake-rust-executable "--no-trans" filename)
      )
  )

;; Load rust-flymake
(defun flymake-rust-load ()
  "Configure flymake mode to check the current buffer's rust syntax."
  (interactive)
  (flymake-easy-load 'flymake-rust-command
                     flymake-rust-err-line-patterns
                     'inplace
                     "rs"))

(provide 'flymake-rust)
;;; flymake-rust.el ends here
