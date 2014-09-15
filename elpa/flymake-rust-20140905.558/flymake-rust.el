;;; flymake-rust.el --- A flymake handler for rust-mode files
;;
;;; Author: Joao Oliveira <joaoxsouls@gmail.com>
;;; URL: https://github.com/joaoxsouls/flymake-rust
;; Version: 20140905.558
;;; X-Original-Version: DEV
;;; Package-Requires: ((flymake-easy "0.1"))
;;;
;;; Commentary:
;; Usage:
;;   (require 'flymake-rust)
;;   (add-hook 'rust-mode-hook 'flymake-rust-load)
;;
;; Uses flymake-easy, from https://github.com/purcell/flymake-easy

;;; Code:

(require 'flymake-easy)

(defconst flymake-rust-err-line-patterns
  '(("^\\(.*.rs\\):\\([0-9]+\\):[0-9]+: [0-9]+:[0-9]+ [a-z]+: \\(.*\\)$" 1 2 nil 3))
  '(("^\\(.*.rs\\):\\([0-9]+\\) \\(.*\\)$" 1 2 nil 3)))

(defvar flymake-rust-executable "rustc"
  "The rust executable to use for syntax checking.")

;; Invoke rust "--parse-only" to get syntax checking
(defun flymake-rust-command (filename)
  "Construct a command that flymake can use to check rust source."
(list flymake-rust-executable "--no-trans" filename))

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
