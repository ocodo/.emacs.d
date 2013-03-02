;;; flymake-haml.el --- A flymake handler for haml files
;;
;;; Author: Steve Purcell <steve@sanityinc.com>
;;; URL: https://github.com/purcell/flymake-haml
;;; Version: DEV
;;; Package-Requires: ((flymake-easy "0.1"))
;;
;;; Commentary:
;;
;; Usage:
;;   (require 'flymake-haml)
;;   (add-hook 'haml-mode-hook 'flymake-haml-load)
;;
;; Uses flymake-easy, from https://github.com/purcell/flymake-easy

;;; Code:

(require 'flymake-easy)

(defconst flymake-haml-err-line-patterns '(("^Syntax error on line \\([0-9]+\\): \\(.*\\)$" nil 1 nil 2)))

;; Invoke utilities with '-c' to get syntax checking
(defun flymake-haml-command (filename)
  "Construct a command that flymake can use to check haml source."
  (list "haml" "-c" filename))

;;;###autoload
(defun flymake-haml-load ()
  "Configure flymake mode to check the current buffer's haml syntax.

This function is designed to be called in `haml-mode-hook'; it
does not alter flymake's global configuration, so function
`flymake-mode' alone will not suffice."
  (interactive)
  (flymake-easy-load 'flymake-haml-command
                     flymake-haml-err-line-patterns
                     'tempdir
                     "haml"))


(provide 'flymake-haml)
;;; flymake-haml.el ends here
