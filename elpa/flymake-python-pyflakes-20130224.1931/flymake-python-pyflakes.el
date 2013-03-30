;;; flymake-python-pyflakes.el --- A flymake handler for python-mode files using pyflakes (or flake8)

;; Copyright (C) 2012 Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/flymake-python-pyflakes
;; Version: 20130224.1931
;; Package-Requires: ((flymake-easy "0.4"))

;;; Commentary:

;; Usage:
;;   (require 'flymake-python-pyflakes)
;;   (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
;;
;; To use "flake8" instead of "pyflakes", add this line:
;;   (setq flymake-python-pyflakes-executable "flake8")
;;
;; Uses flymake-easy, from https://github.com/purcell/flymake-easy

;;; Code:

(require 'flymake-easy)

(defconst flymake-python-pyflakes-err-line-patterns
  '(("^\\(.*?\\.pyw?\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)
    ;; flake8
    ("^\\(.*?\\.pyw?\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$" 1 2 3 4)))

(defvar flymake-python-pyflakes-executable "pyflakes"
  "Pyflakes executable to use for syntax checking.")

(defun flymake-python-pyflakes-command (filename)
  "Construct a command that flymake can use to syntax-check FILENAME."
  (list flymake-python-pyflakes-executable filename))

;;;###autoload
(defun flymake-python-pyflakes-load ()
  "Configure flymake mode to check the current buffer's python syntax using pyflakes."
  (interactive)
  (flymake-easy-load 'flymake-python-pyflakes-command
                     flymake-python-pyflakes-err-line-patterns
                     'inplace
                     "py"
                     "^\\([WFCN]\\|E[0-7]\\)"))


(provide 'flymake-python-pyflakes)
;;; flymake-python-pyflakes.el ends here
