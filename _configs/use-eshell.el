;;; use-eshell --- initialize eshell
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(provide 'use-eshell)
;;; use-eshell ends here
