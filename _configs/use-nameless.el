;;; use-nameless --- initialize nameless
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package nameless
  :init
  (add-hook 'emacs-lisp-mode-hook 'nameless-mode)

  :config
  (setq nameless-prefix "⎆"))

(provide 'use-nameless)
;;; use-nameless ends here
