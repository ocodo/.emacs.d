;;; use-which-key --- use which-key-mode
;;; commentary:
;;; code:

(use-package which-key
  :init (progn
          (which-key-mode t)
          (setq which-key-sort-order 'which-key-description-order)
          (which-key-setup-side-window-bottom))
  :bind
  ("C-x n g" . which-key-mode)
  ("C-h M-t" . which-key-show-top-level))

(provide 'use-which-key)
;;; use-which-key.el ends here
