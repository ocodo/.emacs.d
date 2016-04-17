;;; use-which-key --- use which-key-mode
;;; commentary:
;;; code:

(use-package which-key
  :init (progn
          (which-key-mode t)
          (which-key-setup-minibuffer))
  :bind ("C-x n g" . which-key-mode))

(provide 'use-which-key)
;;; use-which-key.el ends here
