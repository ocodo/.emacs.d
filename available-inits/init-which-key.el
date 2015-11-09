;;; package --- init which-key-mode
;;; commentary:
;;; code:

(use-package which-key
  :init (progn
          (which-key-mode t)
          (which-key-setup-minibuffer))
  :bind ("C-x n g" . which-key-mode))

(provide 'init-which-key)
;;; init-which-key.el ends here
