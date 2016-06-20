;; use-flycheck
(add-hook 'after-use-hook #'global-flycheck-mode)

(add-hook 'prog-mode-hook #'flycheck-mode)
