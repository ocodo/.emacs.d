;; init-remember-theme
(add-hook 'remember-theme-after-load-hook
          (lambda ()
            (set-face-attribute 'default nil :height 180)
            (armitp-mode-line)
            ))

(provide 'init-remember-theme)
