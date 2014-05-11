;; init-remember-theme
(add-hook 'remember-theme-after-load-hook
          (lambda ()
            (set-face-attribute 'default nil :height 180)
            (set-face-attribute 'linum nil :height 110)
            (require 'armitp-mode-line)
            ))

(provide 'init-remember-theme)
