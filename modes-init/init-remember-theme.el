;; init-remember-theme
(require 'armitp-mode-line)
(add-hook 'remember-theme-after-load-hook
          (lambda ()
            (armitp-mode-line)))

(provide 'init-remember-theme)

;;; init-remember-theme.el ends here
