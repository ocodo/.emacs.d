;; init-remember-theme
;; (add-hook 'remember-theme-after-load-hook
;;           (lambda ()
;;             (amitp-mode-line)))

(remember-theme-load)
(add-hook 'kill-emacs-hook 'remember-theme-save)

(provide 'init-remember-theme)
;;; init-remember-theme.el ends here
