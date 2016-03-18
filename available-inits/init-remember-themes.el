;; init-remember-themes
;; (add-hook 'remember-theme-after-load-hook
;;           (lambda ()
;;             (amitp-mode-line)))

(require 'remember-themes)
(remember-theme-load)
(add-hook 'kill-emacs-hook 'remember-theme-save)

(provide 'init-remember-themes)
;;; init-remember-themes.el ends here
