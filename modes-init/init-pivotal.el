;;; init-pivotal.el --- Initialize pivotal mode
;;
;;; Commentary:
;;  Attempt to set the pivotal-api-token from the system environment.
;;
;;; Code:

(add-hook 'pivotal-mode-hook
  (lambda ()
    "Trying to find the Pivotal API key in the shell environment"
    (message ">>> Trying to find the Pivotal API key in the shell environment")
    (if (functionp 'exec-path-from-shell-getenv)
        (when (or (eq pivotal-api-token nil) (eq pivotal-api-token ""))
          (let (api-token-from-env)
            (setq api-token-from-env (exec-path-from-shell-getenv "PIVOTAL_API_KEY"))
            (if (not (eq "" api-token-from-env))
                (progn
                  (message "Setting pivotal-api-token from system environment")
                  (setq pivotal-api-token api-token-from-env))
              ;; else
              (message "You can automatically set pivotal-api-token from the system environment variable $PIVOTAL_API_KEY")))
        ))))

(provide 'init-pivotal)

;;; init-pivotal.el ends here
