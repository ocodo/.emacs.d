;; Anzu mode, pretty isearch and query replace
(global-anzu-mode t)

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "M-C-%") 'anzu-query-replace-regexp)

(provide 'init-anzu)
