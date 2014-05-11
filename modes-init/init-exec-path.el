;; init-exec-path
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize))
(provide 'init-exec-path)
