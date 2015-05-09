;; init-exec-path
(async-start
 (lambda ()
   (exec-path-from-shell-initialize)))

(provide 'init-exec-path)
