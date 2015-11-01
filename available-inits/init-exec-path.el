;;; init-exec-path --- initialize exec-path
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package exec-path
  :init ;; before use
  (progn
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")))

(provide 'init-exec-path)
;;; init-exec-path ends here
