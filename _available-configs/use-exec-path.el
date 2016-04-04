;;; use-exec-path --- initialize exec-path
;;; Commentary:
;;; Code:
(defvar exec-path-from-shell-initialize-done nil)
(defun ensure-exec-path-initialize (fun &rest args)
  "Advise `shell-command' (FUN with ARGS) to defer `exec-path' initialize.

After first run `exec-path-from-shell-initialize-done' will be set to t."
  (unless exec-path-from-shell-initialize-done
    (message "setting up environment with exec-path")
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
    (setq exec-path-from-shell-initialize-done t)))

(advice-add 'shell-command :before #'ensure-exec-path-initialize)
(provide 'use-exec-path)
;;; use-exec-path ends here
