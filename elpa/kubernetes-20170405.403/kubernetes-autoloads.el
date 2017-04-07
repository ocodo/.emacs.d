;;; kubernetes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "kubernetes" "kubernetes.el" (0 0 0 0))
;;; Generated autoloads from kubernetes.el

(autoload 'kubernetes-display-config "kubernetes" "\
Display information for CONFIG in a new window.

\(fn CONFIG)" t nil)

(autoload 'kubernetes-display-configmap "kubernetes" "\
Display information for a configmap in a new window.

CONFIGMAP-NAME is the name of the configmap to display.

\(fn CONFIGMAP-NAME)" t nil)

(autoload 'kubernetes-display-secret "kubernetes" "\
Display information for a secret in a new window.

SECRET-NAME is the name of the secret to display.

\(fn SECRET-NAME)" t nil)

(autoload 'kubernetes-display-service "kubernetes" "\
Display information for a service in a new window.

SERVICE-NAME is the name of the service to display.

\(fn SERVICE-NAME)" t nil)

(autoload 'kubernetes-display-pod "kubernetes" "\
Display information for a pod in a new window.

POD-NAME is the name of the pod to display.

\(fn POD-NAME)" t nil)

(defvar kubernetes-mode-map (let ((keymap (make-sparse-keymap))) (define-key keymap (kbd "p") #'magit-section-backward) (define-key keymap (kbd "n") #'magit-section-forward) (define-key keymap (kbd "M-p") #'magit-section-backward-sibling) (define-key keymap (kbd "M-n") #'magit-section-forward-sibling) (define-key keymap (kbd "C-i") #'magit-section-toggle) (define-key keymap (kbd "^") #'magit-section-up) (define-key keymap [tab] #'magit-section-toggle) (define-key keymap [C-tab] #'magit-section-cycle) (define-key keymap [M-tab] #'magit-section-cycle-diffs) (define-key keymap [S-tab] #'magit-section-cycle-global) (define-key keymap (kbd "q") #'quit-window) (define-key keymap (kbd "RET") #'kubernetes-navigate) (define-key keymap (kbd "M-w") #'kubernetes-copy-thing-at-point) keymap) "\
Keymap for `kubernetes-mode'.  This is the base keymap for all derived modes.")

(autoload 'kubernetes-mode "kubernetes" "\
Base mode for Kubernetes modes.

\\{kubernetes-mode-map}

\(fn)" t nil)

(defvar kubernetes-display-pods-mode-map (let ((keymap (make-sparse-keymap))) (define-key keymap (kbd "?") #'kubernetes-overview-popup) (define-key keymap (kbd "c") #'kubernetes-config-popup) (define-key keymap (kbd "d") #'kubernetes-describe-popup) (define-key keymap (kbd "D") #'kubernetes-mark-for-delete) (define-key keymap (kbd "e") #'kubernetes-exec-popup) (define-key keymap (kbd "g") #'kubernetes-refresh) (define-key keymap (kbd "u") #'kubernetes-unmark) (define-key keymap (kbd "U") #'kubernetes-unmark-all) (define-key keymap (kbd "x") #'kubernetes-execute-marks) (define-key keymap (kbd "l") #'kubernetes-logs-popup) (define-key keymap (kbd "h") #'describe-mode) keymap) "\
Keymap for `kubernetes-display-pods-mode'.")

(autoload 'kubernetes-display-pods-mode "kubernetes" "\
Mode for working with Kubernetes pods.

\\<kubernetes-display-pods-mode-map>Type \\[kubernetes-mark-for-delete] to mark a pod for deletion, and \\[kubernetes-execute-marks] to execute.
Type \\[kubernetes-unmark] to unmark the pod at point, or \\[kubernetes-unmark-all] to unmark all pods.

Type \\[kubernetes-navigate] to inspect the object on the current line.

Type \\[kubernetes-exec] to exec into a pod.

Type \\[kubernetes-logs] when point is on a pod to view its logs.

Type \\[kubernetes-copy-thing-at-point] to copy the pod name at point.

Type \\[kubernetes-refresh] to refresh the buffer.

\\{kubernetes-display-pods-mode-map}

\(fn)" t nil)

(defvar kubernetes-display-configmaps-mode-map (let ((keymap (make-sparse-keymap))) (define-key keymap (kbd "?") #'kubernetes-overview-popup) (define-key keymap (kbd "c") #'kubernetes-config-popup) (define-key keymap (kbd "d") #'kubernetes-describe-popup) (define-key keymap (kbd "D") #'kubernetes-mark-for-delete) (define-key keymap (kbd "g") #'kubernetes-refresh) (define-key keymap (kbd "u") #'kubernetes-unmark) (define-key keymap (kbd "U") #'kubernetes-unmark-all) (define-key keymap (kbd "x") #'kubernetes-execute-marks) (define-key keymap (kbd "h") #'describe-mode) keymap) "\
Keymap for `kubernetes-display-configmaps-mode'.")

(autoload 'kubernetes-display-configmaps-mode "kubernetes" "\
Mode for working with Kubernetes configmaps.

\\<kubernetes-display-configmaps-mode-map>Type \\[kubernetes-mark-for-delete] to mark a configmap for deletion, and \\[kubernetes-execute-marks] to execute.
Type \\[kubernetes-unmark] to unmark the configmap at point, or \\[kubernetes-unmark-all] to unmark all configmaps.

Type \\[kubernetes-navigate] to inspect the object on the current line.

Type \\[kubernetes-copy-thing-at-point] to copy the configmap name at point.

Type \\[kubernetes-refresh] to refresh the buffer.

\\{kubernetes-display-configmaps-mode-map}

\(fn)" t nil)

(defvar kubernetes-display-secrets-mode-map (let ((keymap (make-sparse-keymap))) (define-key keymap (kbd "?") #'kubernetes-overview-popup) (define-key keymap (kbd "c") #'kubernetes-config-popup) (define-key keymap (kbd "d") #'kubernetes-describe-popup) (define-key keymap (kbd "D") #'kubernetes-mark-for-delete) (define-key keymap (kbd "g") #'kubernetes-refresh) (define-key keymap (kbd "u") #'kubernetes-unmark) (define-key keymap (kbd "U") #'kubernetes-unmark-all) (define-key keymap (kbd "x") #'kubernetes-execute-marks) (define-key keymap (kbd "h") #'describe-mode) keymap) "\
Keymap for `kubernetes-display-secrets-mode'.")

(autoload 'kubernetes-display-secrets-mode "kubernetes" "\
Mode for working with Kubernetes secrets.

\\<kubernetes-display-secrets-mode-map>Type \\[kubernetes-mark-for-delete] to mark a secret for deletion, and \\[kubernetes-execute-marks] to execute.
Type \\[kubernetes-unmark] to unmark the secret at point, or \\[kubernetes-unmark-all] to unmark all secrets.

Type \\[kubernetes-navigate] to inspect the object on the current line.

Type \\[kubernetes-copy-thing-at-point] to copy the secret name at point.

Type \\[kubernetes-refresh] to refresh the buffer.

\\{kubernetes-display-secrets-mode-map}

\(fn)" t nil)

(defvar kubernetes-logs-mode-map (let ((keymap (make-sparse-keymap))) (define-key keymap (kbd "n") #'kubernetes-logs-forward-line) (define-key keymap (kbd "p") #'kubernetes-logs-previous-line) (define-key keymap (kbd "RET") #'kubernetes-logs-inspect-line) keymap) "\
Keymap for `kubernetes-logs-mode'.")

(autoload 'kubernetes-logs-mode "kubernetes" "\
Mode for displaying and inspecting Kubernetes logs.

\\<kubernetes-logs-mode-map>Type \\[kubernetes-logs-inspect-line] to open the line at point in a new buffer.

\\{kubernetes-logs-mode-map}

\(fn)" nil nil)

(defvar kubernetes-log-line-mode-map (let ((keymap (make-sparse-keymap))) (define-key keymap (kbd "n") #'kubernetes-logs-forward-line) (define-key keymap (kbd "p") #'kubernetes-logs-previous-line) keymap) "\
Keymap for `kubernetes-log-line-mode'.")

(autoload 'kubernetes-log-line-mode "kubernetes" "\
Mode for inspecting Kubernetes log lines.

\\{kubernetes-log-line-mode-map}

\(fn)" nil nil)

(autoload 'kubernetes-display-thing-mode "kubernetes" "\
Mode for inspecting a Kubernetes object.

\\{kubernetes-display-thing-mode-map}

\(fn)" t nil)

(defvar kubernetes-overview-mode-map (let ((keymap (make-sparse-keymap))) (define-key keymap (kbd "?") #'kubernetes-overview-popup) (define-key keymap (kbd "c") #'kubernetes-config-popup) (define-key keymap (kbd "d") #'kubernetes-describe-popup) (define-key keymap (kbd "g") #'kubernetes-refresh) (define-key keymap (kbd "u") #'kubernetes-unmark) (define-key keymap (kbd "U") #'kubernetes-unmark-all) (define-key keymap (kbd "x") #'kubernetes-execute-marks) (define-key keymap (kbd "h") #'describe-mode) keymap) "\
Keymap for `kubernetes-overview-mode'.")

(autoload 'kubernetes-overview-mode "kubernetes" "\
Mode for working with Kubernetes overview.

\\<kubernetes-overview-mode-map>Type \\[kubernetes-mark-for-delete] to mark an object for deletion, and \\[kubernetes-execute-marks] to execute.
Type \\[kubernetes-unmark] to unmark the object at point, or \\[kubernetes-unmark-all] to unmark all objects.

Type \\[kubernetes-navigate] to inspect the object on the current line.

Type \\[kubernetes-copy-thing-at-point] to copy the thing at point.

Type \\[kubernetes-refresh] to refresh the buffer.

\\{kubernetes-overview-mode-map}

\(fn)" t nil)

(autoload 'kubernetes-display-pods "kubernetes" "\
Display a list of pods in the current Kubernetes context.

\(fn)" t nil)

(autoload 'kubernetes-display-configmaps "kubernetes" "\
Display a list of configmaps in the current Kubernetes context.

\(fn)" t nil)

(autoload 'kubernetes-display-secrets "kubernetes" "\
Display a list of secrets in the current Kubernetes context.

\(fn)" t nil)

(autoload 'kubernetes-overview "kubernetes" "\
Display an overview buffer for Kubernetes.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kubernetes" '("kubernetes-")))

;;;***

;;;### (autoloads nil nil ("kubernetes-evil.el" "kubernetes-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; kubernetes-autoloads.el ends here
