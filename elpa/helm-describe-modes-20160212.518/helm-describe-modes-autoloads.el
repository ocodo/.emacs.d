;;; helm-describe-modes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-describe-modes" "helm-describe-modes.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from helm-describe-modes.el

(autoload 'helm-describe-modes "helm-describe-modes" "\
A convenient Helm version of `describe-mode'.

By default, it lists the major mode, active minor modes, and
inactive minor modes.  Sources can be added or removed by
customizing `helm-describe-modes-function-list'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-describe-modes" '("helm-describe-modes-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-describe-modes-autoloads.el ends here
