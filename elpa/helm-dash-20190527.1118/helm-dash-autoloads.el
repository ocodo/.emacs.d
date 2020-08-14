;;; helm-dash-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-dash" "helm-dash.el" (0 0 0 0))
;;; Generated autoloads from helm-dash.el

(autoload 'helm-dash "helm-dash" "\
Bring up a `helm-dash' search interface.
If INPUT-PATTERN is non-nil, use it as an initial input in helm search.

\(fn &optional INPUT-PATTERN)" t nil)

(autoload 'helm-dash-at-point "helm-dash" "\
Bring up a `helm-dash' search interface with symbol at point." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-dash" '("helm-dash-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-dash-autoloads.el ends here
