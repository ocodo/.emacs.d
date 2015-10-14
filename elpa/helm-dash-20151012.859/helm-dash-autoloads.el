;;; helm-dash-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "helm-dash" "helm-dash.el" (22045 47999 61140
;;;;;;  0))
;;; Generated autoloads from helm-dash.el

(autoload 'helm-dash-install-user-docset "helm-dash" "\


\(fn DOCSET-NAME)" t nil)

(autoload 'helm-dash-install-docset-from-file "helm-dash" "\


\(fn DOCSET-TMP-PATH)" t nil)

(autoload 'helm-dash-install-docset "helm-dash" "\
Download docset with specified DOCSET-NAME and move its stuff to docsets-path.

\(fn DOCSET-NAME)" t nil)

(autoload 'helm-dash "helm-dash" "\
Bring up a Dash search interface in helm.

\(fn)" t nil)

(autoload 'helm-dash-at-point "helm-dash" "\
Bring up a Dash search interface in helm using the symbol at
point as prefilled search.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; helm-dash-autoloads.el ends here
