;;; helm-dash-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-dash" "helm-dash.el" (0 0 0 0))
;;; Generated autoloads from helm-dash.el

(autoload 'helm-dash-activate-docset "helm-dash" "\
Activate DOCSET.  If called interactively prompts for the docset name.

\(fn DOCSET)" t nil)

(autoload 'helm-dash-deactivate-docset "helm-dash" "\
Deactivate DOCSET.  If called interactively prompts for the docset name.

\(fn DOCSET)" t nil)

(autoload 'helm-dash-install-user-docset "helm-dash" "\
Download an unofficial docset with specified DOCSET-NAME and move its stuff to docsets-path.

\(fn DOCSET-NAME)" t nil)

(autoload 'helm-dash-install-docset-from-file "helm-dash" "\
Extract the content of DOCSET-TMP-PATH, move it to `helm-dash-docsets-path` and activate the docset.

\(fn DOCSET-TMP-PATH)" t nil)

(autoload 'helm-dash-install-docset "helm-dash" "\
Download an official docset with specified DOCSET-NAME and move its stuff to docsets-path.

\(fn DOCSET-NAME)" t nil)

(autoload 'helm-dash-async-install-docset "helm-dash" "\
Asynchronously download docset with specified DOCSET-NAME and move its stuff to docsets-path.

\(fn DOCSET-NAME)" t nil)

(autoload 'helm-dash-async-install-docset-from-file "helm-dash" "\
Asynchronously extract the content of DOCSET-TMP-PATH, move it to `helm-dash-docsets-path` and activate the docset.

\(fn DOCSET-TMP-PATH)" t nil)

(autoload 'helm-dash-ensure-docset-installed "helm-dash" "\
Install DOCSET if it is not currently installed.

\(fn DOCSET)" nil nil)

(autoload 'helm-dash "helm-dash" "\
Bring up a `helm-dash' search interface.
If INPUT-PATTERN is non-nil, use it as an initial input in helm search.

\(fn &optional INPUT-PATTERN)" t nil)

(autoload 'helm-dash-at-point "helm-dash" "\
Bring up a `helm-dash' search interface with symbol at point.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-dash" '("helm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-dash-autoloads.el ends here
