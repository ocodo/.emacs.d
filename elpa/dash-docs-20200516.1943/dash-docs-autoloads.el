;;; dash-docs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dash-docs" "dash-docs.el" (0 0 0 0))
;;; Generated autoloads from dash-docs.el

(autoload 'dash-docs-activate-docset "dash-docs" "\
Activate DOCSET.  If called interactively prompts for the docset name.

\(fn DOCSET)" t nil)

(autoload 'dash-docs-deactivate-docset "dash-docs" "\
Deactivate DOCSET.  If called interactively prompts for the docset name.

\(fn DOCSET)" t nil)

(autoload 'dash-docs-install-user-docset "dash-docs" "\
Download an unofficial docset with specified DOCSET-NAME and move its stuff to docsets-path.

\(fn DOCSET-NAME)" t nil)

(autoload 'dash-docs-install-docset-from-file "dash-docs" "\
Extract the content of DOCSET-TMP-PATH, move it to `dash-docs-docsets-path` and activate the docset.

\(fn DOCSET-TMP-PATH)" t nil)

(autoload 'dash-docs-install-docset "dash-docs" "\
Download an official docset with specified DOCSET-NAME and move its stuff to docsets-path.

\(fn DOCSET-NAME)" t nil)

(autoload 'dash-docs-async-install-docset "dash-docs" "\
Asynchronously download docset with specified DOCSET-NAME and move its stuff to docsets-path.

\(fn DOCSET-NAME)" t nil)

(autoload 'dash-docs-async-install-docset-from-file "dash-docs" "\
Asynchronously extract the content of DOCSET-TMP-PATH, move it to `dash-docs-docsets-path` and activate the docset.

\(fn DOCSET-TMP-PATH)" t nil)

(autoload 'dash-docs-ensure-docset-installed "dash-docs" "\
Install DOCSET if it is not currently installed.

\(fn DOCSET)" nil nil)

(autoload 'dash-docs-search "dash-docs" "\
Given a string PATTERN, query docsets and retrieve result.

\(fn PATTERN)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dash-docs" '("dash-docs-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dash-docs-autoloads.el ends here
