;;; ag-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ag-regexp-project-at-point ag-project-regexp ag-project
;;;;;;  ag-regexp ag) "ag" "ag.el" (20923 54391 0 0))
;;; Generated autoloads from ag.el

(autoload 'ag "ag" "\
Search using ag in a given DIRECTORY for a given search STRING,
with STRING defaulting to the symbol under point.

\(fn STRING DIRECTORY)" t nil)

(autoload 'ag-regexp "ag" "\
Search using ag in a given directory for a given regexp.

\(fn STRING DIRECTORY)" t nil)

(autoload 'ag-project "ag" "\
Guess the root of the current project and search it with ag
for the given string.

\(fn STRING)" t nil)

(autoload 'ag-project-regexp "ag" "\
Guess the root of the current project and search it with ag
for the given regexp.

\(fn REGEXP)" t nil)

(defalias 'ag-project-at-point 'ag-project)

(autoload 'ag-regexp-project-at-point "ag" "\
Same as ``ag-regexp-project'', but with the search regexp defaulting
to the symbol under point.

\(fn REGEXP)" t nil)

;;;***

;;;### (autoloads nil nil ("ag-pkg.el") (20923 54391 420353 0))

;;;***

(provide 'ag-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ag-autoloads.el ends here
