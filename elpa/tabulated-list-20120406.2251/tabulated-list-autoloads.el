;;; tabulated-list-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (tabulated-list-mode) "tabulated-list" "tabulated-list.el"
;;;;;;  (20729 13828))
;;; Generated autoloads from tabulated-list.el

(autoload 'tabulated-list-mode "tabulated-list" "\
Generic major mode for browsing a list of items.
This mode is usually not used directly; instead, other major
modes are derived from it, using `define-derived-mode'.

In this major mode, the buffer is divided into multiple columns,
which are labelled using the header line.  Each non-empty line
belongs to one \"entry\", and the entries can be sorted according
to their column values.

An inheriting mode should usually do the following in their body:

 - Set `tabulated-list-format', specifying the column format.
 - Set `tabulated-list-revert-hook', if the buffer contents need
   to be specially recomputed prior to `revert-buffer'.
 - Maybe set a `tabulated-list-entries' function (see below).
 - Maybe set `tabulated-list-printer' (see below).
 - Maybe set `tabulated-list-padding'.
 - Call `tabulated-list-init-header' to initialize `header-line-format'
   according to `tabulated-list-format'.

An inheriting mode is usually accompanied by a \"list-FOO\"
command (e.g. `list-packages', `list-processes').  This command
creates or switches to a buffer and enables the major mode in
that buffer.  If `tabulated-list-entries' is not a function, the
command should initialize it to a list of entries for displaying.
Finally, it should call `tabulated-list-print'.

`tabulated-list-print' calls the printer function specified by
`tabulated-list-printer', once for each entry.  The default
printer is `tabulated-list-print-entry', but a mode that keeps
data in an ewoc may instead specify a printer function (e.g., one
that calls `ewoc-enter-last'), with `tabulated-list-print-entry'
as the ewoc pretty-printer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("tabulated-list-pkg.el") (20729 13828
;;;;;;  365742))

;;;***

(provide 'tabulated-list-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tabulated-list-autoloads.el ends here
