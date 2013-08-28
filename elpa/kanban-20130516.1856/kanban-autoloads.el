;;; kanban-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (kanban-todo kanban-zero kanban-headers) "kanban"
;;;;;;  "kanban.el" (21020 2151 0 0))
;;; Generated autoloads from kanban.el

(autoload 'kanban-headers "kanban" "\
Fill the headers of your table with your org-mode TODO
states. If the table is too narrow, the only the first n TODO
states will be shown, with n as the number of columns in your
table.

\(fn COLUMN)" nil nil)

(autoload 'kanban-zero "kanban" "\
Zero-state Kanban board: This Kanban board just displays all
org-mode headers which have a TODO state in their respective TODO
state. Useful for getting a simple overview of your tasks.

Gets the COLUMN and ROW via TBLFM ($# and @#) and can get a string as MATCH to select only entries with a matching tag, as well as a list of org-mode files as the SCOPE to search for tasks.

\(fn COLUMN ROW &optional MATCH SCOPE)" nil nil)

(autoload 'kanban-todo "kanban" "\
Kanban TODO item grabber. Fills the first row of the kanban
table with org-mode TODO entries, if they are not in another cell
of the table. This allows you to set the state manually and just
use org-mode to supply new TODO entries.

Gets the COLUMN and all other CELS via TBLFM ($# and @2$2..@>$>) and can get a string as MATCH to select only entries with a matching tag, as well as a list of org-mode files as the SCOPE to search for tasks.

\(fn COLUMN CELS &optional MATCH SCOPE)" nil nil)

;;;***

;;;### (autoloads nil nil ("kanban-pkg.el") (21020 2151 305762 0))

;;;***

(provide 'kanban-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; kanban-autoloads.el ends here
