;;; org-transform-tree-table-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-transform-tree-table" "org-transform-tree-table.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-transform-tree-table.el

(autoload 'org-transform-tree/org-table-buffer-from-outline "org-transform-tree-table" "\
Transform an org tree to an org-table and return a new buffer
with the table.

If the region is active, convert that part of the
tree. Otherwise, if point is on an org heading, convert that
heading and its subtree. Otherwise convert the buffer.

In the resulting table, row one is the column titles. The rest of
the rows are property values.

Column one is the outline heading, and the rest are the
properties in the order they first appear in the buffer.

However, all special properties (e.g. 'COLUMNS', '*_ALL') are
placed after all the user properties (i.e. whatever properties
the user has added to capture information).

\(fn)" t nil)

(autoload 'org-transform-tree/csv-buffer-from-outline "org-transform-tree-table" "\
Transform an org tree to CSV format and return a new buffer
with the table.

Except it's not comma separated. It's tab separated because with
all (non) 'standard' ways to escape ',' in CSV files... let's not
even go there.

If the region is active, convert that part of the
tree. Otherwise, if point is on an org heading, convert that
heading and its subtree. Otherwise convert the buffer.

In the resulting table, row one is the column titles. The rest of
the rows are property values.

Column one is the outline heading, and the rest are the
properties in the order they first appear in the buffer.

However, all special properties (e.g. 'COLUMNS', '*_ALL') are
placed after all the user properties (i.e. whatever properties
the user has added to capture information).

\(fn)" t nil)

(autoload 'org-transform-table/org-tree-buffer-from-org-table "org-transform-tree-table" "\
Transform the org-table at point to an org-mode outline and
return a new buffer with the new tree.

Raise an error if point isn't on an org-table.

\(fn)" t nil)

(autoload 'org-transform-table/org-tree-buffer-from-csv "org-transform-tree-table" "\
Transform the buffer CSV table to an org-mode outline and
return a new buffer with the new tree.

\(fn)" t nil)

(autoload 'org-transform-tree-table/toggle "org-transform-tree-table" "\
Toggle between an outline subtree and an org-table, depending
on what point is placed on.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-transform-tree-table" '("ott/")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-transform-tree-table-autoloads.el ends here
