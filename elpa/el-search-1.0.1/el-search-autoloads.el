;;; el-search-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "el-search" "el-search.el" (22533 41871 0 0))
;;; Generated autoloads from el-search.el

(autoload 'el-search-pattern "el-search" "\
Start new or resume last elisp buffer search.

Search current buffer for expressions that are matched by `pcase'
PATTERN.  Use `read' to transform buffer contents into
expressions.

When called from the current search's current search buffer,
continue that search from point.  Otherwise or when a new PATTERN
is given, start a new single-buffer search from point.

Use `emacs-lisp-mode' for reading the input pattern.  Some keys
in the minibuffer have a special binding: to make it possible to
edit multi line input, C-j inserts a newline, and up and down
move the cursor vertically - see `el-search-read-expression-map'
for details.


Additional `pcase' pattern types to be used with this command can
be defined with `el-search-defpattern'.

The following additional pattern types are currently defined:

\(fn PATTERN)" t nil)

(autoload 'el-search-buffers "el-search" "\
Search all live elisp buffers for PATTERN.

\(fn PATTERN)" t nil)

(autoload 'el-search-directory "el-search" "\
Search all elisp files in DIRECTORY for PATTERN.
With prefix arg RECURSIVELY non-nil, search subdirectories recursively.

\(fn PATTERN DIRECTORY &optional RECURSIVELY)" t nil)

(autoload 'el-search-emacs-elisp-sources "el-search" "\
Search Emacs elisp sources for PATTERN.
This command recursively searches all elisp files under
\(expand-file-name \"lisp/\" source-directory).

\(fn PATTERN)" t nil)

(autoload 'el-search-dired-marked-files "el-search" "\
el-search marked files and directories in dired.
With RECURSIVELY given (the prefix arg in an interactive call),
search directories recursively.

\(fn PATTERN &optional RECURSIVELY)" t nil)

(autoload 'el-search-query-replace "el-search" "\
Replace some matches of \"el-search\" pattern FROM-PATTERN.

TO-EXPR is an Elisp expression that is evaluated repeatedly for
each match with bindings created in FROM-PATTERN in effect to
produce a replacement expression.  Operate from point
to (point-max).

As each match is found, the user must type a character saying
what to do with it.  For directions, type ? at that time.

As an alternative to enter FROM-PATTERN and TO-EXPR separately,
you can also give an input of the form

   FROM-PATTERN -> TO-EXPR

\(\">\" and \"=>\" are also allowed as a separator) to the first
prompt and specify both expressions at once.  This format is also
used for history entries.

\(fn FROM-PATTERN TO-EXPR &optional TEXTUAL-TO)" t nil)

(autoload 'el-search-search-from-isearch "el-search" "\
Switch to an el-search session from isearch.
Reuse already given input.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("el-search-pkg.el" "el-search-x.el") (22533
;;;;;;  41871 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; el-search-autoloads.el ends here
