;;; el-search-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "el-search" "el-search.el" (22507 60905 0 0))
;;; Generated autoloads from el-search.el

(autoload 'el-search-pattern "el-search" "\
Start new or resume last elisp search.

Search current buffer for expressions that are matched by `pcase'
PATTERN.  Use `read' to transform buffer contents into
expressions.

Use `emacs-lisp-mode' for reading input.  Some keys in the
minibuffer have a special binding: to make it possible to edit
multi line input, C-j inserts a newline, and up and down move the
cursor vertically - see `el-search-read-expression-map' for more
details.


Additional `pcase' pattern types to be used with this command can
be defined with `el-search-defpattern'.

The following additional pattern types are currently defined:

\(fn PATTERN &optional NO-ERROR)" t nil)

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

to the first prompt and specify both expressions at once.  This
format is also used for history entries.

\(fn FROM-PATTERN TO-EXPR &optional TEXTUAL-TO)" t nil)

(autoload 'el-search-search-from-isearch "el-search" "\


\(fn)" t nil)

(autoload 'el-search-replace-from-isearch "el-search" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("el-search-pkg.el" "el-search-x.el") (22507
;;;;;;  60905 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; el-search-autoloads.el ends here
