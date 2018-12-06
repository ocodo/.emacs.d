;;; el-search-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "el-search" "el-search.el" (0 0 0 0))
;;; Generated autoloads from el-search.el

(autoload 'el-search-count-matches "el-search" "\
Like `count-matches' but accepting an el-search PATTERN instead of a regexp.

Unlike `count-matches' matches \"inside\" other matches also count.

\(fn PATTERN &optional RSTART REND INTERACTIVE)" t nil)

(autoload 'el-search-looking-at "el-search" "\
El-search version of `looking-at'.
Return non-nil when there is a match for PATTERN at point in the
current buffer.

With ALLOW-LEADING-WHITESPACE non-nil, the match may
be preceded by whitespace.

\(fn PATTERN &optional ALLOW-LEADING-WHITESPACE)" nil nil)

(autoload 'el-search-loop-over-bindings "el-search" "\


\(fn FUNCTION)" nil nil)

(autoload 'el-search-install-shift-bindings "el-search" "\


\(fn)" t nil)

(autoload 'el-search-install-bindings-under-prefix "el-search" "\


\(fn PREFIX-KEY)" nil nil)

(autoload 'el-search-pattern "el-search" "\
Start new or resume last elisp buffer search.

Search current buffer for expressions that are matched by
PATTERN.  When called from the current search's current search
buffer, continue that search from point.  Otherwise or when a new
PATTERN is given, start a new single-buffer search from point.

The minibuffer is put into `emacs-lisp-mode' for reading the
input pattern, and there are some special key bindings:
\\<el-search-read-expression-map>\\[newline] inserts a newline,
and <up> and <down> are unbound in the local map to let you move
the cursor vertically - see `el-search-read-expression-map' for
details.

PATTERN is an \"el-search\" pattern - which means, either a
`pcase' pattern or complying with one of the additional pattern
types defined with `el-search-defpattern'.

See `el-search-defined-patterns' for a list of defined patterns.

\(fn PATTERN)" t nil)

(function-put 'el-search-pattern 'interactive-only 'el-search-forward)

(autoload 'el-search-pattern-backward "el-search" "\
Search the current buffer backward for matches of PATTERN.
See the command `el-search-pattern' for more information.

\(fn PATTERN)" t nil)

(function-put 'el-search-pattern-backward 'interactive-only 'el-search-backward)

(autoload 'el-search-this-sexp "el-search" "\
Prepare to el-search the `sexp-at-point'.

Grab the `sexp-at-point' SEXP and prepare to el-search the
current buffer for other matches of 'SEXP.

Use the normal search commands to seize the search.

\(fn SEXP)" t nil)

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
`source-directory'.

\(fn PATTERN)" t nil)

(autoload 'el-search-load-path "el-search" "\
Search PATTERN in all elisp files in all directories in `load-path'.
nil elements in `load-path' (standing for `default-directory')
are ignored.

\(fn PATTERN)" t nil)

(autoload 'el-search-dired-marked-files "el-search" "\
El-search files and directories marked in dired.
With RECURSIVELY given (the prefix arg in an interactive call),
search directories recursively.

This function uses `el-search-stream-of-directory-files' to
compute a the file stream - see there for a description of
related user options.

\(fn PATTERN FILES &optional RECURSIVELY)" t nil)

(autoload 'el-search-ibuffer-marked-buffers "el-search" "\
El-search the buffers marked in *Ibuffer*.

\(fn PATTERN BUFFER-NAMES)" t nil)

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

When called directly after a search command, use the current
search to drive query-replace (like in isearch).  You get a
multi-buffer query-replace this way when the current search is
multi-buffer.  When not called after a search command,
query-replace all matches following point in the current buffer.

It is also possible to replace matches with an arbitrary number
of expressions (even with zero expressions, effectively deleting
matches) by using the \"splicing\" submode that can be toggled
from the prompt with \"s\".  When splicing mode is on (default
off), the replacement expression must evaluate to a list, and all
of the list's elements are inserted in order.

The optional argument TEXTUAL-TO is bound by the interactive form
to the text form of the replacement expression specified.  It is
consulted to construct the text form of each replacement.

\(fn FROM-PATTERN TO-EXPR &optional TEXTUAL-TO)" t nil)

(autoload 'el-search-search-from-isearch "el-search" "\
Switch to an el-search session from isearch.
Reuse already given input.

\(fn)" t nil)

(autoload 'el-search-search-backward-from-isearch "el-search" "\
Switch to `el-search-pattern-backward' from isearch.
Reuse already given input.

\(fn)" t nil)

(autoload 'el-search-replace-from-isearch "el-search" "\
Switch to `el-search-query-replace' from isearch.
Reuse already given input.

\(fn)" t nil)

(autoload 'el-search-occur-from-isearch "el-search" "\
Switch to `el-search-occur' from isearch.
Reuse already given input.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "el-search" '("el-search-" "copy-el-search-object")))

;;;***

;;;### (autoloads nil "el-search-x" "el-search-x.el" (0 0 0 0))
;;; Generated autoloads from el-search-x.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "el-search-x" '("el-search-")))

;;;***

;;;### (autoloads nil nil ("el-search-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; el-search-autoloads.el ends here
