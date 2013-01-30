;;; flycheck-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (flycheck-def-config-file-var flycheck-declare-checker
;;;;;;  flycheck-mode-off flycheck-mode-on flycheck-mode) "flycheck"
;;;;;;  "flycheck.el" (20733 54541))
;;; Generated autoloads from flycheck.el

(defconst flycheck-mode-line-lighter " FlyC" "\
The standard lighter for flycheck mode.")

(autoload 'flycheck-mode "flycheck" "\
Minor mode for on-the-fly syntax checking.

When called interactively, toggle `flycheck-mode'.  With prefix
ARG, enable `flycheck-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `flycheck-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `flycheck-mode'.
Otherwise behave as if called interactively.

In `flycheck-mode' the buffer is automatically syntax-checked
using the first suitable checker from `flycheck-checkers'.  Use
`flycheck-select-checker` to select a checker for the current
buffer manually.

\\{flycheck-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'flycheck-mode-on "flycheck" "\
Unconditionally enable variable `flycheck-mode'.

\(fn)" nil nil)

(autoload 'flycheck-mode-off "flycheck" "\
Unconditionally disable variable `flycheck-mode'.

\(fn)" nil nil)

(autoload 'flycheck-declare-checker "flycheck" "\
Declare SYMBOL as syntax checker with DOCSTRING and PROPERTIES.

DOCSTRING provides documentation for the new checker.  Use
`flycheck-checker-documentation' to access the documentation
string of a checker, and `flycheck-describe-checker' to get help
about a checker.

The following PROPERTIES are understood:

:command A list with the executable (in `car') and the
arguments (in `cdr') of the syntax checker.  The executable is
checked for existence with `executable-find' before executing the
checker.  The arguments are substituted with
`flycheck-substitute-argument' before execution, see the
documentation of this function for a list of special tags allowed
in arguments.

:error-patterns A list of error patterns to parse the output of
the checker.  Each pattern is a list (REGEXP LEVEL).  REGEXP is a
regular expression that matches an error.  This regular
expression may contain match groups extracting specific
information about the error.  The 1st group is the file name, the
2nd group the line number, the 3rd group the column number and
the 4th group the error message.  A group is ignored if it did
not match or the match returned an empty string.  LEVEL is either
warning or error and determines the severity of the error message
parsed with the pattern.

:modes A major mode symbol or a list thereof.  If present the
checker is only used in these modes.

:predicate An Emacs Lisp form.  If present the checker is only
used if the form evaluates to a non-nil result in the buffer to
check.

Either :modes or :predicate must be present.  If both are
present, both must match for the checker to be used.

\(fn SYMBOL DOCSTRING &rest PROPERTIES)" nil (quote macro))

(put 'flycheck-declare-checker 'doc-string-elt '2)

(put 'flycheck-declare-checker 'lisp-indent-function '1)

(autoload 'flycheck-def-config-file-var "flycheck" "\
Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

SYMBOL is declared as customizable variable (see `defcustom`)
providing a configuration file for CHECKER.  The CHECKER argument
is used for documentation purposes only.  If given use FILE-NAME
as initial value.

Use this together with the config tag in checker arguments.

\(fn SYMBOL CHECKER &optional FILE-NAME)" nil (quote macro))

(put 'flycheck-def-config-file-var 'lisp-indent-function '3)

;;;***

;;;### (autoloads nil nil ("dependencies.el" "flycheck-pkg.el") (20733
;;;;;;  54541 280449))

;;;***

(provide 'flycheck-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-autoloads.el ends here
