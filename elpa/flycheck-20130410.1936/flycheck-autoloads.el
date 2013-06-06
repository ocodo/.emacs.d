;;; flycheck-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (flycheck-info flycheck-def-option-var flycheck-def-config-file-var
;;;;;;  flycheck-declare-checker global-flycheck-mode flycheck-mode)
;;;;;;  "flycheck" "flycheck.el" (20838 20826 0 0))
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

Flycheck mode will not be enabled if `flycheck-may-enable-mode' returns false.

In `flycheck-mode' the buffer is automatically syntax-checked
using the first suitable syntax checker from `flycheck-checkers'.
Use `flycheck-select-checker' to select a checker for the current
buffer manually.

\\{flycheck-mode-map}

\(fn &optional ARG)" t nil)

(defvar global-flycheck-mode nil "\
Non-nil if Global-Flycheck mode is enabled.
See the command `global-flycheck-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-flycheck-mode'.")

(custom-autoload 'global-flycheck-mode "flycheck" nil)

(autoload 'global-flycheck-mode "flycheck" "\
Toggle Flycheck mode in all buffers.
With prefix ARG, enable Global-Flycheck mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Flycheck mode is enabled in all buffers where
`flycheck-mode-on-safe' would do it.
See `flycheck-mode' for more information on Flycheck mode.

\(fn &optional ARG)" t nil)

(autoload 'flycheck-declare-checker "flycheck" "\
Declare SYMBOL as syntax checker with DOCSTRING and PROPERTIES.

DOCSTRING provides documentation for the new checker.  Use
`flycheck-checker-documentation' to access the documentation
string of a checker, and `flycheck-describe-checker' to get help
about a checker.

The following PROPERTIES are understood:

`:command' A list with the executable (in `car') and the
arguments (in `cdr') of the syntax checker.  The executable is
checked for existence with `executable-find' before executing the
checker.  The arguments are substituted with
`flycheck-substitute-argument' before execution, see the
documentation of this function for a list of special tags allowed
in arguments.

`:error-patterns' A list of error patterns to parse the output of
the checker.  Each pattern is a list (REGEXP LEVEL).  REGEXP is a
regular expression that matches an error.  This regular
expression may contain match groups extracting specific
information about the error.  The 1st group is the file name, the
2nd group the line number, the 3rd group the column number and
the 4th group the error message.  A group is ignored if it did
not match or the match returned an empty string.  LEVEL is either
warning or error and determines the severity of the error message
parsed with the pattern.

`:error-parser' A function symbol to parse errors with.  The
function must accept three arguments OUTPUT CHECKER BUFFER, where
OUTPUT is the output as string and CHECKER the checker symbol
that was used to check BUFFER.  The function must return a list
of `flycheck-error' objects parsed from OUTPUT.

`:modes' A major mode symbol or a list thereof.  If present the
checker is only used in these modes.

`:predicate' An Emacs Lisp form.  If present the checker is only
used if the form evaluates to a non-nil result in the buffer to
check.

`:next-checkers' A list where each element is either a checker
symbol to run after this checker or a cons cell (PREDICATE
. CHECKER).  In the latter case, CHECKER is the checker symbol to
run, and the PREDICATE symbol specifies when to run the checker:
If PREDICATE is `no-errors' run the next checker only if this
checker returned no errors at all.  If PREDICATE is
`warnings-only', run the next checker only if this checker
returned only warnings.  Only the first usable and
registered (see `flycheck-registered-checker-p') is run.

A checker must have a `:command' property, either
`:error-patterns' or `:error-parser' (but not both), and at least
one of `:predicate' and `:modes'.  If `:predicate' and `:modes'
are present, both must match for the checker to be used.

\(fn SYMBOL DOCSTRING &rest PROPERTIES)" nil t)

(put 'flycheck-declare-checker 'lisp-indent-function '1)

(put 'flycheck-declare-checker 'doc-string-elt '2)

(autoload 'flycheck-def-config-file-var "flycheck" "\
Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

SYMBOL is declared as customizable variable (see `defcustom`)
providing a configuration file for CHECKER.  The CHECKER argument
is used for documentation purposes only.  If given use FILE-NAME
as initial value.

Use this together with the `config-file' cell in syntax checker
commands.

\(fn SYMBOL CHECKER &optional FILE-NAME)" nil t)

(put 'flycheck-def-config-file-var 'lisp-indent-function '3)

(autoload 'flycheck-def-option-var "flycheck" "\
Define SYMBOL as option variable with INIT-VALUE for CHECKER.

INIT-VALUE is the initial value for the new variable.  DOCSTRING
is its docstring.

The variable is declared with `defcustom', and declared
buffer-local.  CUSTOM-ARGS are forwarded to `defcustom'.

Use this together with the `option' cell in syntax checker
commands.

\(fn SYMBOL INIT-VALUE CHECKER DOCSTRING &rest CUSTOM-ARGS)" nil t)

(put 'flycheck-def-option-var 'lisp-indent-function '3)

(put 'flycheck-def-option-var 'doc-string-elt '4)

(autoload 'flycheck-info "flycheck" "\
Open the Flycheck manual.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("flycheck-pkg.el") (20838 20826 412305
;;;;;;  0))

;;;***

(provide 'flycheck-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-autoloads.el ends here
