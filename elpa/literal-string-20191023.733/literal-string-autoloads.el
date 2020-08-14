;;; literal-string-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "literal-string" "literal-string.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from literal-string.el

(autoload 'literal-string-edit-string "literal-string" "\
Edit current string literal in a separate buffer.

Uses `edit-indirect-mode`.  Use `edit-indirect-commit` to end
editing.

After committing the editing buffer, quotes and escape sequences
are correctly escaped and indentation of multi-line strings is
reinserted.

When a string was a single line (or blank) and is converted to a
multi-line paragraph, following lines are indented by the amount
specified by `literal-string-get-default-indent-level`.

Multi-line strings keep their earlier indentation level unless
`literal-string-force-indent` is set." t nil)

(autoload 'literal-string-mode "literal-string" "\
A minor mode for editing literal (documentation) strings in
source code.

If called interactively, enable Literal-String mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Provides support for editing strings, automatic (un)escaping of
quotes and docstring indentation.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "literal-string" '("literal-string-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; literal-string-autoloads.el ends here
