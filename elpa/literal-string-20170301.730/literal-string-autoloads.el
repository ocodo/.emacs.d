;;; literal-string-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "literal-string" "literal-string.el" (22919
;;;;;;  56158 0 0))
;;; Generated autoloads from literal-string.el

(autoload 'literal-string-edit-string "literal-string" "\
Indent current string literal.
Removes docstring indentation

\(fn)" t nil)

(autoload 'literal-string-mode "literal-string" "\
A minor mode for editing literal (documentation) strings in
source code.

Provides support for editing strings formatted in markdown,
automatic (un)escaping of quotes and docstring indentation.

\(fn &optional ARG)" t nil)

(autoload 'literal-string-editing-mode "literal-string" "\
Minor mode used in edit buffer by `literal-string-mode`.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; literal-string-autoloads.el ends here
