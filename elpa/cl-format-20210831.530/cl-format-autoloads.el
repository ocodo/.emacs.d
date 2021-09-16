;;; cl-format-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cl-format" "cl-format.el" (0 0 0 0))
;;; Generated autoloads from cl-format.el

(autoload 'cl-format "cl-format" "\
Format FMT using ARGS and print it to STREAM.

The full documentation not available until this function is
loaded.

\(fn STREAM FMT &rest ARGS)" nil nil)

(autoload 'cl-formatter "cl-format" "\
Compile FMT into a function.

This macro parses and compiles FMT into a function, which may be
passed as format argument to `cl-format'.

\(fn FMT)" nil t)

(autoload 'cl-error "cl-format" "\
Like `error', but use CL format strings.

\(fn FMT &rest ARGS)" nil nil)

(autoload 'cl-message "cl-format" "\
Like `error', but use CL format strings.

\(fn FMT &rest ARGS)" nil nil)

(autoload 'cl-format-font-lock-mode "cl-format" "\
Adds font-lock support for cl format strings.

If called interactively, enable Cl-Format-Font-Lock mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cl-format" '("cl-format-")))

;;;***

;;;### (autoloads nil "cl-format-builtins" "cl-format-builtins.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from cl-format-builtins.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cl-format-builtins" '("?$" "?%" "?&" "?*" "?/" "?<" "??" "?\\[" "?^" "?_" "?a" "?b" "?c" "?d" "?e" "?f" "?g" "?i" "?o" "?p" "?r" "?s" "?t" "?w" "?x" "?{" "?~" "cl-format-" "single-key-description-forthcoming")))

;;;***

;;;### (autoloads nil "cl-format-def" "cl-format-def.el" (0 0 0 0))
;;; Generated autoloads from cl-format-def.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cl-format-def" '("cl-" "define-cl-format-directive")))

;;;***

;;;### (autoloads nil "clisp-format" "clisp-format.el" (0 0 0 0))
;;; Generated autoloads from clisp-format.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "clisp-format" '("clisp-")))

;;;***

;;;### (autoloads nil nil ("cl-format-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cl-format-autoloads.el ends here
