;;; cl-format-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "cl-format" "cl-format.el" (22294 64741 0 0))
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

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("cl-format-builtins.el" "cl-format-def.el"
;;;;;;  "cl-format-pkg.el" "clisp-format.el") (22294 64741 379678
;;;;;;  0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cl-format-autoloads.el ends here
