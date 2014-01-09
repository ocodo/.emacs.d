;;; go-errcheck-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (go-errcheck) "go-errcheck" "go-errcheck.el" (21196
;;;;;;  43997 0 0))
;;; Generated autoloads from go-errcheck.el

(autoload 'go-errcheck "go-errcheck" "\
Run errcheck on the current buffer's directory and display the
  output in a compilation buffer.

If ARG is non-nil, go-errcheck will query for the values of
IGNOREPKG and IGNORE which will override any defaults or file
local variables.

When called non-interactively, DIRECTORY, IGNOREPKG and IGNORE
can be specified as arguments.

\(fn DIRECTORY IGNOREPKG IGNORE)" t nil)

;;;***

;;;### (autoloads nil nil ("go-errcheck-pkg.el") (21196 43997 949789
;;;;;;  0))

;;;***

(provide 'go-errcheck-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; go-errcheck-autoloads.el ends here
