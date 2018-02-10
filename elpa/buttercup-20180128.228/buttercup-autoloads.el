;;; buttercup-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "buttercup" "buttercup.el" (23166 22973 960925
;;;;;;  931000))
;;; Generated autoloads from buttercup.el

(autoload 'buttercup-run-at-point "buttercup" "\
Run the buttercup suite at point.

\(fn)" t nil)

(autoload 'buttercup-run-discover "buttercup" "\
Discover and load test files, then run all defined suites.

Takes directories as command line arguments, defaulting to the
current directory.

\(fn)" nil nil)

(autoload 'buttercup-run-markdown "buttercup" "\
Run all test suites defined in Markdown files passed as arguments.
A suite must be defined within a Markdown \"lisp\" code block.

\(fn)" nil nil)

(autoload 'buttercup-minor-mode "buttercup" "\
Activate buttercup minor mode.

With buttercup minor mode active the following is activated:

- `describe' and `it' forms are fontified with
  `font-lock-keyword-face'.
- `describe' and `it' forms are available from `imenu' for
  quicker access.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("buttercup-compat.el" "buttercup-pkg.el")
;;;;;;  (23166 22973 964925 926000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; buttercup-autoloads.el ends here
