;;; swift-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (swift-mode swift-mode-run-repl) "swift-mode" "swift-mode.el"
;;;;;;  (21406 22985 0 0))
;;; Generated autoloads from swift-mode.el

(autoload 'swift-mode-run-repl "swift-mode" "\
Run a REPL process, input and output via buffer `*swift-repl*'.
If there is a process already running in `*swift-repl*', switch to that buffer.
With argument CMD allows you to edit the command line (default is value
of `swift-repl-executable').
With DONT-SWITCH-P cursor will stay in current buffer.
Runs the hook `swift-repl-mode-hook' (after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)

\(fn CMD &optional DONT-SWITCH-P)" t nil)

(autoload 'swift-mode "swift-mode" "\
Major mode for Apple's Swift programming language.

\\<swift-mode-map>

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))

;;;***

;;;### (autoloads nil nil ("swift-mode-pkg.el") (21406 22985 676404
;;;;;;  0))

;;;***

(provide 'swift-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; swift-mode-autoloads.el ends here
