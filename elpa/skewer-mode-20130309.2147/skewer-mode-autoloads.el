;;; skewer-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (skewer-css-mode) "skewer-css" "skewer-css.el"
;;;;;;  (20798 31888))
;;; Generated autoloads from skewer-css.el

(autoload 'skewer-css-mode "skewer-css" "\
Minor mode for interactively loading new CSS rules.

\(fn &optional ARG)" t nil)

(add-hook 'css-mode-hook 'skewer-css-mode)

;;;***

;;;### (autoloads (run-skewer skewer-mode list-skewer-clients) "skewer-mode"
;;;;;;  "skewer-mode.el" (20798 31888))
;;; Generated autoloads from skewer-mode.el

(autoload 'list-skewer-clients "skewer-mode" "\
List the attached browsers in a buffer.

\(fn)" t nil)

(autoload 'skewer-mode "skewer-mode" "\
Minor mode for interacting with a browser.

\(fn &optional ARG)" t nil)

(add-hook 'js2-mode-hook 'skewer-mode)

(autoload 'run-skewer "skewer-mode" "\
Attach a browser to Emacs for a skewer JavaScript REPL. Uses
`browse-url' to launch a browser.

\(fn)" t nil)

;;;***

;;;### (autoloads (skewer-repl skewer-repl--response-hook) "skewer-repl"
;;;;;;  "skewer-repl.el" (20798 31888))
;;; Generated autoloads from skewer-repl.el

(autoload 'skewer-repl--response-hook "skewer-repl" "\
Catches all browser messages logging some to the REPL.

\(fn RESPONSE)" nil nil)

(autoload 'skewer-repl "skewer-repl" "\
Start a JavaScript REPL to be evaluated in the visiting browser.

\(fn)" t nil)

(eval-after-load 'skewer-mode '(progn (add-hook 'skewer-response-hook #'skewer-repl--response-hook) (define-key skewer-mode-map (kbd "C-c C-z") #'skewer-repl)))

;;;***

;;;### (autoloads nil nil ("cache-table.el" "skewer-mode-pkg.el")
;;;;;;  (20798 31888 728215))

;;;***

(provide 'skewer-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; skewer-mode-autoloads.el ends here
