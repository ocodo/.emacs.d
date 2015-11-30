;;; alchemist-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "alchemist" "alchemist.el" (22108 42487 585145
;;;;;;  414000))
;;; Generated autoloads from alchemist.el

(autoload 'alchemist-mode "alchemist" "\
Toggle alchemist mode.

Key bindings:
\\{alchemist-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "alchemist-iex" "alchemist-iex.el" (22108 42487
;;;;;;  613145 411000))
;;; Generated autoloads from alchemist-iex.el

(defalias 'run-elixir 'alchemist-iex-run)

(autoload 'alchemist-iex-run "alchemist-iex" "\
Start an IEx process.
Show the IEx buffer if an IEx process is already run.

\(fn &optional ARG)" t nil)

(autoload 'alchemist-iex-project-run "alchemist-iex" "\
Start an IEx process with mix 'iex -S mix' in the
context of an Elixir project.
Show the IEx buffer if an IEx process is already run.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "alchemist-phoenix" "alchemist-phoenix.el"
;;;;;;  (22108 42487 609145 411000))
;;; Generated autoloads from alchemist-phoenix.el

(autoload 'alchemist-phoenix-project-p "alchemist-phoenix" "\
Return non-nil if `default- is inside an Phoenix project.

\(fn)" nil nil)

(autoload 'alchemist-phoenix-mode "alchemist-phoenix" "\
Minor mode for Elixir Phoenix web framework projects.

The following commands are available:

\\{alchemist-phoenix-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'alchemist-phoenix-enable-mode "alchemist-phoenix" "\


\(fn)" nil nil)

(dolist (hook '(alchemist-mode-hook)) (add-hook hook 'alchemist-phoenix-enable-mode))

;;;***

;;;### (autoloads nil "alchemist-refcard" "alchemist-refcard.el"
;;;;;;  (22108 42487 593145 413000))
;;; Generated autoloads from alchemist-refcard.el

(autoload 'alchemist-refcard "alchemist-refcard" "\
Generate an Alchemist refcard of all the features.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "alchemist-test-mode" "alchemist-test-mode.el"
;;;;;;  (22108 42487 621145 410000))
;;; Generated autoloads from alchemist-test-mode.el

(autoload 'alchemist-test-mode "alchemist-test-mode" "\
Minor mode for Elixir ExUnit files.

The following commands are available:

\\{alchemist-test-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'alchemist-test-enable-mode "alchemist-test-mode" "\


\(fn)" nil nil)

(dolist (hook '(alchemist-mode-hook)) (add-hook hook 'alchemist-test-enable-mode))

;;;***

;;;### (autoloads nil nil ("alchemist-company.el" "alchemist-compile.el"
;;;;;;  "alchemist-complete.el" "alchemist-eval.el" "alchemist-execute.el"
;;;;;;  "alchemist-file.el" "alchemist-goto.el" "alchemist-help.el"
;;;;;;  "alchemist-hooks.el" "alchemist-interact.el" "alchemist-key.el"
;;;;;;  "alchemist-macroexpand.el" "alchemist-message.el" "alchemist-mix.el"
;;;;;;  "alchemist-pkg.el" "alchemist-project.el" "alchemist-report.el"
;;;;;;  "alchemist-scope.el" "alchemist-server.el" "alchemist-utils.el")
;;;;;;  (22108 42487 629577 346000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; alchemist-autoloads.el ends here
