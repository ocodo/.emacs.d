;;; alchemist-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "alchemist" "alchemist.el" (0 0 0 0))
;;; Generated autoloads from alchemist.el

(autoload 'alchemist-mode "alchemist" "\
Toggle alchemist mode.

Key bindings:
\\{alchemist-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist" '("alchemist-")))

;;;***

;;;### (autoloads nil "alchemist-company" "alchemist-company.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from alchemist-company.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-company" '("alchemist-company")))

;;;***

;;;### (autoloads nil "alchemist-compile" "alchemist-compile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from alchemist-compile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-compile" '("alchemist-compile")))

;;;***

;;;### (autoloads nil "alchemist-complete" "alchemist-complete.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from alchemist-complete.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-complete" '("alchemist-comp")))

;;;***

;;;### (autoloads nil "alchemist-eval" "alchemist-eval.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from alchemist-eval.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-eval" '("alchemist-eval-")))

;;;***

;;;### (autoloads nil "alchemist-execute" "alchemist-execute.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from alchemist-execute.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-execute" '("alchemist-execute")))

;;;***

;;;### (autoloads nil "alchemist-file" "alchemist-file.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from alchemist-file.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-file" '("alchemist-file-")))

;;;***

;;;### (autoloads nil "alchemist-goto" "alchemist-goto.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from alchemist-goto.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-goto" '("alchemist-goto-")))

;;;***

;;;### (autoloads nil "alchemist-help" "alchemist-help.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from alchemist-help.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-help" '("alchemist-help")))

;;;***

;;;### (autoloads nil "alchemist-hex" "alchemist-hex.el" (0 0 0 0))
;;; Generated autoloads from alchemist-hex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-hex" '("alchemist-hex")))

;;;***

;;;### (autoloads nil "alchemist-hooks" "alchemist-hooks.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from alchemist-hooks.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-hooks" '("alchemist-hooks-")))

;;;***

;;;### (autoloads nil "alchemist-iex" "alchemist-iex.el" (0 0 0 0))
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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-iex" '("inferior-elixir" "alchemist-iex-")))

;;;***

;;;### (autoloads nil "alchemist-info" "alchemist-info.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from alchemist-info.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-info" '("alchemist-info-")))

;;;***

;;;### (autoloads nil "alchemist-interact" "alchemist-interact.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from alchemist-interact.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-interact" '("alchemist-interact-")))

;;;***

;;;### (autoloads nil "alchemist-key" "alchemist-key.el" (0 0 0 0))
;;; Generated autoloads from alchemist-key.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-key" '("alchemist-key-command-prefix")))

;;;***

;;;### (autoloads nil "alchemist-macroexpand" "alchemist-macroexpand.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from alchemist-macroexpand.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-macroexpand" '("alchemist-macroexpand-")))

;;;***

;;;### (autoloads nil "alchemist-message" "alchemist-message.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from alchemist-message.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-message" '("alchemist-message")))

;;;***

;;;### (autoloads nil "alchemist-mix" "alchemist-mix.el" (0 0 0 0))
;;; Generated autoloads from alchemist-mix.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-mix" '("alchemist-")))

;;;***

;;;### (autoloads nil "alchemist-phoenix" "alchemist-phoenix.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from alchemist-phoenix.el

(autoload 'alchemist-phoenix-project-p "alchemist-phoenix" "\
Return non-nil if `default-directory' is inside a Phoenix project.

\(fn)" nil nil)

(autoload 'alchemist-phoenix-mode "alchemist-phoenix" "\
Minor mode for Elixir Phoenix web framework projects.

The following commands are available:

\\{alchemist-phoenix-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'alchemist-phoenix-enable-mode "alchemist-phoenix" "\


\(fn)" nil nil)

(dolist (hook '(alchemist-mode-hook)) (add-hook hook 'alchemist-phoenix-enable-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-phoenix" '("alchemist-phoenix-")))

;;;***

;;;### (autoloads nil "alchemist-project" "alchemist-project.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from alchemist-project.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-project" '("alchemist-project-")))

;;;***

;;;### (autoloads nil "alchemist-refcard" "alchemist-refcard.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from alchemist-refcard.el

(autoload 'alchemist-refcard "alchemist-refcard" "\
Generate an Alchemist refcard of all the features.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-refcard" '("alchemist-refcard-")))

;;;***

;;;### (autoloads nil "alchemist-report" "alchemist-report.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from alchemist-report.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-report" '("alchemist-report-")))

;;;***

;;;### (autoloads nil "alchemist-scope" "alchemist-scope.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from alchemist-scope.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-scope" '("alchemist-scope-")))

;;;***

;;;### (autoloads nil "alchemist-server" "alchemist-server.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from alchemist-server.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-server" '("alchemist-server")))

;;;***

;;;### (autoloads nil "alchemist-test-mode" "alchemist-test-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from alchemist-test-mode.el

(autoload 'alchemist-test-mode "alchemist-test-mode" "\
Minor mode for Elixir ExUnit files.

The following commands are available:

\\{alchemist-test-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'alchemist-test-enable-mode "alchemist-test-mode" "\


\(fn)" nil nil)

(dolist (hook '(alchemist-mode-hook)) (add-hook hook 'alchemist-test-enable-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-test-mode" '("alchemist-test")))

;;;***

;;;### (autoloads nil "alchemist-utils" "alchemist-utils.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from alchemist-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alchemist-utils" '("alchemist-utils-")))

;;;***

;;;### (autoloads nil nil ("alchemist-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; alchemist-autoloads.el ends here
