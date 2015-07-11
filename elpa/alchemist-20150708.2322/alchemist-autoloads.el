;;; alchemist-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "alchemist" "alchemist.el" (21920 42687 199401
;;;;;;  0))
;;; Generated autoloads from alchemist.el

(autoload 'alchemist-version "alchemist" "\
Display Alchemist's version.

\(fn &optional SHOW-VERSION)" t nil)

(autoload 'alchemist-mode "alchemist" "\
Toggle alchemist mode.

Key bindings:
\\{alchemist-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "alchemist-iex" "alchemist-iex.el" (21920 42687
;;;;;;  247401 0))
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

;;;### (autoloads nil "alchemist-refcard" "alchemist-refcard.el"
;;;;;;  (21920 42687 207401 0))
;;; Generated autoloads from alchemist-refcard.el

(autoload 'alchemist-refcard "alchemist-refcard" "\
Generate an Alchemist refcard of all the features.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "alchemist-test-mode" "alchemist-test-mode.el"
;;;;;;  (21920 42687 263401 0))
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

;;;### (autoloads nil nil ("alchemist-buffer.el" "alchemist-company.el"
;;;;;;  "alchemist-compile.el" "alchemist-complete.el" "alchemist-eval.el"
;;;;;;  "alchemist-execute.el" "alchemist-goto.el" "alchemist-help.el"
;;;;;;  "alchemist-hooks.el" "alchemist-message.el" "alchemist-mix.el"
;;;;;;  "alchemist-pkg.el" "alchemist-project.el" "alchemist-server.el"
;;;;;;  "alchemist-utils.el") (21920 42687 278445 455000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; alchemist-autoloads.el ends here
