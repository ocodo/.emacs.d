;;; elixir-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "elixir-mode" "elixir-mode.el" (21487 3809
;;;;;;  886176 999000))
;;; Generated autoloads from elixir-mode.el

(autoload 'elixir-mode-iex "elixir-mode" "\
Elixir mode interactive REPL.
Optional argument ARGS-P .

\(fn &optional ARGS-P)" t nil)

(autoload 'elixir-mode-open-modegithub "elixir-mode" "\
Elixir mode open GitHub page.

\(fn)" t nil)

(autoload 'elixir-mode-open-elixir-home "elixir-mode" "\
Elixir mode go to language home.

\(fn)" t nil)

(autoload 'elixir-mode-open-docs-master "elixir-mode" "\
Elixir mode go to master documentation.

\(fn)" t nil)

(autoload 'elixir-mode-open-docs-stable "elixir-mode" "\
Elixir mode go to stable documentation.

\(fn)" t nil)

(autoload 'elixir-mode-show-version "elixir-mode" "\
Elixir mode print version.

\(fn)" t nil)

(autoload 'elixir-mode "elixir-mode" "\
Major mode for editing Elixir code.

\\{elixir-mode-map}

\(fn)" t nil)

(autoload 'elixir-mode-run-tests "elixir-mode" "\
Run ERT test for `elixir-mode'.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-mode))

(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))

(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))

;;;***

;;;### (autoloads nil nil ("elixir-mode-pkg.el" "elixir-smie.el"
;;;;;;  "ert-bootstrap.el") (21487 3809 895195 682000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; elixir-mode-autoloads.el ends here
