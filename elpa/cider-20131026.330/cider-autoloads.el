;;; cider-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (cider cider-jack-in) "cider" "cider.el" (21100
;;;;;;  36417 0 0))
;;; Generated autoloads from cider.el

(autoload 'cider-jack-in "cider" "\
Start a nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server.

\(fn &optional PROMPT-PROJECT)" t nil)

(autoload 'cider "cider" "\
Connect to an nREPL server identified by HOST and PORT.

\(fn HOST PORT)" t nil)

(eval-after-load 'clojure-mode '(progn (define-key clojure-mode-map (kbd "C-c M-j") 'cider-jack-in) (define-key clojure-mode-map (kbd "C-c M-c") 'cider)))

;;;***

;;;### (autoloads (cider-disable-on-existing-clojure-buffers cider-enable-on-existing-clojure-buffers)
;;;;;;  "cider-interaction" "cider-interaction.el" (21100 36417 0
;;;;;;  0))
;;; Generated autoloads from cider-interaction.el

(autoload 'cider-enable-on-existing-clojure-buffers "cider-interaction" "\
Enable interaction mode on existing Clojure buffers.
See command `cider-mode'.

\(fn)" t nil)

(autoload 'cider-disable-on-existing-clojure-buffers "cider-interaction" "\
Disable interaction mode on existing Clojure buffers.
See command `cider-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads (cider-mode) "cider-mode" "cider-mode.el" (21100
;;;;;;  36417 0 0))
;;; Generated autoloads from cider-mode.el

(autoload 'cider-mode "cider-mode" "\
Minor mode for REPL interaction from a Clojure buffer.

\\{cider-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "nrepl-client" "nrepl-client.el" (21100 36417
;;;;;;  0 0))
;;; Generated autoloads from nrepl-client.el

(add-hook 'nrepl-connected-hook 'cider-enable-on-existing-clojure-buffers)

;;;***

;;;### (autoloads nil nil ("cider-eldoc.el" "cider-macroexpansion.el"
;;;;;;  "cider-pkg.el" "cider-repl-mode.el" "cider-repl.el" "cider-selector.el"
;;;;;;  "cider-util.el" "cider-version.el") (21100 36417 545983 0))

;;;***

(provide 'cider-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cider-autoloads.el ends here
