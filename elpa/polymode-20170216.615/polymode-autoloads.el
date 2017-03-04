;;; polymode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "poly-R" "poly-R.el" (0 0 0 0))
;;; Generated autoloads from poly-R.el
 (autoload 'poly-noweb+r-mode "poly-R")
 (autoload 'poly-markdown+r-mode "poly-R")
 (autoload 'poly-rapport-mode "poly-R")
 (autoload 'poly-html+r-mode "poly-R")
 (autoload 'poly-brew+r-mode "poly-R")
 (autoload 'poly-r+c++-mode "poly-R")
 (autoload 'poly-c++r-mode "poly-R")
 (autoload 'poly-ess-help+r-mode "poly-R")
 (autoload 'poly-Rd-mode "poly-R")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poly-R" '("poly-" "pm-")))

;;;***

;;;### (autoloads nil "poly-base" "poly-base.el" (0 0 0 0))
;;; Generated autoloads from poly-base.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poly-base" '("pm-")))

;;;***

;;;### (autoloads nil "poly-c" "poly-c.el" (0 0 0 0))
;;; Generated autoloads from poly-c.el
 (autoload 'poly-noweb+c-mode "poly-c")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poly-c" '("pm-" "poly-noweb+c-mode")))

;;;***

;;;### (autoloads nil "poly-erb" "poly-erb.el" (0 0 0 0))
;;; Generated autoloads from poly-erb.el
 (autoload 'poly-coffee+erb-mode "poly-erb")
 (autoload 'poly-javascript+erb-mode "poly-erb")
 (autoload 'poly-html+erb-mode "poly-erb")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poly-erb" '("pm-" "poly-")))

;;;***

;;;### (autoloads nil "poly-lock" "poly-lock.el" (0 0 0 0))
;;; Generated autoloads from poly-lock.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poly-lock" '("poly-lock-")))

;;;***

;;;### (autoloads nil "poly-markdown" "poly-markdown.el" (0 0 0 0))
;;; Generated autoloads from poly-markdown.el
 (autoload 'poly-markdown-mode "poly-markdown")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poly-markdown" '("pm-" "poly-markdown-")))

;;;***

;;;### (autoloads nil "poly-noweb" "poly-noweb.el" (0 0 0 0))
;;; Generated autoloads from poly-noweb.el
 (autoload 'poly-noweb-mode "poly-noweb")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poly-noweb" '("poly-noweb-" "pm-")))

;;;***

;;;### (autoloads nil "poly-org" "poly-org.el" (0 0 0 0))
;;; Generated autoloads from poly-org.el
 (autoload 'poly-org-mode "poly-org")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poly-org" '("pm-" "poly-org-mode")))

;;;***

;;;### (autoloads nil "poly-slim" "poly-slim.el" (0 0 0 0))
;;; Generated autoloads from poly-slim.el
 (autoload 'poly-slim-mode "poly-slim")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poly-slim" '("pm-" "poly-slim-mode")))

;;;***

;;;### (autoloads nil "polymode" "polymode.el" (0 0 0 0))
;;; Generated autoloads from polymode.el

(autoload 'define-polymode "polymode" "\
Define a new polymode MODE.
This macro defines command MODE and an indicator variable MODE
which becomes t when MODE is active and nil otherwise.

MODE command is similar to standard emacs major modes and it can
be used in `auto-mode-alist'. Standard hook MODE-hook is run at
the end of the initialization of each polymode buffer (both
indirect and base buffers). Additionally MODE-map is created
based on the CONFIG's :map slot and the value of the :keymap
argument; see below.

CONFIG is a name of a config object representing the mode.

MODE command can also be use as a minor mode. Current major mode
is not reinitialized if it coincides with the :mode slot of
CONFIG object or if the :mode slot is nil.

BODY contains code to be executed after the complete
  initialization of the polymode (`pm-initialize') and before
  running MODE-hook. Before the BODY code, you can write keyword
  arguments, i.e. alternating keywords and values.  The following
  special keywords are supported:

:lighter SPEC   Optional LIGHTER is displayed in the mode line when
                the mode is on.  If omitted, it defaults to
                the :lighter slot of CONFIG object.

:keymap MAP Same as the KEYMAP argument.

                If nil, a new MODE-map keymap is created what
                directly inherits from the keymap defined by
                the :map slot of CONFIG object. In most cases it
                is a simple map inheriting form
                `polymode-mode-map'. If t or an alist (of
                bindings suitable to be passed to
                `easy-mmode-define-keymap') a keymap MODE-MAP is
                build by mergin this alist with the :map
                specification of the CONFIG object. If a symbol,
                it should be a variable whose value is a
                keymap. No MODE-MAP is automatically created in
                the latter case and :map slot of the CONFIG
                object is ignored.

:after-hook     A single lisp form which is evaluated after the mode hooks
                have been run.  It should not be quoted.

\(fn MODE CONFIG &optional KEYMAP &rest BODY)" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "polymode" '("pm--kill-span" "poly")))

;;;***

;;;### (autoloads nil "polymode-classes" "polymode-classes.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from polymode-classes.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "polymode-classes" '("pm-")))

;;;***

;;;### (autoloads nil "polymode-compat" "polymode-compat.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from polymode-compat.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "polymode-compat" '("pm-" "polymode-switch-buffer-keep-evil-state-maybe" "*span*")))

;;;***

;;;### (autoloads nil "polymode-core" "polymode-core.el" (0 0 0 0))
;;; Generated autoloads from polymode-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "polymode-core" '("polymode-")))

;;;***

;;;### (autoloads nil "polymode-debug" "polymode-debug.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from polymode-debug.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "polymode-debug" '("pm-")))

;;;***

;;;### (autoloads nil "polymode-export" "polymode-export.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from polymode-export.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "polymode-export" '("polymode-" "pm-")))

;;;***

;;;### (autoloads nil "polymode-methods" "polymode-methods.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from polymode-methods.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "polymode-methods" '("pm-")))

;;;***

;;;### (autoloads nil "polymode-weave" "polymode-weave.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from polymode-weave.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "polymode-weave" '("pm-" "polymode-")))

;;;***

;;;### (autoloads nil nil ("polymode-configuration.el" "polymode-pkg.el"
;;;;;;  "polymode-tangle.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; polymode-autoloads.el ends here
