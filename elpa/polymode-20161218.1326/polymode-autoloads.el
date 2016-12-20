;;; polymode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "poly-R" "poly-R.el" (22617 16736 321133 241000))
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

;;;***

;;;### (autoloads nil "poly-c" "poly-c.el" (22617 16736 233133 129000))
;;; Generated autoloads from poly-c.el
 (autoload 'poly-noweb+c-mode "poly-c")

;;;***

;;;### (autoloads nil "poly-erb" "poly-erb.el" (22617 16736 193133
;;;;;;  79000))
;;; Generated autoloads from poly-erb.el
 (autoload 'poly-coffee+erb-mode "poly-erb")
 (autoload 'poly-javascript+erb-mode "poly-erb")
 (autoload 'poly-html+erb-mode "poly-erb")

;;;***

;;;### (autoloads nil "poly-markdown" "poly-markdown.el" (22617 16736
;;;;;;  297133 210000))
;;; Generated autoloads from poly-markdown.el
 (autoload 'poly-markdown-mode "poly-markdown")

;;;***

;;;### (autoloads nil "poly-noweb" "poly-noweb.el" (22617 16736 201133
;;;;;;  89000))
;;; Generated autoloads from poly-noweb.el
 (autoload 'poly-noweb-mode "poly-noweb")

;;;***

;;;### (autoloads nil "poly-org" "poly-org.el" (22617 16736 305133
;;;;;;  220000))
;;; Generated autoloads from poly-org.el
 (autoload 'poly-org-mode "poly-org")

;;;***

;;;### (autoloads nil "poly-slim" "poly-slim.el" (22617 16736 285133
;;;;;;  195000))
;;; Generated autoloads from poly-slim.el
 (autoload 'poly-slim-mode "poly-slim")

;;;***

;;;### (autoloads nil "polymode" "polymode.el" (22617 16736 273133
;;;;;;  180000))
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

;;;***

;;;### (autoloads nil nil ("poly-base.el" "poly-lock.el" "polymode-classes.el"
;;;;;;  "polymode-compat.el" "polymode-configuration.el" "polymode-core.el"
;;;;;;  "polymode-debug.el" "polymode-export.el" "polymode-methods.el"
;;;;;;  "polymode-pkg.el" "polymode-tangle.el" "polymode-weave.el")
;;;;;;  (22617 16736 357133 286000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; polymode-autoloads.el ends here
