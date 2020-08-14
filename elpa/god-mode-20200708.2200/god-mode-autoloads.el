;;; god-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "god-mode" "god-mode.el" (0 0 0 0))
;;; Generated autoloads from god-mode.el

(autoload 'god-local-mode "god-mode" "\
Minor mode for running commands.

If called interactively, enable God-Local mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'god-mode "god-mode" "\
Toggle global `god-local-mode'." t nil)

(autoload 'god-mode-all "god-mode" "\
Toggle `god-local-mode' in all buffers." t nil)

(autoload 'god-mode-maybe-activate "god-mode" "\
Activate `god-local-mode' on individual buffers when appropriate.
STATUS is passed as an argument to `god-mode-activate'.

\(fn &optional STATUS)" nil nil)

(autoload 'god-execute-with-current-bindings "god-mode" "\
Execute a single command from God mode, preserving current keybindings.

This command activates God mode temporarily, and deactivates God
mode as soon as the next command is run.  Prefix arguments do not
count as commands for this purpose, and do not cause God mode to
exit.  Moreover, any prefix argument that exists at the time of
this command's invocation is passed along to the next command.

Unlike normal use of God mode, this command makes available all
keybindings that were active at the time of its invocation,
including keybindings that are normally invisible to God mode,
such as those in `emulation-mode-map-alists' or text overlay
properties.  This makes it suitable for use with packages like
Evil that utilize such higher-priority keymaps.  (See Info
node `(elisp)Active Keymaps' for technical details on keymap
precedence.  For an alternative to this command, check out the
evil-god-state package, available on MELPA.)

This command has no effect when called from within God mode.

For interactive use only.  CALLED-INTERACTIVELY is a dummy
parameter to help enforce this restriction.

\(fn &optional CALLED-INTERACTIVELY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "god-mode" '("god-")))

;;;***

;;;### (autoloads nil "god-mode-isearch" "god-mode-isearch.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from god-mode-isearch.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "god-mode-isearch" '("god-mode-isearch-")))

;;;***

;;;### (autoloads nil nil ("god-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; god-mode-autoloads.el ends here
