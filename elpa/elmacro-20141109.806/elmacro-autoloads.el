;;; elmacro-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "elmacro" "elmacro.el" (22010 16145 497140
;;;;;;  0))
;;; Generated autoloads from elmacro.el

(autoload 'elmacro-show-last-macro "elmacro" "\
Show the last macro as elisp with NAME.

\(fn NAME)" t nil)

(autoload 'elmacro-show-last-commands "elmacro" "\
Take the latest COUNT commands and show them as elisp.

The default number of commands shown is 300. You can change this
number by using a numeric prefix argument or by using the
universal argument, in which case it'll ask for how many in the
minibuffer. See also `kmacro-edit-lossage'.

\(fn &optional COUNT)" t nil)

(autoload 'elmacro-clear-recorded-commands "elmacro" "\
Clear the list of recorded commands.

\(fn)" t nil)

(defvar elmacro-mode nil "\
Non-nil if elmacro mode is enabled.
See the command `elmacro-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `elmacro-mode'.")

(custom-autoload 'elmacro-mode "elmacro" nil)

(autoload 'elmacro-mode "elmacro" "\
Toggle emacs activity recording (elmacro mode).
With a prefix argument ARG, enable elmacro mode if ARG is
positive, and disable it otherwise. If called from Lisp, enable
the mode if ARG is omitted or nil.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; elmacro-autoloads.el ends here
