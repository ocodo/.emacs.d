;;; elmacro-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "elmacro" "elmacro.el" (21506 58444 679797
;;;;;;  0))
;;; Generated autoloads from elmacro.el

(autoload 'elmacro-show-last-macro "elmacro" "\
Show the last macro as elisp with NAME.

\(fn NAME)" t nil)

(autoload 'elmacro-show-lossage "elmacro" "\
Show lossage as elisp.

\(fn)" t nil)

(defvar elmacro-mode nil "\
Non-nil if elmacro mode is enabled.
See the command `elmacro-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `elmacro-mode'.")

(custom-autoload 'elmacro-mode "elmacro" nil)

(autoload 'elmacro-mode "elmacro" "\
Toggle elmacro mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; elmacro-autoloads.el ends here
