;;; emojify-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "emojify" "emojify.el" (22258 7001 0 0))
;;; Generated autoloads from emojify.el

(autoload 'emojify-set-emoji-styles "emojify" "\
Set the type of emojis that should be displayed.

STYLES is the styles emoji styles that should be used, see `emojify-emoji-styles'

\(fn STYLES)" nil nil)

(autoload 'emojify-mode "emojify" "\
Emojify mode

\(fn &optional ARG)" t nil)

(defvar global-emojify-mode nil "\
Non-nil if Global-Emojify mode is enabled.
See the command `global-emojify-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-emojify-mode'.")

(custom-autoload 'global-emojify-mode "emojify" nil)

(autoload 'global-emojify-mode "emojify" "\
Toggle Emojify mode in all buffers.
With prefix ARG, enable Global-Emojify mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Emojify mode is enabled in all buffers where
`emojify-mode' would do it.
See `emojify-mode' for more information on Emojify mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("emojify-pkg.el") (22258 7002 13905 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; emojify-autoloads.el ends here
