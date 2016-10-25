;;; annoying-arrows-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "annoying-arrows-mode" "annoying-arrows-mode.el"
;;;;;;  (22541 58023 0 0))
;;; Generated autoloads from annoying-arrows-mode.el

(autoload 'annoying-arrows-mode "annoying-arrows-mode" "\
Annoying-Arrows emacs minor mode.

\(fn &optional ARG)" t nil)

(defvar global-annoying-arrows-mode nil "\
Non-nil if Global Annoying-Arrows mode is enabled.
See the `global-annoying-arrows-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-annoying-arrows-mode'.")

(custom-autoload 'global-annoying-arrows-mode "annoying-arrows-mode" nil)

(autoload 'global-annoying-arrows-mode "annoying-arrows-mode" "\
Toggle Annoying-Arrows mode in all buffers.
With prefix ARG, enable Global Annoying-Arrows mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Annoying-Arrows mode is enabled in all buffers where
`annoying-arrows-mode' would do it.
See `annoying-arrows-mode' for more information on Annoying-Arrows mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; annoying-arrows-mode-autoloads.el ends here
