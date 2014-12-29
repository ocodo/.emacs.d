;;; evil-jumper-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "evil-jumper" "evil-jumper.el" (21664 42527
;;;;;;  263401 0))
;;; Generated autoloads from evil-jumper.el

(autoload 'evil-jumper-mode "evil-jumper" "\
Minor mode for vim jumplist emulation.

\(fn &optional ARG)" t nil)

(defvar global-evil-jumper-mode nil "\
Non-nil if Global-Evil-Jumper mode is enabled.
See the command `global-evil-jumper-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-jumper-mode'.")

(custom-autoload 'global-evil-jumper-mode "evil-jumper" nil)

(autoload 'global-evil-jumper-mode "evil-jumper" "\
Toggle Evil-Jumper mode in all buffers.
With prefix ARG, enable Global-Evil-Jumper mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Jumper mode is enabled in all buffers where
`turn-on-evil-jumper-mode' would do it.
See `evil-jumper-mode' for more information on Evil-Jumper mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-jumper-mode "evil-jumper" "\
Turns on vim jumplist emulation.

\(fn)" t nil)

(autoload 'turn-off-evil-jumper-mode "evil-jumper" "\
Turns off vim jumplist emulation.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-jumper-autoloads.el ends here
