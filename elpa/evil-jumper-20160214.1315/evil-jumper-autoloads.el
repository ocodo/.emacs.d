;;; evil-jumper-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "evil-jumper" "evil-jumper.el" (22218 26464
;;;;;;  846530 46000))
;;; Generated autoloads from evil-jumper.el

(defvar evil-jumper-mode nil "\
Non-nil if Evil-Jumper mode is enabled.
See the command `evil-jumper-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `evil-jumper-mode'.")

(custom-autoload 'evil-jumper-mode "evil-jumper" nil)

(autoload 'evil-jumper-mode "evil-jumper" "\
Global minor mode for vim jumplist emulation.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-jumper-mode "evil-jumper" "\
Turns on vim jumplist emulation.

\(fn)" t nil)

(autoload 'turn-off-evil-jumper-mode "evil-jumper" "\
Turns off vim jumplist emulation.

\(fn)" t nil)

(defalias 'global-evil-jumper-mode 'evil-jumper-mode)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-jumper-autoloads.el ends here
