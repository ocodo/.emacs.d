;;; evil-space-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "evil-space" "evil-space.el" (21841 22688 335401
;;;;;;  0))
;;; Generated autoloads from evil-space.el

(autoload 'evil-space-setup "evil-space" "\
Setup `evil-space` for motion `key`

`SPC` and `S-SPC` are map to next and prev

\(fn KEY NEXT PREV)" nil t)

(defvar evil-space-mode nil "\
Non-nil if Evil-Space mode is enabled.
See the command `evil-space-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `evil-space-mode'.")

(custom-autoload 'evil-space-mode "evil-space" nil)

(autoload 'evil-space-mode "evil-space" "\
Evil space mode.

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'evil-space-default-setup 'evil-space-mode "0.0.4")

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-space-autoloads.el ends here
