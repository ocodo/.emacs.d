;;; evil-snipe-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "evil-snipe" "evil-snipe.el" (22030 34897 357140
;;;;;;  0))
;;; Generated autoloads from evil-snipe.el

(defvar evil-snipe-mode nil "\
Non-nil if Evil-Snipe mode is enabled.
See the command `evil-snipe-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `evil-snipe-mode'.")

(custom-autoload 'evil-snipe-mode "evil-snipe" nil)

(autoload 'evil-snipe-mode "evil-snipe" "\
evil-snipe minor mode.

\(fn &optional ARG)" t nil)

(defvar evil-snipe-override-mode nil "\
Non-nil if Evil-Snipe-Override mode is enabled.
See the command `evil-snipe-override-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `evil-snipe-override-mode'.")

(custom-autoload 'evil-snipe-override-mode "evil-snipe" nil)

(autoload 'evil-snipe-override-mode "evil-snipe" "\
evil-snipe minor mode that overrides evil-mode f/F/t/T/;/, bindings.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-snipe-mode "evil-snipe" "\
Enable evil-snipe-mode in the current buffer.

\(fn &optional INTERNAL)" nil nil)

(autoload 'turn-off-evil-snipe-mode "evil-snipe" "\
Disable evil-snipe-mode in the current buffer.

\(fn &optional INTERNAL)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-snipe-autoloads.el ends here
