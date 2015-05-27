;;; evil-snipe-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "evil-snipe" "evil-snipe.el" (21861 60011 11401
;;;;;;  0))
;;; Generated autoloads from evil-snipe.el

(autoload 'evil-snipe-mode "evil-snipe" "\
evil-snipe minor mode.

\(fn &optional ARG)" t nil)

(autoload 'evil-snipe-override-mode "evil-snipe" "\
evil-snipe minor mode that overrides evil-mode f/F/t/T/;/, bindings.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-snipe-mode "evil-snipe" "\
Enable evil-snipe-mode in the current buffer.

\(fn &optional INTERNAL)" nil nil)

(autoload 'turn-off-evil-snipe-mode "evil-snipe" "\
Disable evil-snipe-mode in the current buffer.

\(fn &optional INTERNAL)" nil nil)

(defvar global-evil-snipe-mode nil "\
Non-nil if Global-Evil-Snipe mode is enabled.
See the command `global-evil-snipe-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-snipe-mode'.")

(custom-autoload 'global-evil-snipe-mode "evil-snipe" nil)

(autoload 'global-evil-snipe-mode "evil-snipe" "\
Toggle Evil-Snipe mode in all buffers.
With prefix ARG, enable Global-Evil-Snipe mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Snipe mode is enabled in all buffers where
`turn-on-evil-snipe-mode' would do it.
See `evil-snipe-mode' for more information on Evil-Snipe mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-snipe-autoloads.el ends here
