;;; showkey-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "showkey" "showkey.el" (0 0 0 0))
;;; Generated autoloads from showkey.el

(let ((loads (get 'Show-Key 'custom-loads))) (if (member '"showkey" loads) nil (put 'Show-Key 'custom-loads (cons '"showkey" loads))))

(defvar showkey-tooltip-mode nil "\
Non-nil if Showkey-Tooltip mode is enabled.
See the `showkey-tooltip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `showkey-tooltip-mode'.")

(custom-autoload 'showkey-tooltip-mode "showkey" nil)

(autoload 'showkey-tooltip-mode "showkey" "\
Global minor mode that logs the keys you use.
See option `showkey-tooltip-ignored-events' for customization.

Note that keys such as `C-g' that quit, and keys that raise an error,
are not logged.

\(fn &optional ARG)" t nil)

(defvar showkey-log-mode nil "\
Non-nil if Showkey-Log mode is enabled.
See the `showkey-log-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `showkey-log-mode'.")

(custom-autoload 'showkey-log-mode "showkey" nil)

(autoload 'showkey-log-mode "showkey" "\
Global minor mode that logs the keys you use.
See options `showkey-log-erase-keys', `showkey-log-ignored-events',
and `showkey-log-frame-alist' for customization.

Note that keys such as `C-g' that quit, and keys that raise an error,
are not logged.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "showkey" '("showkey-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; showkey-autoloads.el ends here
