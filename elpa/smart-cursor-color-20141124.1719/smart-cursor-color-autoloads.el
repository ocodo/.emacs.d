;;; smart-cursor-color-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "smart-cursor-color" "smart-cursor-color.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-cursor-color.el

(defvar smart-cursor-color-mode nil "\
Non-nil if Smart-Cursor-Color mode is enabled.
See the `smart-cursor-color-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smart-cursor-color-mode'.")

(custom-autoload 'smart-cursor-color-mode "smart-cursor-color" nil)

(autoload 'smart-cursor-color-mode "smart-cursor-color" "\
Dynamically changed cursor color at point's color.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-smart-cursor-color "smart-cursor-color" "\
Unconditionally turn on `smart-cursor-color-mode'.

\(fn)" t nil)

(autoload 'turn-off-smart-cursor-color "smart-cursor-color" "\
Unconditionally turn off `smart-cursor-color-mode'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-cursor-color" '("scc--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smart-cursor-color-autoloads.el ends here
