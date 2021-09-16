;;; keymap-utils-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "keymap-utils" "keymap-utils.el" (0 0 0 0))
;;; Generated autoloads from keymap-utils.el

(autoload 'kmu-current-local-mapvar "keymap-utils" "\
Return the variable bound to the current local keymap.
Interactively also show the variable in the echo area." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "keymap-utils" '("kmu-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; keymap-utils-autoloads.el ends here
