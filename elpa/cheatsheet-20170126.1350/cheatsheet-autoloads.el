;;; cheatsheet-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "cheatsheet" "cheatsheet.el" (0 0 0 0))
;;; Generated autoloads from cheatsheet.el

(autoload 'cheatsheet-add "cheatsheet" "\
Add CHEAT to cheatsheet.

\(fn &rest CHEAT)" nil nil)

(autoload 'cheatsheet-add-group "cheatsheet" "\
Add cheats to the same group.

\(fn GROUP &rest CHEATS)" nil nil)

(autoload 'cheatsheet-show "cheatsheet" "\
Create buffer and show cheatsheet.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cheatsheet" '("cheatsheet-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cheatsheet-autoloads.el ends here
