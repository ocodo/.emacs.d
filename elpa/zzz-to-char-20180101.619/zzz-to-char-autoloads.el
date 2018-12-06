;;; zzz-to-char-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "zzz-to-char" "zzz-to-char.el" (0 0 0 0))
;;; Generated autoloads from zzz-to-char.el

(autoload 'zzz-to-char "zzz-to-char" "\
Kill text between the point and the character CHAR.

This command is similar to `zap-to-char', it kills target
character too.

\(fn CHAR)" t nil)

(autoload 'zzz-up-to-char "zzz-to-char" "\
Kill text between the point and the character CHAR.

This command is similar to `zap-up-to-char', it doesn't kill
target character.

\(fn CHAR)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "zzz-to-char" '("zzz-to-char-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; zzz-to-char-autoloads.el ends here
