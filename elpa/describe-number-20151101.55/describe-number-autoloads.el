;;; describe-number-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "describe-number" "describe-number.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from describe-number.el

(autoload 'describe-number "describe-number" "\
Describe information about VALUE, which can be a number or a string.

\(fn VALUE)" t nil)

(autoload 'describe-number-at-point "describe-number" "\
Describe number at point or region by using `describe-number'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "describe-number" '("describe-number--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; describe-number-autoloads.el ends here
