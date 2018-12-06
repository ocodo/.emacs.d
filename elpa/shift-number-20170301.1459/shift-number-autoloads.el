;;; shift-number-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "shift-number" "shift-number.el" (0 0 0 0))
;;; Generated autoloads from shift-number.el

(autoload 'shift-number-up "shift-number" "\
Increase the number at point (or on the current line) by ARG.

\(fn &optional ARG)" t nil)

(autoload 'shift-number-down "shift-number" "\
Decrease the number at point (or on the current line) by ARG.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "shift-number" '("shift-number")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; shift-number-autoloads.el ends here
