;;; loccur-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "loccur" "loccur.el" (0 0 0 0))
;;; Generated autoloads from loccur.el

(autoload 'loccur-mode "loccur" "\
Minor mode for navigating through the file.
Hides all lines without matches like `occur' does, but without opening
a new window.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "loccur" '("loccur")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loccur-autoloads.el ends here
