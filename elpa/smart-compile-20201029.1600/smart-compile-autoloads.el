;;; smart-compile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "smart-compile" "smart-compile.el" (0 0 0 0))
;;; Generated autoloads from smart-compile.el

(autoload 'smart-compile "smart-compile" "\
An interface to `compile'.
It calls `compile' or other compile function,
which is defined in `smart-compile-alist'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-compile" '("smart-compile-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smart-compile-autoloads.el ends here
