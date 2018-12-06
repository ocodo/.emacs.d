;;; go-rename-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "go-rename" "go-rename.el" (0 0 0 0))
;;; Generated autoloads from go-rename.el

(autoload 'go-rename "go-rename" "\
Rename the entity denoted by the identifier at point, using
the `gorename' tool. With FORCE, call `gorename' with the
`-force' flag.

\(fn NEW-NAME &optional FORCE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "go-rename" '("go-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; go-rename-autoloads.el ends here
