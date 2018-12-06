;;; go-add-tags-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "go-add-tags" "go-add-tags.el" (0 0 0 0))
;;; Generated autoloads from go-add-tags.el

(autoload 'go-add-tags "go-add-tags" "\
Add field tags for struct fields.

\(fn TAGS BEGIN END CONV-FN)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "go-add-tags" '("go-add-tags-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; go-add-tags-autoloads.el ends here
