;;; ac-html-bootstrap-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ac-html-bootstrap" "ac-html-bootstrap.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ac-html-bootstrap.el

(autoload 'ac-html-bootstrap+ "ac-html-bootstrap" "\
Enable bootstrap ac-html completion" t nil)

(defalias 'company-web-bootstrap+ 'ac-html-bootstrap+)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ac-html-bootstrap" '("ac-html-bootstrap-source-dir")))

;;;***

;;;### (autoloads nil "ac-html-fa" "ac-html-fa.el" (0 0 0 0))
;;; Generated autoloads from ac-html-fa.el

(autoload 'ac-html-fa+ "ac-html-fa" "\
Enable Font Awesome completion for `ac-html' or `company-web'" t nil)

(defalias 'company-web-fa+ 'ac-html-fa+)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ac-html-fa" '("ac-html-fa-source-dir")))

;;;***

;;;### (autoloads nil nil ("ac-html-bootstrap-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ac-html-bootstrap-autoloads.el ends here
