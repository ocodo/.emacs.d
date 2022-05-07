;;; processing-snippets-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "processing-snippets" "processing-snippets.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from processing-snippets.el

(autoload 'processing-snippets-initialize "processing-snippets" nil nil nil)

(eval-after-load 'yasnippet '(processing-snippets-initialize))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "processing-snippets" '("processing-snippets-root")))

;;;***

;;;### (autoloads nil nil ("processing-snippets-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; processing-snippets-autoloads.el ends here
