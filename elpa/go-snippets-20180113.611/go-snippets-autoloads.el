;;; go-snippets-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "go-snippets" "go-snippets.el" (0 0 0 0))
;;; Generated autoloads from go-snippets.el

(autoload 'go-snippets-initialize "go-snippets" "\


\(fn)" nil nil)

(eval-after-load 'yasnippet '(go-snippets-initialize))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "go-snippets" '("go-snippets-dir")))

;;;***

;;;### (autoloads nil nil ("go-snippets-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; go-snippets-autoloads.el ends here
