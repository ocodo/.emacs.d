;;; poly-markdown-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "poly-markdown" "poly-markdown.el" (0 0 0 0))
;;; Generated autoloads from poly-markdown.el
 (autoload 'poly-markdown-mode "poly-markdown")

(add-to-list 'auto-mode-alist '("\\.md\\'" . poly-markdown-mode))
 (autoload 'poly-gfm-mode "poly-markdown")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poly-markdown" '("poly-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; poly-markdown-autoloads.el ends here
