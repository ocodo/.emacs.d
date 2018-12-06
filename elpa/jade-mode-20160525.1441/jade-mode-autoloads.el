;;; jade-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "jade-mode" "jade-mode.el" (0 0 0 0))
;;; Generated autoloads from jade-mode.el

(autoload 'jade-mode "jade-mode" "\
Major mode for editing jade node.js templates

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))

(add-to-list 'auto-mode-alist '("\\.pug\\'" . jade-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jade-mode" '("jade-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jade-mode-autoloads.el ends here
