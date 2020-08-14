;;; gitignore-templates-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gitignore-templates" "gitignore-templates.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from gitignore-templates.el

(autoload 'gitignore-templates-insert "gitignore-templates" "\
Insert .gitignore template for NAME.

\(fn NAME)" t nil)

(autoload 'gitignore-templates-new-file "gitignore-templates" "\
Create a .gitignore file for NAME in DIRECTORY.
With a prefix argument prompt for a directory to use.
If DIRECTORY is nil, use `default-directory'.

\(fn NAME &optional DIRECTORY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gitignore-templates" '("gitignore-templates")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gitignore-templates-autoloads.el ends here
