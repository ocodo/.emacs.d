;;; gitignore-templates-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "gitignore-templates" "gitignore-templates.el"
;;;;;;  (23360 15307 932552 667000))
;;; Generated autoloads from gitignore-templates.el

(autoload 'gitignore-templates-insert "gitignore-templates" "\
Insert .gitignore template for NAME.

\(fn NAME)" t nil)

(autoload 'gitignore-templates-new-file "gitignore-templates" "\
Create a .gitignore file for NAME in DIRECTORY.
With a prefix argument prompt for a directory to use.
If DIRECTORY is nil, use `default-directory'.

\(fn NAME &optional DIRECTORY)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gitignore-templates-autoloads.el ends here
