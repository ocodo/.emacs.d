;;; magit-org-todos-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "magit-org-todos" "magit-org-todos.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from magit-org-todos.el

(autoload 'magit-org-todos-insert-org-todos "magit-org-todos" "\
Insert org todos from the local todo.org.

\(fn)" nil nil)

(autoload 'magit-org-todos-autoinsert "magit-org-todos" "\
Automatically insert todo section into magit status buffer.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-org-todos" '("magit-org-todos-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-org-todos-autoloads.el ends here
