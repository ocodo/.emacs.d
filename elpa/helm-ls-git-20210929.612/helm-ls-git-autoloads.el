;;; helm-ls-git-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-ls-git" "helm-ls-git.el" (0 0 0 0))
;;; Generated autoloads from helm-ls-git.el

(add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG$" . helm-ls-git-commit-mode))

(autoload 'helm-ls-git-commit-mode "helm-ls-git" "\
Mode to edit COMMIT_EDITMSG files.

Commands:
\\{helm-ls-git-commit-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("/git-rebase-todo$" . helm-ls-git-rebase-todo-mode))

(autoload 'helm-ls-git-rebase-todo-mode "helm-ls-git" "\
Major Mode to edit git-rebase-todo files when using git rebase -i.

Commands:
\\{helm-ls-git-rebase-todo-mode-map}

\(fn)" t nil)

(autoload 'helm-ls-git "helm-ls-git" "\


\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-ls-git" '("helm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-ls-git-autoloads.el ends here
