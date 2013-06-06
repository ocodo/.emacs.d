;;; git-commit-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (git-commit-mode) "git-commit-mode" "git-commit-mode.el"
;;;;;;  (20838 20819 0 0))
;;; Generated autoloads from git-commit-mode.el

(autoload 'git-commit-mode "git-commit-mode" "\
Major mode for editing git commit messages.

This mode helps with editing git commit messages both by
providing commands to do common tasks, and by highlighting the
basic structure of and errors in git commit messages.

\(fn)" t nil)

(eval-after-load 'magit #'(progn (define-derived-mode magit-log-edit-mode git-commit-mode "Magit Log Edit") (substitute-key-definition 'magit-log-edit-toggle-signoff 'git-commit-signoff magit-log-edit-mode-map) (substitute-key-definition 'magit-log-edit-commit 'git-commit-log-edit-commit magit-log-edit-mode-map)))

(setq auto-mode-alist (append auto-mode-alist '(("/COMMIT_EDITMSG\\'" . git-commit-mode) ("/NOTES_EDITMSG\\'" . git-commit-mode) ("/MERGE_MSG\\'" . git-commit-mode) ("/TAG_EDITMSG\\'" . git-commit-mode) ("/PULLREQ_EDITMSG\\'" . git-commit-mode))))

;;;***

;;;### (autoloads nil nil ("git-commit-mode-pkg.el") (20838 20819
;;;;;;  785451 0))

;;;***

(provide 'git-commit-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; git-commit-mode-autoloads.el ends here
