;;; helm-git-grep-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (helm-git-grep-with-exclude-file-pattern helm-git-grep-at-point
;;;;;;  helm-git-grep helm-git-grep-help helm-git-grep-toggle-showing-trailing-leading-line
;;;;;;  helm-git-grep-toggle-ignore-case helm-git-grep-run-save-buffer
;;;;;;  helm-git-grep-run-elscreen-action helm-git-grep-run-other-frame-action
;;;;;;  helm-git-grep-run-other-window-action helm-git-grep-run-default-action
;;;;;;  helm-git-grep-run-persistent-action) "helm-git-grep" "helm-git-grep.el"
;;;;;;  (21241 58393 0 0))
;;; Generated autoloads from helm-git-grep.el

(autoload 'helm-git-grep-run-persistent-action "helm-git-grep" "\
Run grep persistent action from `helm-git-grep'.

\(fn)" t nil)

(autoload 'helm-git-grep-run-default-action "helm-git-grep" "\
Run grep default action from `helm-git-grep'.

\(fn)" t nil)

(autoload 'helm-git-grep-run-other-window-action "helm-git-grep" "\
Run grep goto other window action from `helm-git-grep'.

\(fn)" t nil)

(autoload 'helm-git-grep-run-other-frame-action "helm-git-grep" "\
Run grep goto other frame action from `helm-git-grep'.

\(fn)" t nil)

(autoload 'helm-git-grep-run-elscreen-action "helm-git-grep" "\
Run grep goto elscreen action from `helm-git-grep'.

\(fn)" t nil)

(autoload 'helm-git-grep-run-save-buffer "helm-git-grep" "\
Run grep save results action from `helm-git-grep'.

\(fn)" t nil)

(autoload 'helm-git-grep-toggle-ignore-case "helm-git-grep" "\
Toggle ignore case option for git grep command from `helm-git-grep'.

\(fn)" t nil)

(autoload 'helm-git-grep-toggle-showing-trailing-leading-line "helm-git-grep" "\
Toggle show leading and trailing lines option for git grep.

\(fn)" t nil)

(autoload 'helm-git-grep-help "helm-git-grep" "\
Help command for `helm-git-grep'.

\(fn)" t nil)

(defvar helm-git-grep-mode-line-string "\\<helm-git-grep-map>\\[helm-git-grep-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\\[helm-select-3rd-action]:NthAct \\[helm-toggle-suspend-update]:Tog.suspend" "\
String displayed in mode-line in `helm-git-grep'.")

(autoload 'helm-git-grep "helm-git-grep" "\
Helm git grep.

if submodules exists, grep submodules too.

\(fn)" t nil)

(autoload 'helm-git-grep-at-point "helm-git-grep" "\
Helm git grep with symbol at point.

if submodules exists, grep submodules too.

\(fn)" t nil)

(autoload 'helm-git-grep-with-exclude-file-pattern "helm-git-grep" "\
Helm git grep with exclude file pattern.

file pattern is iterpreted as an POSIX extended regular expression.

if submodules exists, don't grep submodules.

\(fn)" t nil)

(define-obsolete-function-alias 'helm-git-grep-from-here 'helm-git-grep-at-point "0.5")

;;;***

;;;### (autoloads nil nil ("helm-git-grep-pkg.el") (21241 58393 374559
;;;;;;  0))

;;;***

(provide 'helm-git-grep-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-git-grep-autoloads.el ends here
