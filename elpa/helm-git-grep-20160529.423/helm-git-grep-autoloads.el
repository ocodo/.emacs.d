;;; helm-git-grep-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "helm-git-grep" "helm-git-grep.el" (22347 49585
;;;;;;  297200 876000))
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

Use region as input instead of the thing at point
if region exists.

if submodules exists, grep submodules too.

\(fn)" t nil)

(autoload 'helm-git-grep-with-exclude-file-pattern "helm-git-grep" "\
Helm git grep with exclude file pattern.

file pattern is interpreted as an POSIX extended regular expression.

if submodules exists, don't grep submodules.

\(fn)" t nil)

(autoload 'helm-git-grep-from-isearch "helm-git-grep" "\
Invoke `helm-git-grep' from isearch.

\(fn)" t nil)

(autoload 'helm-git-grep-from-helm "helm-git-grep" "\
Invoke `helm-git-grep' from other helm.

\(fn)" t nil)

(define-obsolete-function-alias 'helm-git-grep-from-here 'helm-git-grep-at-point "0.5")

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; helm-git-grep-autoloads.el ends here
