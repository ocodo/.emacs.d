;;; helm-git-grep-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-git-grep" "helm-git-grep.el" (0 0 0 0))
;;; Generated autoloads from helm-git-grep.el

(autoload 'helm-git-grep-ls-files-limited-by-pathspec "helm-git-grep" "\
Show result of `git ls-files' to check files limited by pathspec which is defined by `helm-git-grep-pathspecs'.

\(fn)" t nil)

(autoload 'helm-git-grep "helm-git-grep" "\
Helm git grep.

`helm-git-grep-sources' is used as helm sources.

\(fn)" t nil)

(autoload 'helm-git-grep-at-point "helm-git-grep" "\
Helm git grep with symbol at point.

Use region as input instead of the thing at point
if region exists.

`helm-git-grep-sources' is used as helm sources.

\(fn)" t nil)

(autoload 'helm-git-grep-from-isearch "helm-git-grep" "\
Invoke `helm-git-grep' from isearch.

\(fn)" t nil)

(autoload 'helm-git-grep-from-helm "helm-git-grep" "\
Invoke `helm-git-grep' from other helm.

\(fn)" t nil)

(defconst helm-git-grep-with-exclude-file-pattern-obsolete-message "use `helm-git-grep-pathspecs' to exclude files form search result.")

(autoload 'helm-git-grep-with-exclude-file-pattern "helm-git-grep" "\
Obsolete.

\(fn)" t nil)

(define-obsolete-function-alias 'helm-git-grep-from-here 'helm-git-grep-at-point "0.5")

(make-obsolete 'helm-git-grep-with-exclude-file-pattern helm-git-grep-with-exclude-file-pattern-obsolete-message "0.10.0")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-git-grep" '("helm-git-grep-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-git-grep-autoloads.el ends here
