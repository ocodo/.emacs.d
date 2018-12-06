;;; helm-cmd-t-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-C-x-b" "helm-C-x-b.el" (0 0 0 0))
;;; Generated autoloads from helm-C-x-b.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-C-x-b" '("helm-C-x-")))

;;;***

;;;### (autoloads nil "helm-cmd-t" "helm-cmd-t.el" (0 0 0 0))
;;; Generated autoloads from helm-cmd-t.el

(autoload 'helm-cmd-t "helm-cmd-t" "\
Choose file from current repo.

With prefix arg C-u, run `helm-cmd-t-repos'.

\(fn &optional ARG)" t nil)

(autoload 'helm-cmd-t-repos "helm-cmd-t" "\
Manage helm-cmd-t caches.

\(fn &optional PRESELECT-ROOT)" t nil)

(autoload 'helm-cmd-t-grep "helm-cmd-t" "\
Grep action run from `helm-cmd-t-repos'.

Redirect to the correct grep based on `candidate-buffer'.

\(fn CANDIDATE-BUFFER &optional GLOBS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-cmd-t" '("helm-")))

;;;***

;;;### (autoloads nil "helm-cmd-t-find" "helm-cmd-t-find.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from helm-cmd-t-find.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-cmd-t-find" '("helm-cmd-t-")))

;;;***

;;;### (autoloads nil nil ("helm-cmd-t-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-cmd-t-autoloads.el ends here
