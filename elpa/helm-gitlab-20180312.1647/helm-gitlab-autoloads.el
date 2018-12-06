;;; helm-gitlab-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-gitlab" "helm-gitlab.el" (0 0 0 0))
;;; Generated autoloads from helm-gitlab.el

(autoload 'helm-gitlab-projects "helm-gitlab" "\
List Gitlab projects using Helm interface.

\(fn)" t nil)

(autoload 'helm-gitlab-project-issues "helm-gitlab" "\
List Gitlab projects using Helm interface.

\(fn)" t nil)

(autoload 'helm-gitlab-issues "helm-gitlab" "\
List Gitlab issues using Helm interface.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-gitlab" '("helm-gitlab--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-gitlab-autoloads.el ends here
