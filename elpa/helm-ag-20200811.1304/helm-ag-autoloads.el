;;; helm-ag-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-ag" "helm-ag.el" (0 0 0 0))
;;; Generated autoloads from helm-ag.el

(autoload 'helm-ag-pop-stack "helm-ag" "\
Not documented." t nil)

(autoload 'helm-ag-clear-stack "helm-ag" "\
Not documented." t nil)

(autoload 'helm-ag-this-file "helm-ag" "\
Do ag with in this file with QUERY.

\(fn &optional QUERY)" t nil)

(autoload 'helm-ag "helm-ag" "\
Do ag with in BASEDIR and with QUERY.

\(fn &optional BASEDIR QUERY)" t nil)

(autoload 'helm-do-ag-this-file "helm-ag" "\
Not documented, QUERY.

\(fn &optional QUERY)" t nil)

(autoload 'helm-do-ag "helm-ag" "\
Not documented, BASEDIR, TARGETS, DEFAULT-INPUT.

\(fn &optional BASEDIR TARGETS DEFAULT-INPUT)" t nil)

(autoload 'helm-ag-project-root "helm-ag" "\
Not documented, QUERY.

\(fn &optional QUERY)" t nil)

(autoload 'helm-do-ag-project-root "helm-ag" "\
Not documented, QUERY.

\(fn &optional QUERY)" t nil)

(autoload 'helm-ag-buffers "helm-ag" "\
Not documented, QUERY.

\(fn &optional QUERY)" t nil)

(autoload 'helm-do-ag-buffers "helm-ag" "\
Not documented, QUERY.

\(fn &optional QUERY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-ag" '("helm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-ag-autoloads.el ends here
