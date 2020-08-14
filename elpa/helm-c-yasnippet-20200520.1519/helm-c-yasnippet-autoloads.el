;;; helm-c-yasnippet-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-c-yasnippet" "helm-c-yasnippet.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from helm-c-yasnippet.el

(autoload 'helm-yas-complete "helm-c-yasnippet" "\
List of yasnippet snippets using `helm' interface." t nil)

(autoload 'helm-yas-visit-snippet-file "helm-c-yasnippet" "\
List of yasnippet snippet files" t nil)

(autoload 'helm-yas-create-snippet-on-region "helm-c-yasnippet" "\
Create a snippet from region.

\(fn &optional START END FILE-NAME)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-c-yasnippet" '("helm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-c-yasnippet-autoloads.el ends here
