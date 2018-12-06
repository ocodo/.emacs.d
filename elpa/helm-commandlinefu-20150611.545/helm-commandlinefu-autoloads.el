;;; helm-commandlinefu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-commandlinefu" "helm-commandlinefu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from helm-commandlinefu.el

(autoload 'helm-commandlinefu-browse "helm-commandlinefu" "\
Browse the Commandlinefu.com archive, sort by votes.
If SORT-BY-DATE is non-nil, sort by date.

\(fn &optional SORT-BY-DATE)" t nil)

(autoload 'helm-commandlinefu-search "helm-commandlinefu" "\
Browse Commandlinefu.com, sort by votes.

\(fn)" t nil)

(autoload 'helm-commandlinefu-search-clf "helm-commandlinefu" "\
Helm interface for clf.
see URL `https://github.com/ncrocfer/clf'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-commandlinefu" '("helm-commandlinefu-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-commandlinefu-autoloads.el ends here
