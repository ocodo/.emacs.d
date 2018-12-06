;;; flx-isearch-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flx-isearch" "flx-isearch.el" (0 0 0 0))
;;; Generated autoloads from flx-isearch.el

(autoload 'flx-isearch-mode "flx-isearch" "\


\(fn &optional ARG)" t nil)

(autoload 'flx-isearch-forward "flx-isearch" "\
Start a fuzzy forward isearch

\(fn &optional REGEXP-P NO-RECURSIVE-EDIT)" t nil)

(autoload 'flx-isearch-backward "flx-isearch" "\
Start a fuzzy backward isearch

\(fn &optional REGEXP-P NO-RECURSIVE-EDIT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flx-isearch" '("flx-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flx-isearch-autoloads.el ends here
