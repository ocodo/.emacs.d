;;; helm-google-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-google" "helm-google.el" (0 0 0 0))
;;; Generated autoloads from helm-google.el

(autoload 'helm-google "helm-google" "\
Web search interface for Emacs.

\(fn &optional ENGINE SEARCH-TERM)" t nil)

(autoload 'helm-google-searx "helm-google" "\
Explicitly use Searx for the web search.

\(fn &optional SEARCH-TERM)" t nil)

(autoload 'helm-google-google "helm-google" "\
Explicitly use Google for the web search.

\(fn &optional SEARCH-TERM)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-google" '("helm-google-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-google-autoloads.el ends here
