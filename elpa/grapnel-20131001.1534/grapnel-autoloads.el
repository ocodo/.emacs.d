;;; grapnel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "grapnel" "grapnel.el" (0 0 0 0))
;;; Generated autoloads from grapnel.el

(autoload 'grapnel-retrieve-url "grapnel" "\
Retrieve URL asynchronously and call the corresponding handler in
HANDLER-ALIST.  See the documentation for `grapnel-callback-dispatch' for
details on HANDLER-ALIST.

REQUEST-METHOD: a string and can be any valid HTTP verb
URL-PARAMS: an alist and will be formatted into a query string and url encoded
REQUEST-DATA: an alist, automatically formatted and urlencoded, sent over stdin
REQUEST-HEADERS: an alist of header name to value pairs

\(fn URL HANDLER-ALIST &optional REQUEST-METHOD URL-PARAMS REQUEST-DATA REQUEST-HEADERS)" nil nil)

(autoload 'grapnel-retrieve-url-sync "grapnel" "\
Behaves the same as `grapnel-retrieve-url' but synchronously.

\(fn URL HANDLER-ALIST &optional REQUEST-METHOD URL-PARAMS REQUEST-DATA REQUEST-HEADERS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "grapnel" '("grapnel-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; grapnel-autoloads.el ends here
