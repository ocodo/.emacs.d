;;; suggestion-box-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "suggestion-box" "suggestion-box.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from suggestion-box.el

(autoload 'suggestion-box-find-backend "suggestion-box" "\
Find backend available backend.  See also `suggestion-box-backend-functions'.

\(fn)" nil nil)

(autoload 'suggestion-box "suggestion-box" "\
Show convenience information on the cursor.
The argument STRING can be string or text propertied string.
See also `suggestion-box-h-embed-normalize' for more example.

\(fn STRING)" nil nil)

(autoload 'suggestion-box-put "suggestion-box" "\
Put text property to TEXT object.
You can use :backend, :handler, and :data keywords to add property.
See also `suggestion-box-h-embed-normalize' function for more example.

\(fn TEXT &key BACKEND HANDLER DATA)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "suggestion-box" '("suggestion-box-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; suggestion-box-autoloads.el ends here
