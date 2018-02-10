;;; cl-print-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "cl-print" "cl-print.el" (23166 23035 548850
;;;;;;  913000))
;;; Generated autoloads from cl-print.el

(cl-defgeneric cl-print-object (object stream) "\
Dispatcher to print OBJECT on STREAM according to its type.
You can add methods to it to customize the output.
But if you just want to print something, don't call this directly:
call other entry points instead, such as `cl-prin1'." (prin1 object stream))

(autoload 'cl-prin1 "cl-print" "\


\(fn OBJECT &optional STREAM)" nil nil)

(autoload 'cl-prin1-to-string "cl-print" "\


\(fn OBJECT)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cl-print-autoloads.el ends here
