;;; define-word-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "define-word" "define-word.el" (23166 23019
;;;;;;  196870 740000))
;;; Generated autoloads from define-word.el

(autoload 'define-word "define-word" "\
Define WORD using various services.

By default uses `define-word-default-service', but a prefix arg
lets the user choose service.

\(fn WORD SERVICE &optional CHOOSE-SERVICE)" t nil)

(autoload 'define-word-at-point "define-word" "\
Use `define-word' to define word at point.
When the region is active, define the marked phrase.
Prefix ARG lets you choose service.

In a non-interactive call SERVICE can be passed.

\(fn ARG &optional SERVICE)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; define-word-autoloads.el ends here
