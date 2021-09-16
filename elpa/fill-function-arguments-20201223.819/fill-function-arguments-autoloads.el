;;; fill-function-arguments-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fill-function-arguments" "fill-function-arguments.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from fill-function-arguments.el

(autoload 'fill-function-arguments-to-single-line "fill-function-arguments" "\
Convert current bracketed list to a single line." t nil)

(autoload 'fill-function-arguments-to-multi-line "fill-function-arguments" "\
Convert current bracketed list to one line per argument." t nil)

(autoload 'fill-function-arguments-dwim "fill-function-arguments" "\
Fill the thing at point in a context-sensitive way.

If point is a string or comment and
`fill-function-arguments-fall-through-to-fill-paragraph' is
enabled, then just run `fill-paragragh'.

Otherwise if point is inside a bracketed list (e.g. a function
call, an array declaration, etc.) then if the list is currently
on a single line call `fill-function-arguments-to-multi-line',
otherwise call `fill-function-arguments-to-single-line'." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fill-function-arguments" '("fill-function-arguments-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fill-function-arguments-autoloads.el ends here
