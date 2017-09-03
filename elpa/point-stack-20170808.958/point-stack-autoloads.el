;;; point-stack-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "point-stack" "point-stack.el" (22955 53182
;;;;;;  109298 773000))
;;; Generated autoloads from point-stack.el

(autoload 'point-stack-push "point-stack" "\
Push current buffer, point, and window scroll position onto the stack.

\(fn)" t nil)

(autoload 'point-stack-pop "point-stack" "\
Push current location on forward stack, move to previous location.

\(fn)" t nil)

(autoload 'point-stack-forward-stack-pop "point-stack" "\
Push current location on stack, pop and move to location from forward stack.

\(fn)" t nil)

(autoload 'point-stack-setup-advices "point-stack" "\
Advise navigation functions to call `point-stack-push' before
any navigation is made. This way, it can be used as a replacement
for the global mark ring.

\(fn)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; point-stack-autoloads.el ends here
