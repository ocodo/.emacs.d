;;; point-stack-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (point-stack-forward-stack-pop point-stack-pop
;;;;;;  point-stack-push) "point-stack" "point-stack.el" (21215 23678
;;;;;;  0 0))
;;; Generated autoloads from point-stack.el

(autoload 'point-stack-push "point-stack" "\
Push current buffer, point, and scroll position onto stack.

\(fn)" t nil)

(autoload 'point-stack-pop "point-stack" "\
Push current location onto forward stack, move to previous location.

\(fn)" t nil)

(autoload 'point-stack-forward-stack-pop "point-stack" "\
Push current location onto stack, pop and move to location from forward stack.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("point-stack-pkg.el") (21215 23678 343780
;;;;;;  0))

;;;***

(provide 'point-stack-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; point-stack-autoloads.el ends here
