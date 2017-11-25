;;; flycheck-tip-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "error-tip" "error-tip.el" (23064 61675 135234
;;;;;;  396000))
;;; Generated autoloads from error-tip.el

(autoload 'error-tip-error-p "error-tip" "\
Return non-nil if error is occurred in current buffer.
This function can catch error against flycheck, and flymake.

\(fn)" nil nil)

(autoload 'error-tip-cycle-dwim "error-tip" "\
Showing error function.
This function switches proper error showing function by context.
 (whether flycheck or flymake) The REVERSE option jumps by inverse if
the value is non-nil.

\(fn &optional REVERSE)" t nil)

(autoload 'error-tip-cycle-dwim-reverse "error-tip" "\
Same as ‘error-tip-cycle-dwim’, but it jumps to inverse direction.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "flycheck-tip" "flycheck-tip.el" (23064 61675
;;;;;;  131234 407000))
;;; Generated autoloads from flycheck-tip.el

(autoload 'flycheck-tip-cycle "flycheck-tip" "\
Move to next error if it's exists.
If it wasn't exists then move to previous error.
Move to previous error if REVERSE is non-nil.

\(fn &optional REVERSE)" t nil)

(autoload 'flycheck-tip-cycle-reverse "flycheck-tip" "\
Do `flycheck-tip-cycle by reverse order.

\(fn)" t nil)

(autoload 'flycheck-tip--get "flycheck-tip" "\


\(fn ELEMENT ERR)" nil nil)

;;;***

;;;### (autoloads nil "flymake-tip" "flymake-tip.el" (23064 61675
;;;;;;  131234 407000))
;;; Generated autoloads from flymake-tip.el

(autoload 'flymake-tip-cycle "flymake-tip" "\


\(fn REVERSE)" t nil)

(autoload 'flymake-tip-cycle-reverse "flymake-tip" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("flycheck-tip-pkg.el") (23064 61675 135234
;;;;;;  396000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; flycheck-tip-autoloads.el ends here
