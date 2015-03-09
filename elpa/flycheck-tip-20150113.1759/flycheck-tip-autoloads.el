;;; flycheck-tip-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "eclim-tip" "eclim-tip.el" (21757 46962 527401
;;;;;;  0))
;;; Generated autoloads from eclim-tip.el

(autoload 'eclim-tip-cycle "eclim-tip" "\


\(fn &optional REVERSE)" t nil)

(autoload 'eclim-tip-cycle-reverse "eclim-tip" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "flycheck-tip" "flycheck-tip.el" (21757 46962
;;;;;;  515401 0))
;;; Generated autoloads from flycheck-tip.el

(autoload 'flycheck-tip-cycle "flycheck-tip" "\
Move to next error if it's exists.
If it wasn't exists then move to previous error.
Move to previous error if REVERSE is non-nil.

\(fn &optional REVERSE)" t nil)

(autoload 'flycheck-tip-cycle-reverse "flycheck-tip" "\
Do `flycheck-tip-cycle by reverse order.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("error-tip.el" "flycheck-tip-pkg.el" "flymake-tip.el")
;;;;;;  (21757 46962 535491 82000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; flycheck-tip-autoloads.el ends here
