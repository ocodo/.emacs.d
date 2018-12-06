;;; flycheck-ycmd-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck-ycmd" "flycheck-ycmd.el" (0 0 0 0))
;;; Generated autoloads from flycheck-ycmd.el

(autoload 'flycheck-ycmd-setup "flycheck-ycmd" "\
Convenience function to setup the ycmd flycheck checker.

This adds a hook to watch for ycmd parse results, and it adds the
ycmd checker to the list of flycheck checkers.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck-ycmd" '("flycheck-ycmd--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-ycmd-autoloads.el ends here
