;;; insert-shebang-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "insert-shebang" "insert-shebang.el" (23278
;;;;;;  48193 0 0))
;;; Generated autoloads from insert-shebang.el

(autoload 'insert-shebang "insert-shebang" "\
Insert shebang line automatically.
Calls function `insert-shebang-get-extension-and-insert'.  With argument as
`buffer-name'.

\(fn)" t nil)
(add-hook 'find-file-hook 'insert-shebang)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; insert-shebang-autoloads.el ends here
