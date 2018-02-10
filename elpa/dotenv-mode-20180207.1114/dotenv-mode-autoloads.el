;;; dotenv-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "dotenv-mode" "dotenv-mode.el" (23166 23026
;;;;;;  664861 683000))
;;; Generated autoloads from dotenv-mode.el

(autoload 'dotenv-mode "dotenv-mode" "\
Major mode for `.env' files.

\(fn)" t nil)

(mapc (lambda (s) (add-to-list 'auto-mode-alist `(,s . dotenv-mode))) '("\\.env\\'" "\\.env\\.example\\'"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dotenv-mode-autoloads.el ends here
