;;; wgrep-helm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "wgrep-helm" "wgrep-helm.el" (21335 7901 530176
;;;;;;  999000))
;;; Generated autoloads from wgrep-helm.el

(autoload 'wgrep-helm-setup "wgrep-helm" "\


\(fn)" nil nil)
(add-hook 'helm-grep-mode-hook 'wgrep-helm-setup)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; wgrep-helm-autoloads.el ends here
