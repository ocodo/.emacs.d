;;; gitconfig-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (gitconfig-mode) "gitconfig-mode" "gitconfig-mode.el"
;;;;;;  (21097 6319 0 0))
;;; Generated autoloads from gitconfig-mode.el

(autoload 'gitconfig-mode "gitconfig-mode" "\
A major mode for editing .gitconfig files.

\(fn)" t nil)

(dolist (pattern (list (rx "/.gitconfig" string-end) (rx "/.git/config" string-end) (rx "/git/config" string-end))) (add-to-list 'auto-mode-alist (cons pattern 'gitconfig-mode)))

;;;***

;;;### (autoloads nil nil ("gitconfig-mode-pkg.el") (21097 6319 912829
;;;;;;  0))

;;;***

(provide 'gitconfig-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gitconfig-mode-autoloads.el ends here
