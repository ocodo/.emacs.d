;;; vimrc-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "vimrc-mode" "vimrc-mode.el" (22955 53095 557138
;;;;;;  357000))
;;; Generated autoloads from vimrc-mode.el
 (add-to-list 'auto-mode-alist '("\\.vim\\'" . vimrc-mode))
 (add-to-list 'auto-mode-alist '("[._]?g?vimrc\\'" . vimrc-mode))
 (add-to-list 'auto-mode-alist '("\\.exrc\\'" . vimrc-mode))

(autoload 'vimrc-mode "vimrc-mode" "\
Major mode for editing `vimrc', `xxx.vim' and `.exrc' configuration files.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; vimrc-mode-autoloads.el ends here
