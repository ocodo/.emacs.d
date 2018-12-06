;;; vimrc-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vimrc-mode" "vimrc-mode.el" (0 0 0 0))
;;; Generated autoloads from vimrc-mode.el
 (add-to-list 'auto-mode-alist '("\\.vim\\'" . vimrc-mode))
 (add-to-list 'auto-mode-alist '("[._]?g?vimrc\\'" . vimrc-mode))
 (add-to-list 'auto-mode-alist '("\\.exrc\\'" . vimrc-mode))

(autoload 'vimrc-mode "vimrc-mode" "\
Major mode for editing `vimrc', `xxx.vim' and `.exrc' configuration files.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vimrc-mode" '("vimrc-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vimrc-mode-autoloads.el ends here
