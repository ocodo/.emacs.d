;;; puppet-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "puppet-mode" "puppet-mode.el" (0 0 0 0))
;;; Generated autoloads from puppet-mode.el

(autoload 'puppet-mode "puppet-mode" "\


\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "puppet-mode" '("puppet-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; puppet-mode-autoloads.el ends here
