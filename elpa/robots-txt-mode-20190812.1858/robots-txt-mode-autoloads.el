;;; robots-txt-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "robots-txt-mode" "robots-txt-mode.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from robots-txt-mode.el

(autoload 'robots-txt-mode "robots-txt-mode" "\
Major mode for editing `robots.txt'

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("/robots\\.txt\\'" . robots-txt-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "robots-txt-mode" '("robots-txt-mode-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; robots-txt-mode-autoloads.el ends here
