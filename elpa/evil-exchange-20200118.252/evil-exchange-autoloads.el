;;; evil-exchange-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-exchange" "evil-exchange.el" (0 0 0 0))
;;; Generated autoloads from evil-exchange.el

(autoload 'evil-exchange "evil-exchange" "\
Exchange two regions with evil motion." t)

(autoload 'evil-exchange-cancel "evil-exchange" "\
Cancel current pending exchange." t nil)

(autoload 'evil-exchange-install "evil-exchange" "\
Setting evil-exchange key bindings." nil nil)

(autoload 'evil-exchange-cx-install "evil-exchange" "\
Setting evil-exchange key bindings in a vim-compatible way" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-exchange" '("evil-exchange")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-exchange-autoloads.el ends here
