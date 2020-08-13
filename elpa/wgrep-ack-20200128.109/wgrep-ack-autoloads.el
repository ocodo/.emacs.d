;;; wgrep-ack-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "wgrep-ack" "wgrep-ack.el" (0 0 0 0))
;;; Generated autoloads from wgrep-ack.el

(autoload 'wgrep-ack-and-a-half-setup "wgrep-ack" nil nil nil)

(autoload 'wgrep-ack-setup "wgrep-ack" nil nil nil)

(add-hook 'ack-and-a-half-mode-hook 'wgrep-ack-and-a-half-setup)

(add-hook 'ack-mode-hook 'wgrep-ack-setup)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "wgrep-ack" '("wgrep-ack-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wgrep-ack-autoloads.el ends here
