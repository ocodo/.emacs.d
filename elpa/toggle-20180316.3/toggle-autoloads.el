;;; toggle-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "toggle" "toggle.el" (0 0 0 0))
;;; Generated autoloads from toggle.el

(autoload 'toggle-style "toggle" "\


\(fn NAME)" t nil)

(autoload 'toggle-buffer "toggle" "\
Opens a related file to the current buffer using matching rules.
Matches the current buffer against rules in toggle-mappings. If a
match is found, switches to that buffer.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "toggle" '("toggle-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; toggle-autoloads.el ends here
