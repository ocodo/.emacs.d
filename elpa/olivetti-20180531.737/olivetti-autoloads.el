;;; olivetti-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "olivetti" "olivetti.el" (0 0 0 0))
;;; Generated autoloads from olivetti.el

(autoload 'turn-on-olivetti-mode "olivetti" "\
Turn on `olivetti-mode' unconditionally.

\(fn)" t nil)

(autoload 'olivetti-mode "olivetti" "\
Olivetti provides a nice writing environment.

Window margins are set to relative widths to accomodate a text
body width set with `olivetti-body-width'.

When `olivetti-hide-mode-line' is non-nil, the mode line is also
hidden.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "olivetti" '("olivetti-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; olivetti-autoloads.el ends here
