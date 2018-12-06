;;; select-themes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "select-themes" "select-themes.el" (0 0 0 0))
;;; Generated autoloads from select-themes.el

(autoload 'select-themes "select-themes" "\
Interactively select a THEME, from the available custom themes.

You can also select '*Emacs default*' to return to Emacs default theme.

Note: multiple enabled themes cause Emacs to slow down, so we
disable them before selecting the new theme.

\(fn THEME)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; select-themes-autoloads.el ends here
