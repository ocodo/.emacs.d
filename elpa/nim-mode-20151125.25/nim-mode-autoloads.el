;;; nim-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "company-nim" "company-nim.el" (22108 42348
;;;;;;  401165 570000))
;;; Generated autoloads from company-nim.el

(autoload 'company-nim "company-nim" "\
`company-mode` backend for nimsuggest.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "nim-mode" "nim-mode.el" (22108 42348 421165
;;;;;;  566000))
;;; Generated autoloads from nim-mode.el

(autoload 'nim-mode "nim-mode" "\
A major mode for the Nim programming language.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.nim\\'" . nim-mode))

;;;***

;;;### (autoloads nil nil ("nim-compile.el" "nim-fill.el" "nim-helper.el"
;;;;;;  "nim-indent.el" "nim-mode-pkg.el" "nim-rx.el" "nim-suggest.el"
;;;;;;  "nim-syntax.el" "nim-thing-at-point.el" "nim-util.el" "nim-vars.el")
;;;;;;  (22108 42348 434400 939000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; nim-mode-autoloads.el ends here
