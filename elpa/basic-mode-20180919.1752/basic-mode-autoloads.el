;;; basic-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "basic-mode" "basic-mode.el" (0 0 0 0))
;;; Generated autoloads from basic-mode.el

(autoload 'basic-mode "basic-mode" "\
Major mode for editing BASIC code.
Commands:
TAB indents for BASIC code. RET will insert a new line starting
with a fresh line number if line numbers are turned on.

To turn on line numbers, customize variables `basic-auto-number'
and `basic-line-number-cols'.

\\{basic-mode-map}

\(fn)" t nil)
 (add-to-list 'auto-mode-alist '("\\.bas\\'" . basic-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "basic-mode" '("basic-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; basic-mode-autoloads.el ends here
