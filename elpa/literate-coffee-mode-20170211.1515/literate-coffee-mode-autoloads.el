;;; literate-coffee-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "literate-coffee-mode" "literate-coffee-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from literate-coffee-mode.el

(autoload 'litcoffee-mode "literate-coffee-mode" "\
Major mode for editing Literate CoffeeScript.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.litcoffee\\'" . litcoffee-mode))

(add-to-list 'auto-mode-alist '("\\.coffee.md\\'" . litcoffee-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "literate-coffee-mode" '("litcoffee-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; literate-coffee-mode-autoloads.el ends here
