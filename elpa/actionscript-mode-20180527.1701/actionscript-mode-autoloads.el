;;; actionscript-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "actionscript-mode" "actionscript-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from actionscript-mode.el

(autoload 'actionscript-mode "actionscript-mode" "\
Major mode for editing Actionscript files.
\\{actionscript-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "actionscript-mode" '("reload-actionscript-mode" "actionscript-" "preprocessor-kwds")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; actionscript-mode-autoloads.el ends here
