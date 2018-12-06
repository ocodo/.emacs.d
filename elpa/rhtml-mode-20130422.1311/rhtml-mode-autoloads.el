;;; rhtml-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rhtml-erb" "rhtml-erb.el" (0 0 0 0))
;;; Generated autoloads from rhtml-erb.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rhtml-erb" '("skip-whitespace-forward" "rhtml-")))

;;;***

;;;### (autoloads nil "rhtml-fonts" "rhtml-fonts.el" (0 0 0 0))
;;; Generated autoloads from rhtml-fonts.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rhtml-fonts" '("erb-type-to-" "rhtml-")))

;;;***

;;;### (autoloads nil "rhtml-mode" "rhtml-mode.el" (0 0 0 0))
;;; Generated autoloads from rhtml-mode.el

(autoload 'rhtml-mode "rhtml-mode" "\
Embedded Ruby Mode (RHTML)

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . rhtml-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rhtml-mode" '("rhtml-dashize" "extract-partial")))

;;;***

;;;### (autoloads nil "rhtml-navigation" "rhtml-navigation.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from rhtml-navigation.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rhtml-navigation" '("match-strings" "current-line" "rails-root" "rinari-find-by-context" "rhtml-")))

;;;***

;;;### (autoloads nil "rhtml-ruby-hook" "rhtml-ruby-hook.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from rhtml-ruby-hook.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rhtml-ruby-hook" '("rhtml-" "rthml-insert-from-ruby-temp")))

;;;***

;;;### (autoloads nil "rhtml-sgml-hacks" "rhtml-sgml-hacks.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from rhtml-sgml-hacks.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rhtml-sgml-hacks" '("sgml-" "rhtml-")))

;;;***

;;;### (autoloads nil nil ("rhtml-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rhtml-mode-autoloads.el ends here
