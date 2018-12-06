;;; multi-line-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "multi-line" "multi-line.el" (0 0 0 0))
;;; Generated autoloads from multi-line.el

(autoload 'multi-line-enable-mode-hooks "multi-line" "\
Set default language specific strategies for multi-line.

\(fn)" t nil)

(autoload 'multi-line-disable-mode-hooks "multi-line" "\
Remove default language specific strategies for multi-line.

\(fn)" t nil)

(autoload 'multi-line "multi-line" "\
Multi-line the statement at point.

When ARG is provided single-line the statement at point instead.

\(fn ARG)" t nil)

(autoload 'multi-line-single-line "multi-line" "\
Single-line the statement at point.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multi-line" '("multi-line-")))

;;;***

;;;### (autoloads nil "multi-line-candidate" "multi-line-candidate.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from multi-line-candidate.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multi-line-candidate" '("multi-line-candidate")))

;;;***

;;;### (autoloads nil "multi-line-cycle" "multi-line-cycle.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from multi-line-cycle.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multi-line-cycle" '("multi-line-")))

;;;***

;;;### (autoloads nil "multi-line-decorator" "multi-line-decorator.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from multi-line-decorator.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multi-line-decorator" '("multi-line-")))

;;;***

;;;### (autoloads nil "multi-line-enter" "multi-line-enter.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from multi-line-enter.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multi-line-enter" '("multi-line-")))

;;;***

;;;### (autoloads nil "multi-line-find" "multi-line-find.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from multi-line-find.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multi-line-find" '("multi-line-")))

;;;***

;;;### (autoloads nil "multi-line-highlight" "multi-line-highlight.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from multi-line-highlight.el

(autoload 'multi-line-clear-highlights "multi-line-highlight" "\
Remove any existing multi-line highlight overlays.

\(fn)" t nil)

(autoload 'multi-line-highlight-current-candidates "multi-line-highlight" "\
Highlight the positions at which multi-line will consider adding newlines.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multi-line-highlight" '("multi-line-")))

;;;***

;;;### (autoloads nil "multi-line-respace" "multi-line-respace.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from multi-line-respace.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multi-line-respace" '("multi-line-")))

;;;***

;;;### (autoloads nil "multi-line-shared" "multi-line-shared.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from multi-line-shared.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multi-line-shared" '("multi-line-")))

;;;***

;;;### (autoloads nil nil ("multi-line-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; multi-line-autoloads.el ends here
