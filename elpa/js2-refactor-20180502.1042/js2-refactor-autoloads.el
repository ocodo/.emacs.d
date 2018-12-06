;;; js2-refactor-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "js2-refactor" "js2-refactor.el" (0 0 0 0))
;;; Generated autoloads from js2-refactor.el

(autoload 'js2-refactor-mode "js2-refactor" "\
Minor mode providing JavaScript refactorings.

\(fn &optional ARG)" t nil)

(autoload 'js2r-add-keybindings-with-prefix "js2-refactor" "\
Add js2r keybindings using the prefix PREFIX.

\(fn PREFIX)" nil nil)

(autoload 'js2r-add-keybindings-with-modifier "js2-refactor" "\
Add js2r keybindings using the modifier MODIFIER.

\(fn MODIFIER)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js2-refactor" '("js2")))

;;;***

;;;### (autoloads nil "js2r-conditionals" "js2r-conditionals.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from js2r-conditionals.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js2r-conditionals" '("js2r-ternary-to-if")))

;;;***

;;;### (autoloads nil "js2r-conveniences" "js2r-conveniences.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from js2r-conveniences.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js2r-conveniences" '("js2r-" "move-line-")))

;;;***

;;;### (autoloads nil "js2r-formatting" "js2r-formatting.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from js2r-formatting.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js2r-formatting" '("js2r-")))

;;;***

;;;### (autoloads nil "js2r-functions" "js2r-functions.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from js2r-functions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js2r-functions" '("js2r-")))

;;;***

;;;### (autoloads nil "js2r-helpers" "js2r-helpers.el" (0 0 0 0))
;;; Generated autoloads from js2r-helpers.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js2r-helpers" '("js2r--")))

;;;***

;;;### (autoloads nil "js2r-iife" "js2r-iife.el" (0 0 0 0))
;;; Generated autoloads from js2r-iife.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js2r-iife" '("js2r-")))

;;;***

;;;### (autoloads nil "js2r-paredit" "js2r-paredit.el" (0 0 0 0))
;;; Generated autoloads from js2r-paredit.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js2r-paredit" '("js2r-")))

;;;***

;;;### (autoloads nil "js2r-vars" "js2r-vars.el" (0 0 0 0))
;;; Generated autoloads from js2r-vars.el

(autoload 'js2r-extract-var "js2r-vars" "\


\(fn)" t nil)

(autoload 'js2r-extract-let "js2r-vars" "\


\(fn)" t nil)

(autoload 'js2r-extract-const "js2r-vars" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js2r-vars" '("js2r-" "current-line-contents")))

;;;***

;;;### (autoloads nil "js2r-wrapping" "js2r-wrapping.el" (0 0 0 0))
;;; Generated autoloads from js2r-wrapping.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js2r-wrapping" '("js2r-")))

;;;***

;;;### (autoloads nil nil ("js2-refactor-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; js2-refactor-autoloads.el ends here
