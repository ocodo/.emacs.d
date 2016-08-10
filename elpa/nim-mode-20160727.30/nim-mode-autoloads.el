;;; nim-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "company-nim" "company-nim.el" (0 0 0 0))
;;; Generated autoloads from company-nim.el

(autoload 'company-nim "company-nim" "\
`company-mode` backend for nimsuggest.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'company-nim-builtin "company-nim" "\
`company-mode` backend for Nim’s primitive functions.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-nim" '("company-nim" "nim-doc-buffer")))

;;;***

;;;### (autoloads nil "flycheck-nimsuggest" "flycheck-nimsuggest.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flycheck-nimsuggest.el

(autoload 'flycheck-nim-nimsuggest-start "flycheck-nimsuggest" "\
Start nimsuggest’s ‘chk’ method syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck.

\(fn CHECKER CALLBACK)" nil nil)

(eval-after-load "flycheck" '(progn (flycheck-define-generic-checker 'nim-nimsuggest "A syntax checker for Nim lang using nimsuggest.\n\nSee URL `'." :start 'flycheck-nim-nimsuggest-start :modes '(nim-mode nimscript-mode) :predicate (lambda nil (and (nim-suggest-available-p) nim-use-flycheck-nimsuggest))) (add-to-list 'flycheck-checkers 'nim-nimsuggest)))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck-nimsuggest" '("flycheck-nimsuggest-patterns")))

;;;***

;;;### (autoloads nil "nim-compile" "nim-compile.el" (0 0 0 0))
;;; Generated autoloads from nim-compile.el

(autoload 'nim-compile "nim-compile" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-compile" '("nim-")))

;;;***

;;;### (autoloads nil "nim-eldoc" "nim-eldoc.el" (0 0 0 0))
;;; Generated autoloads from nim-eldoc.el

(autoload 'nim-eldoc-setup "nim-eldoc" "\
Setup eldoc configuration for nim-mode.

\(fn)" nil nil)

(add-hook 'nim-common-init-hook 'nim-eldoc-setup)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-eldoc" '("nim-eldoc-")))

;;;***

;;;### (autoloads nil "nim-fill" "nim-fill.el" (0 0 0 0))
;;; Generated autoloads from nim-fill.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-fill" '("nim-fill-")))

;;;***

;;;### (autoloads nil "nim-helper" "nim-helper.el" (0 0 0 0))
;;; Generated autoloads from nim-helper.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-helper" '("nim-")))

;;;***

;;;### (autoloads nil "nim-mode" "nim-mode.el" (0 0 0 0))
;;; Generated autoloads from nim-mode.el

(autoload 'nim-mode "nim-mode" "\
A major mode for the Nim programming language.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.nim\\'" . nim-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-mode" '("nim-")))

;;;***

;;;### (autoloads nil "nim-smie" "nim-smie.el" (0 0 0 0))
;;; Generated autoloads from nim-smie.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-smie" '("nim-")))

;;;***

;;;### (autoloads nil "nim-suggest" "nim-suggest.el" (0 0 0 0))
;;; Generated autoloads from nim-suggest.el

(autoload 'nim-suggest-available-p "nim-suggest" "\


\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-suggest" '("nim")))

;;;***

;;;### (autoloads nil "nim-syntax" "nim-syntax.el" (0 0 0 0))
;;; Generated autoloads from nim-syntax.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-syntax" '("nim")))

;;;***

;;;### (autoloads nil "nim-thing-at-point" "nim-thing-at-point.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from nim-thing-at-point.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-thing-at-point" '("nim-")))

;;;***

;;;### (autoloads nil "nim-util" "nim-util.el" (0 0 0 0))
;;; Generated autoloads from nim-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-util" '("nim-")))

;;;***

;;;### (autoloads nil "nim-vars" "nim-vars.el" (0 0 0 0))
;;; Generated autoloads from nim-vars.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-vars" '("nim")))

;;;***

;;;### (autoloads nil "nimscript-mode" "nimscript-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from nimscript-mode.el

(autoload 'nimscript-mode "nimscript-mode" "\
A major-mode for NimScript files.
This major-mode is activated when you enter *.nims and *.nimble
suffixed files, but if it’s .nimble file, also another logic is
applied. See also ‘nimscript-mode-maybe’.

\(fn)" t nil)

(autoload 'nimscript-mode-maybe "nimscript-mode" "\
Most likely turn on ‘nimscript-mode’.
In *.nimble files, if the first line sentence matches
‘nim-nimble-ini-format-regex’, this function activates ‘conf-mode’
instead.  The default regex’s matching word is [Package].

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.nim\\(ble\\|s\\)\\'" . nimscript-mode-maybe))

;;;***

;;;### (autoloads nil nil ("nim-mode-pkg.el" "nim-rx.el") (0 0 0
;;;;;;  0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; nim-mode-autoloads.el ends here
