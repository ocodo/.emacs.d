;;; nim-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "flycheck-nimsuggest" "flycheck-nimsuggest.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flycheck-nimsuggest.el

(autoload 'flycheck-nim-nimsuggest-start "flycheck-nimsuggest" "\
Start nimsuggest’s ‘chk’ method syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck.

\(fn CHECKER CALLBACK)" nil nil)

(autoload 'flycheck-nimsuggest-setup "flycheck-nimsuggest" "\
Setup flycheck configuration for nimsuggest.

\(fn)" nil nil)

(eval-after-load "flycheck" '(progn (flycheck-define-generic-checker 'nim-nimsuggest "A syntax checker for Nim lang using nimsuggest.\n\nSee URL `https://github.com/nim-lang/nimsuggest'." :start 'flycheck-nim-nimsuggest-start :modes '(nim-mode nimscript-mode) :predicate (lambda nil (and (bound-and-true-p nim-use-flycheck-nimsuggest) (nim-suggest-available-p)))) (add-to-list 'flycheck-checkers 'nim-nimsuggest)))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck-nimsuggest" '("flycheck-nimsuggest-")))

;;;***

;;;### (autoloads nil "nim-capf" "nim-capf.el" (0 0 0 0))
;;; Generated autoloads from nim-capf.el

(autoload 'nim-capf-nimsuggest-completion-at-point "nim-capf" "\
Complete the symbol at point using nimsuggest.

\(fn)" nil nil)

(autoload 'nim-builtin-completion-at-point "nim-capf" "\
Complete the symbol at point for .nim files.

\(fn)" nil nil)

(autoload 'nimscript-builtin-completion-at-point "nim-capf" "\
Complete the symbol at point for nimscript files.

\(fn)" nil nil)

(autoload 'nim-capf-setup "nim-capf" "\
Setup.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-capf" '("nim-capf-")))

;;;***

;;;### (autoloads nil "nim-compile" "nim-compile.el" (0 0 0 0))
;;; Generated autoloads from nim-compile.el

(autoload 'nim-compile "nim-compile" "\
Compile and execute the current buffer as a nim file.  All output is writton into the *compilation* buffer.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nim-compile" '("nim-")))

;;;***

;;;### (autoloads nil "nim-eldoc" "nim-eldoc.el" (0 0 0 0))
;;; Generated autoloads from nim-eldoc.el

(autoload 'nim-eldoc-function "nim-eldoc" "\
Return a doc string appropriate for the current context, or nil.

\(fn)" t nil)

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
Return non-nil if nimsuggest is available in current buffer.

\(fn)" nil nil)

(autoload 'nimsuggest-mode "nim-suggest" "\
Minor mode for nimsuggest.

\(fn &optional ARG)" t nil)

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nimscript-mode" '("nimscirpt-mode-syntax-table")))

;;;***

;;;### (autoloads nil nil ("company-nim.el" "nim-mode-pkg.el" "nim-rx.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; nim-mode-autoloads.el ends here
