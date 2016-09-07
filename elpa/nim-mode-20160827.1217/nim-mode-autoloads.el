;;; nim-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "company-nim" "company-nim.el" (22479 30305
;;;;;;  596413 290000))
;;; Generated autoloads from company-nim.el

(autoload 'company-nim "company-nim" "\
`company-mode` backend for nimsuggest.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'company-nim-builtin "company-nim" "\
`company-mode` backend for Nim’s primitive functions.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "flycheck-nimsuggest" "flycheck-nimsuggest.el"
;;;;;;  (22479 30305 560413 185000))
;;; Generated autoloads from flycheck-nimsuggest.el

(autoload 'flycheck-nim-nimsuggest-start "flycheck-nimsuggest" "\
Start nimsuggest’s ‘chk’ method syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck.

\(fn CHECKER CALLBACK)" nil nil)

(eval-after-load "flycheck" '(progn (flycheck-define-generic-checker 'nim-nimsuggest "A syntax checker for Nim lang using nimsuggest.\n\nSee URL `'." :start 'flycheck-nim-nimsuggest-start :modes '(nim-mode nimscript-mode) :predicate (lambda nil (and (nim-suggest-available-p) nim-use-flycheck-nimsuggest))) (add-to-list 'flycheck-checkers 'nim-nimsuggest)))

;;;***

;;;### (autoloads nil "nim-compile" "nim-compile.el" (22479 30305
;;;;;;  584413 255000))
;;; Generated autoloads from nim-compile.el

(autoload 'nim-compile "nim-compile" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "nim-eldoc" "nim-eldoc.el" (22479 30305 580413
;;;;;;  244000))
;;; Generated autoloads from nim-eldoc.el

(autoload 'nim-eldoc-setup "nim-eldoc" "\
Setup eldoc configuration for nim-mode.

\(fn)" nil nil)

(add-hook 'nim-common-init-hook 'nim-eldoc-setup)

;;;***

;;;### (autoloads nil "nim-mode" "nim-mode.el" (22479 30305 608413
;;;;;;  325000))
;;; Generated autoloads from nim-mode.el

(autoload 'nim-mode "nim-mode" "\
A major mode for the Nim programming language.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.nim\\'" . nim-mode))

;;;***

;;;### (autoloads nil "nim-suggest" "nim-suggest.el" (22479 30305
;;;;;;  608413 325000))
;;; Generated autoloads from nim-suggest.el

(autoload 'nim-suggest-available-p "nim-suggest" "\
Return non-nil if nimsuggest is available in current buffer.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "nimscript-mode" "nimscript-mode.el" (22479
;;;;;;  30305 576413 232000))
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

;;;### (autoloads nil nil ("nim-fill.el" "nim-helper.el" "nim-mode-pkg.el"
;;;;;;  "nim-rx.el" "nim-smie.el" "nim-syntax.el" "nim-thing-at-point.el"
;;;;;;  "nim-util.el" "nim-vars.el") (22479 30305 604413 313000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; nim-mode-autoloads.el ends here
