;;; nim-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "company-nim" "company-nim.el" (22271 10450
;;;;;;  990268 622000))
;;; Generated autoloads from company-nim.el

(autoload 'company-nim "company-nim" "\
`company-mode` backend for nimsuggest.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'company-nim-builtin "company-nim" "\
`company-mode` backend for Nim’s primitive functions.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "nim-eldoc" "nim-eldoc.el" (22271 10450 978268
;;;;;;  553000))
;;; Generated autoloads from nim-eldoc.el

(autoload 'nim-eldoc-setup "nim-eldoc" "\
Setup eldoc configuration for nim-mode.

\(fn)" nil nil)

(add-hook 'nim-mode-hook 'nim-eldoc-setup)

;;;***

;;;### (autoloads nil "nim-mode" "nim-mode.el" (22271 10451 10268
;;;;;;  738000))
;;; Generated autoloads from nim-mode.el

(autoload 'nim-mode "nim-mode" "\
A major mode for the Nim programming language.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.nim\\'" . nim-mode))

;;;***

;;;### (autoloads nil "nimscript-mode" "nimscript-mode.el" (22271
;;;;;;  10450 974268 529000))
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

;;;### (autoloads nil nil ("flycheck-nim-async.el" "nim-compile.el"
;;;;;;  "nim-fill.el" "nim-helper.el" "nim-mode-pkg.el" "nim-rx.el"
;;;;;;  "nim-smie.el" "nim-suggest.el" "nim-syntax.el" "nim-thing-at-point.el"
;;;;;;  "nim-util.el" "nim-vars.el") (22271 10451 19917 567000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; nim-mode-autoloads.el ends here
