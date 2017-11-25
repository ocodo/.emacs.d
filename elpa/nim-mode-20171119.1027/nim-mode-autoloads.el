;;; nim-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "nim-capf" "nim-capf.el" (23064 61191 716908
;;;;;;  411000))
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
 (add-hook 'nimsuggest-mode-hook 'nim-capf-setup)

;;;***

;;;### (autoloads nil "nim-compile" "nim-compile.el" (23064 61191
;;;;;;  720908 393000))
;;; Generated autoloads from nim-compile.el

(autoload 'nim-compile "nim-compile" "\
Compile and execute the current buffer as a nim file.
All output is written into the *nim-compile* buffer.
If you put COMMAND argument, you can specify the compilation command.

\(fn &optional COMMAND)" t nil)

;;;***

;;;### (autoloads nil "nim-mode" "nim-mode.el" (23064 61191 728908
;;;;;;  358000))
;;; Generated autoloads from nim-mode.el

(autoload 'nim-mode "nim-mode" "\
A major mode for the Nim programming language.

\(fn)" t nil)

(autoload 'nimscript-mode "nim-mode" "\
A major-mode for NimScript files.
This major-mode is activated when you enter *.nims and *.nimble
suffixed files, but if it’s .nimble file, also another logic is
applied. See also ‘nimscript-mode-maybe’.

\(fn)" t nil)

(autoload 'nimscript-mode-maybe "nim-mode" "\
Most likely turn on ‘nimscript-mode’.
In *.nimble files, if the first line sentence matches
‘nim-nimble-ini-format-regex’, this function activates ‘conf-mode’
instead.  The default regex’s matching word is [Package].

\(fn)" t nil)
 (add-to-list 'auto-mode-alist '("\\.nim\\'" . nim-mode))
 (add-to-list 'auto-mode-alist '("\\.nim\\(ble\\|s\\)\\'" . nimscript-mode-maybe))

(autoload 'nim-eldoc-function "nim-mode" "\
Return a doc string appropriate for the current context, or nil.

\(fn)" t nil)

(autoload 'nim-eldoc-on "nim-mode" "\
This may or may not work.  Maybe this configuration has to set on.
Major-mode configuration according to the document.

\(fn)" t nil)

(autoload 'nim-eldoc-setup "nim-mode" "\
This function may not work.
I tried to configure this stuff to be user definable, but currently failing.
The eldoc support should be turned on automatically, so please
use `nim-eldoc-off' manually if you don't like it.

\(fn &rest ARGS)" nil nil)

;;;***

;;;### (autoloads nil "nim-suggest" "nim-suggest.el" (23064 61191
;;;;;;  728908 358000))
;;; Generated autoloads from nim-suggest.el

(autoload 'nimsuggest-available-p "nim-suggest" "\
Return non-nil if nimsuggest is available in current buffer.

\(fn)" nil nil)

(autoload 'nimsuggest-mode "nim-suggest" "\
Minor mode for nimsuggest.

\(fn &optional ARG)" t nil)

(autoload 'nimsuggest-flymake-setup "nim-suggest" "\
Kinda experimental function to use flymake on Emacs 26.

\(fn)" nil nil)

(autoload 'nimsuggest-eldoc--nimsuggest "nim-suggest" "\
Update `eldoc-last-message' by nimsuggest's information.

\(fn)" nil nil)
 (add-hook 'nimsuggest-mode-hook 'nimsuggest-xref-setup)

(autoload 'nimsuggest-xref-setup "nim-suggest" "\
Setup xref backend for nimsuggest.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("nim-fill.el" "nim-helper.el" "nim-mode-pkg.el"
;;;;;;  "nim-rx.el" "nim-smie.el" "nim-syntax.el" "nim-util.el" "nim-vars.el")
;;;;;;  (23064 61191 728908 358000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; nim-mode-autoloads.el ends here
