;;; flyspell-correct-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "flyspell-correct" "flyspell-correct.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from flyspell-correct.el

(autoload 'flyspell-correct-word-generic "flyspell-correct" "\
Correct word before point using `flyspell-correct-interface'.
Adapted from `flyspell-correct-word-before-point'.

\(fn)" t nil)

(autoload 'flyspell-correct-previous-word-generic "flyspell-correct" "\
Correct the first misspelled word that occurs before point.
But don't look beyond what's visible on the screen.

Uses `flyspell-correct-word-generic' function for correction.

\(fn POSITION)" t nil)

(autoload 'flyspell-correct-next-word-generic "flyspell-correct" "\
Correct the first misspelled word that occurs after point.
But don't look beyond what's visible on the screen.

Uses `flyspell-correct-word-generic' function for correction.

\(fn POSITION)" t nil)

(autoload 'flyspell-correct-auto-mode "flyspell-correct" "\
Minor mode for automatically correcting word at point.

Take my advice and don't use this functionality unless you find
`flyspell-correct-previous-word-generic' function useless for
your purposes. Seriously, just try named function for completion.
You can find more info in comment[1].

\[1]:
https://github.com/syl20bnr/spacemacs/issues/6209#issuecomment-274320376

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flyspell-correct" '("flyspell-correct-")))

;;;***

;;;### (autoloads nil "flyspell-correct-ido" "flyspell-correct-ido.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flyspell-correct-ido.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flyspell-correct-ido" '("flyspell-correct-ido")))

;;;***

;;;### (autoloads nil nil ("flyspell-correct-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; flyspell-correct-autoloads.el ends here
