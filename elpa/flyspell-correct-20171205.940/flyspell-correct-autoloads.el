;;; flyspell-correct-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "flyspell-correct" "flyspell-correct.el" (23138
;;;;;;  48677 274319 968000))
;;; Generated autoloads from flyspell-correct.el

(autoload 'flyspell-correct-at-point "flyspell-correct" "\
Correct word before point using `flyspell-correct-interface'.
Adapted from `flyspell-correct-word-before-point'.

\(fn)" t nil)

(autoload 'flyspell-correct-previous "flyspell-correct" "\
Correct the first misspelled word that occurs before POSITION.
But don't look beyond what's visible on the screen.

Uses `flyspell-correct-at-point' function for correction.

\(fn POSITION)" t nil)

(autoload 'flyspell-correct-next "flyspell-correct" "\
Correct the first misspelled word that occurs after POSITION.
But don't look beyond what's visible on the screen.

Uses `flyspell-correct-at-point' function for correction.

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

;;;***

;;;### (autoloads nil nil ("flyspell-correct-ido.el" "flyspell-correct-pkg.el")
;;;;;;  (23138 48677 278319 973000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; flyspell-correct-autoloads.el ends here
