;;; flyspell-correct-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "flyspell-correct" "flyspell-correct.el" (22366
;;;;;;  21218 916453 409000))
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

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; flyspell-correct-autoloads.el ends here
