;;; evil-numbers-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (evil-numbers/dec-at-pt evil-numbers/inc-at-pt)
;;;;;;  "evil-numbers" "evil-numbers.el" (21406 25030 0 0))
;;; Generated autoloads from evil-numbers.el

(autoload 'evil-numbers/inc-at-pt "evil-numbers" "\
Increment the number at point or after point before end-of-line by `amount'.
When region is selected, increment all numbers in the region by `amount'

NO-REGION is internal flag that allows
`evil-numbers/inc-at-point' to be called recursively when
applying the regional features of `evil-numbers/inc-at-point'.

\(fn AMOUNT &optional NO-REGION)" t nil)

(autoload 'evil-numbers/dec-at-pt "evil-numbers" "\
Decrement the number at point or after point before end-of-line by `amount'.

If a region is active, decrement all the numbers at a point by `amount'.

This function uses `evil-numbers/inc-at-pt'

\(fn AMOUNT)" t nil)

;;;***

;;;### (autoloads nil nil ("evil-numbers-pkg.el") (21406 25030 40117
;;;;;;  0))

;;;***

(provide 'evil-numbers-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-numbers-autoloads.el ends here
