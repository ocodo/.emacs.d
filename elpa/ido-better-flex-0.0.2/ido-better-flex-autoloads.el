;;; ido-better-flex-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ido-better-flex/match ido-better-flex/score ido-better-flex/disable
;;;;;;  ido-better-flex/enable) "ido-better-flex" "ido-better-flex.el"
;;;;;;  (20799 54528 0 0))
;;; Generated autoloads from ido-better-flex.el

(autoload 'ido-better-flex/enable "ido-better-flex" "\


\(fn)" t nil)

(autoload 'ido-better-flex/disable "ido-better-flex" "\


\(fn)" t nil)

(autoload 'ido-better-flex/score "ido-better-flex" "\
Computes the score of matching string with abbreviation.
   The return value is in the range 0.0 to 1.0 the later being full-match.

\(fn STRING ABBREVIATION)" nil nil)

(autoload 'ido-better-flex/match "ido-better-flex" "\
Returns an ordered list (higher score first) of items that match the
   current `ido-text'. Items are included only if their score is greater than zero.

\(fn ITEMS)" nil nil)

(defadvice ido-set-matches-1 (around ido-better-flex-match) "\
An advice using `ido-better-flex' for IDO matching." (setq ad-return-value (ido-better-flex/match (ad-get-arg 0))))

(ido-better-flex/enable)

;;;***

;;;### (autoloads nil nil ("ido-better-flex-pkg.el") (20799 54528
;;;;;;  207402 0))

;;;***

(provide 'ido-better-flex-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ido-better-flex-autoloads.el ends here
