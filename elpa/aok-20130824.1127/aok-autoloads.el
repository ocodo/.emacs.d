;;; aok-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (occur-select mode-occur type-occur all-occur)
;;;;;;  "aok" "aok.el" (21311 59874 0 0))
;;; Generated autoloads from aok.el

(autoload 'all-occur "aok" "\
Search all buffers for REXP.

\(fn REXP)" t nil)

(autoload 'type-occur "aok" "\
EXTENSION denotes a filetype extension to search.
Run occur in all buffers whose names match this type for REXP.

\(fn EXTENSION REXP)" t nil)

(autoload 'mode-occur "aok" "\
Search all buffers with major mode MODE for REXP.

\(fn MODE REXP)" t nil)

(autoload 'occur-select "aok" "\
select what you wan't to see occur

\(fn MORE REGX &optional NOTHING)" t nil)

;;;***

;;;### (autoloads nil nil ("aok-pkg.el") (21311 59874 229470 0))

;;;***

(provide 'aok-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; aok-autoloads.el ends here
