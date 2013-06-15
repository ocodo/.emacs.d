;;; awk-it-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (awk-it-version awk-it-to-kill-ring awk-it-to-file
;;;;;;  awk-it-with-file awk-it-file awk-it-raw awk-it-single-with-separator
;;;;;;  awk-it-single awk-it-with-separator awk-it) "awk-it" "awk-it.el"
;;;;;;  (20923 54379 0 0))
;;; Generated autoloads from awk-it.el

(autoload 'awk-it "awk-it" "\
Run AWK for each line between point and mark.

\(fn BEG END)" t nil)

(autoload 'awk-it-with-separator "awk-it" "\
Run AWK for each line between point and mark, specifying custom
field separator.

\(fn BEG END FS)" t nil)

(autoload 'awk-it-single "awk-it" "\
Run AWK code(single) for each line between point and mark.

\(fn BEG END)" t nil)

(autoload 'awk-it-single-with-separator "awk-it" "\
Run AWK code(single) for each line between point and mark,
specifying custom field separator.

\(fn BEG END FS CODE)" t nil)

(autoload 'awk-it-raw "awk-it" "\
Run AWK code(raw) for each line between point and mark.

\(fn BEG END)" t nil)

(autoload 'awk-it-file "awk-it" "\
Run AWK code on a region from FILE(auto expands).

\(fn BEG END FILE)" t nil)

(autoload 'awk-it-with-file "awk-it" "\
Run AWK it! with code from FILE(inserts code into template field).

\(fn BEG END FILE)" t nil)

(autoload 'awk-it-to-file "awk-it" "\
Save last AWK it! code to file(as raw AWK code).

\(fn FILE)" t nil)

(autoload 'awk-it-to-kill-ring "awk-it" "\
Save last AWK it! code to kill-ring(as specified by mode).

\(fn)" t nil)

(autoload 'awk-it-version "awk-it" "\
Returns current AWK it! version.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("awk-it-pkg.el") (20923 54379 386967 0))

;;;***

(provide 'awk-it-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; awk-it-autoloads.el ends here
