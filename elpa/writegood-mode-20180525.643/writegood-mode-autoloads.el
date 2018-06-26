;;; writegood-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "writegood-mode" "writegood-mode.el" (23344
;;;;;;  44646 371142 293000))
;;; Generated autoloads from writegood-mode.el

(autoload 'writegood-reading-ease "writegood-mode" "\
Flesch-Kincaid reading ease test in the region bounded by START and END.

Scores roughly between 0 and 100.

\(fn &optional START END)" t nil)

(autoload 'writegood-grade-level "writegood-mode" "\
Flesch-Kincaid grade level test. Converts reading ease score to a grade level (Score ~ years of school needed to read passage).

\(fn &optional START END)" t nil)

(autoload 'writegood-mode "writegood-mode" "\
Colorize issues with the writing in the buffer.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; writegood-mode-autoloads.el ends here
