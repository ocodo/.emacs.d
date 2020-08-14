;;; 0xc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "0xc" "0xc.el" (0 0 0 0))
;;; Generated autoloads from 0xc.el

(autoload '0xc-convert "0xc" "\
Read a number and a base, and output its representation in said base.
If SILENT is non-nil, do not output anything

\(fn BASE &optional NUMBER SILENT)" t nil)

(autoload '0xc-convert-point "0xc" "\
Replace the number at point with its representation in base.

\(fn &optional BASE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "0xc" '("0xc-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; 0xc-autoloads.el ends here
