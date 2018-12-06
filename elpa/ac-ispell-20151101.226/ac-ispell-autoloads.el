;;; ac-ispell-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ac-ispell" "ac-ispell.el" (0 0 0 0))
;;; Generated autoloads from ac-ispell.el

(autoload 'ac-ispell-ac-setup "ac-ispell" "\
Add `ac-source-ispell' to `ac-sources' and enable `auto-complete' mode

\(fn)" t nil)

(autoload 'ac-ispell-setup "ac-ispell" "\
Declare auto-complete source based on `ac-ispell-requires'

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ac-ispell" '("ac-ispell-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ac-ispell-autoloads.el ends here
