;;; focus-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "focus" "focus.el" (0 0 0 0))
;;; Generated autoloads from focus.el

(autoload 'focus-mode "focus" "\
Dim the font color of text in surrounding sections.

\(fn &optional ARG)" t nil)

(autoload 'focus-read-only-mode "focus" "\
A read-only mode optimized for `focus-mode'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "focus" '("turn-off-focus-read-only-mode" "focus-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; focus-autoloads.el ends here
