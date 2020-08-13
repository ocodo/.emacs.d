;;; nameless-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "nameless" "nameless.el" (0 0 0 0))
;;; Generated autoloads from nameless.el

(autoload 'nameless-mode "nameless" "\
Toggle Nameless mode on or off.

If called interactively, enable Nameless mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\\{nameless-mode-map}

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'nameless-mode-from-hook 'nameless-mode "1.0.0")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nameless" '("nameless-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nameless-autoloads.el ends here
