;;; ameba-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ameba" "ameba.el" (0 0 0 0))
;;; Generated autoloads from ameba.el

(autoload 'ameba-check-current-file "ameba" "\
Run check on the current file." t nil)

(autoload 'ameba-check-project "ameba" "\
Run check on the current project." t nil)

(autoload 'ameba-check-directory "ameba" "\
Run check on the DIRECTORY if present or prompt user if not.

\(fn &optional DIRECTORY)" t nil)

(autoload 'ameba-mode "ameba" "\
Minor mode to interface with Ameba.

If called interactively, enable Ameba mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ameba" '("ameba-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ameba-autoloads.el ends here
