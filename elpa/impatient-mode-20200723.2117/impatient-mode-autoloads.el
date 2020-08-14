;;; impatient-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "impatient-mode" "impatient-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from impatient-mode.el

(autoload 'impatient-mode "impatient-mode" "\
Serves the buffer live over HTTP.

If called interactively, enable Impatient mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "impatient-mode" '("httpd/imp" "imp")))

;;;***

;;;### (autoloads nil nil ("impatient-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; impatient-mode-autoloads.el ends here
