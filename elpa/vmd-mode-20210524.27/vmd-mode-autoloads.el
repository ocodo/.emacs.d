;;; vmd-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vmd-mode" "vmd-mode.el" (0 0 0 0))
;;; Generated autoloads from vmd-mode.el

(autoload 'vmd-mode "vmd-mode" "\
Live Markdown preview with `vmd'.

If called interactively, enable Vmd mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vmd-mode" '("vmd-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vmd-mode-autoloads.el ends here
