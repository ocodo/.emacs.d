;;; occur-context-resize-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "occur-context-resize" "occur-context-resize.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from occur-context-resize.el

(autoload 'occur-context-resize-mode "occur-context-resize" "\
Dynamically resize context around matches in occur-mode.

If called interactively, enable Occur-Context-Resize mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\\{occur-context-resize-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "occur-context-resize" '("occur-context-resize-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; occur-context-resize-autoloads.el ends here
