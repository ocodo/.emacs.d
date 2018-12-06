;;; crystal-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "crystal-mode" "crystal-mode.el" (0 0 0 0))
;;; Generated autoloads from crystal-mode.el

(autoload 'crystal-tool-format "crystal-mode" "\
Format the contents of the current buffer without persisting the result.

\(fn)" t nil)

(autoload 'crystal-tool-expand "crystal-mode" "\
Expand macro at point.

\(fn)" t nil)

(autoload 'crystal-tool-imp "crystal-mode" "\
Show implementations at point.

\(fn)" t nil)

(autoload 'crystal-tool-context "crystal-mode" "\
Show content at point.

\(fn)" t nil)

(autoload 'crystal-def-jump "crystal-mode" "\
Jump to the definition of the expression at POINT.

\(fn POINT &optional OTHER-WINDOW)" t nil)

(autoload 'crystal-def-jump-other-window "crystal-mode" "\


\(fn POINT)" t nil)

(autoload 'crystal-spec-switch "crystal-mode" "\
Switch source file and its spec file.

\(fn)" t nil)

(autoload 'crystal-spec-line "crystal-mode" "\
Run spec on current line.

\(fn)" t nil)

(autoload 'crystal-spec-buffer "crystal-mode" "\
Run spec for current buffer.

\(fn)" t nil)

(autoload 'crystal-spec-all "crystal-mode" "\
Run all specs for current project.

\(fn)" t nil)

(autoload 'crystal-mode "crystal-mode" "\
Major mode for editing Crystal code.

\\{crystal-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons (purecopy (concat "\\(?:\\." "cr" "\\)\\'")) 'crystal-mode))

(dolist (name (list "crystal")) (add-to-list 'interpreter-mode-alist (cons (purecopy name) 'crystal-mode)))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "crystal-mode" '("crystal-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; crystal-mode-autoloads.el ends here
