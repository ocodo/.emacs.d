;;; tss-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tss" "tss.el" (0 0 0 0))
;;; Generated autoloads from tss.el

(autoload 'tss-popup-help "tss" "\
Popup help about anything at point.

\(fn)" t nil)

(autoload 'tss-jump-to-definition "tss" "\
Jump to method definition at point.

\(fn)" t nil)

(autoload 'tss-implement-definition "tss" "\
Implement inherited definitions of current component.

\(fn)" t nil)

(autoload 'tss-run-flymake "tss" "\
Run check by flymake for current buffer.

\(fn)" t nil)

(autoload 'tss-reload-current-project "tss" "\
Reload project data for current buffer.

\(fn)" t nil)

(autoload 'tss-restart-current-buffer "tss" "\
Restart TSS for current buffer.

\(fn)" t nil)

(autoload 'tss-stop-current-buffer "tss" "\
Stop TSS for current buffer.

\(fn)" t nil)

(autoload 'tss-setup-current-buffer "tss" "\
Do setup for using TSS in current buffer.

\(fn)" t nil)

(autoload 'tss-config-default "tss" "\
Do setting recommemded configuration.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tss" '("tss-" "ac-source-tss-")))

;;;***

;;;### (autoloads nil "typescript" "typescript.el" (0 0 0 0))
;;; Generated autoloads from typescript.el

(autoload 'typescript-mode "typescript" "\
Major mode for editing typescript.

Key bindings:

\\{typescript-mode-map}

\(fn)" t nil)

(eval-after-load 'folding '(when (fboundp 'folding-add-to-marks-list) (folding-add-to-marks-list 'typescript-mode "// {{{" "// }}}")))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "typescript" '("typescript-" "with-typescript")))

;;;***

;;;### (autoloads nil nil ("tss-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tss-autoloads.el ends here
