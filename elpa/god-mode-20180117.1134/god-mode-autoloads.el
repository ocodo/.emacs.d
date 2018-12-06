;;; god-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "god-mode" "god-mode.el" (0 0 0 0))
;;; Generated autoloads from god-mode.el

(autoload 'god-local-mode "god-mode" "\
Minor mode for running commands.

\(fn &optional ARG)" t nil)

(autoload 'god-mode "god-mode" "\
Toggle global God mode.

\(fn)" t nil)

(autoload 'god-mode-all "god-mode" "\
Toggle God mode in all buffers.

\(fn)" t nil)

(autoload 'god-mode-maybe-activate "god-mode" "\
Activate God mode locally on individual buffers when appropriate.

\(fn &optional STATUS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "god-mode" '("god-" "key-string-after-consuming-key")))

;;;***

;;;### (autoloads nil "god-mode-isearch" "god-mode-isearch.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from god-mode-isearch.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "god-mode-isearch" '("god-mode-isearch-")))

;;;***

;;;### (autoloads nil nil ("god-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; god-mode-autoloads.el ends here
