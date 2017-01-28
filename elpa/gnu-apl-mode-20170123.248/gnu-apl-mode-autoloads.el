;;; gnu-apl-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "gnu-apl-documentation" "gnu-apl-documentation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from gnu-apl-documentation.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnu-apl-documentation" '("gnu-apl-" "*gnu-apl-")))

;;;***

;;;### (autoloads nil "gnu-apl-editor" "gnu-apl-editor.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from gnu-apl-editor.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnu-apl-editor" '("gnu-apl-")))

;;;***

;;;### (autoloads nil "gnu-apl-follow" "gnu-apl-follow.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from gnu-apl-follow.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnu-apl-follow" '("gnu-apl-")))

;;;***

;;;### (autoloads nil "gnu-apl-interactive" "gnu-apl-interactive.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from gnu-apl-interactive.el

(autoload 'gnu-apl "gnu-apl-interactive" "\
Start the GNU APL interpreter in a buffer.
APL-EXECUTABLE is the path to the apl program (defaults
to ‘gnu-apl-executable’).

\(fn APL-EXECUTABLE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnu-apl-interactive" '("gnu-apl-" "*gnu-apl-")))

;;;***

;;;### (autoloads nil "gnu-apl-mode" "gnu-apl-mode.el" (0 0 0 0))
;;; Generated autoloads from gnu-apl-mode.el

(let ((loads (get 'gnu-apl 'custom-loads))) (if (member '"gnu-apl-mode" loads) nil (put 'gnu-apl 'custom-loads (cons '"gnu-apl-mode" loads))))

(autoload 'gnu-apl-mode "gnu-apl-mode" "\
Major mode for editing GNU APL files.

\(fn)" t nil)

(autoload 'gnu-apl "gnu-apl-mode" "\
Start the GNU APL interpreter in a buffer.
APL-EXECUTABLE is the path to the apl program (defaults
to ‘gnu-apl-executable’).

\(fn APL-EXECUTABLE)" t nil)

(add-to-list 'auto-mode-alist '("\\.apl\\'" . gnu-apl-mode))

(add-to-list 'interpreter-mode-alist '("apl" . gnu-apl-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnu-apl-mode" '("company-gnu-apl" "gnu-apl-")))

;;;***

;;;### (autoloads nil "gnu-apl-network" "gnu-apl-network.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from gnu-apl-network.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnu-apl-network" '("gnu-apl--" "*gnu-apl-")))

;;;***

;;;### (autoloads nil "gnu-apl-osx-workaround" "gnu-apl-osx-workaround.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from gnu-apl-osx-workaround.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnu-apl-osx-workaround" '("gnu-apl-update-fontset-character")))

;;;***

;;;### (autoloads nil "gnu-apl-plot" "gnu-apl-plot.el" (0 0 0 0))
;;; Generated autoloads from gnu-apl-plot.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnu-apl-plot" '("gnu-apl-")))

;;;***

;;;### (autoloads nil "gnu-apl-refdocs-bsd-license" "gnu-apl-refdocs-bsd-license.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from gnu-apl-refdocs-bsd-license.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnu-apl-refdocs-bsd-license" '("gnu-apl--symbol-doc")))

;;;***

;;;### (autoloads nil "gnu-apl-spreadsheet" "gnu-apl-spreadsheet.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from gnu-apl-spreadsheet.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnu-apl-spreadsheet" '("gnu-apl-")))

;;;***

;;;### (autoloads nil "gnu-apl-symbols" "gnu-apl-symbols.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from gnu-apl-symbols.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnu-apl-symbols" '("gnu-apl--symbols")))

;;;***

;;;### (autoloads nil "gnu-apl-util" "gnu-apl-util.el" (0 0 0 0))
;;; Generated autoloads from gnu-apl-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnu-apl-util" '("gnu-apl--")))

;;;***

;;;### (autoloads nil nil ("gnu-apl-input.el" "gnu-apl-mode-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gnu-apl-mode-autoloads.el ends here
