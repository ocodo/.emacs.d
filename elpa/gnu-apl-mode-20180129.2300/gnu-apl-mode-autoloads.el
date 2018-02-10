;;; gnu-apl-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "gnu-apl-mode" "gnu-apl-mode.el" (23166 23115
;;;;;;  116755 87000))
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

;;;***

;;;### (autoloads nil nil ("gnu-apl-documentation.el" "gnu-apl-editor.el"
;;;;;;  "gnu-apl-finnapl.el" "gnu-apl-follow.el" "gnu-apl-input.el"
;;;;;;  "gnu-apl-interactive.el" "gnu-apl-mode-pkg.el" "gnu-apl-network.el"
;;;;;;  "gnu-apl-osx-workaround.el" "gnu-apl-plot.el" "gnu-apl-refdocs-bsd-license.el"
;;;;;;  "gnu-apl-spreadsheet.el" "gnu-apl-symbols.el" "gnu-apl-util.el")
;;;;;;  (23166 23115 120755 82000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gnu-apl-mode-autoloads.el ends here
