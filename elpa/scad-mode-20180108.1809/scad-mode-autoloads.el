;;; scad-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "scad-mode" "scad-mode.el" (23138 47881 196807
;;;;;;  961000))
;;; Generated autoloads from scad-mode.el

(add-to-list 'auto-mode-alist '("\\.scad$" . scad-mode))

(autoload 'scad-mode "scad-mode" "\
Major mode for editing OpenSCAD code.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `scad-mode-hook'.

Key bindings:
\\{scad-mode-map}

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; scad-mode-autoloads.el ends here
