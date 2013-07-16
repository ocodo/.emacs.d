;;; remember-theme-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (remember-theme-load remember-theme-save) "remember-theme"
;;;;;;  "remember-theme.el" (20964 55703 0 0))
;;; Generated autoloads from remember-theme.el

(autoload 'remember-theme-save "remember-theme" "\
Creates (or replaces) ~/.emacs-theme, and stores the name of
the current Emacs theme, for retrieval by remember-theme-load

\(fn)" nil nil)

(autoload 'remember-theme-load "remember-theme" "\
Load the theme used last - This is stored in the file
~/.emacs-theme. The last line of .emacs-theme is read as the
theme name.

~/.emacs-theme is created by remember-theme-save manually
creating or editing this file is not supported

\(fn)" nil nil)

(when load-file-name (add-hook 'after-init-hook 'remember-theme-load) (add-hook 'kill-emacs-hook 'remember-theme-save))

;;;***

;;;### (autoloads nil nil ("remember-theme-pkg.el") (20964 55705
;;;;;;  362040 0))

;;;***

(provide 'remember-theme-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; remember-theme-autoloads.el ends here
