;;; remember-themes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "remember-themes" "remember-themes.el" (22252
;;;;;;  2123 0 0))
;;; Generated autoloads from remember-themes.el

(autoload 'remember-theme-read "remember-themes" "\
Return first line from `remember-theme-emacs-dot-file'.

\(fn)" nil nil)

(autoload 'remember-theme-save "remember-themes" "\
Creates (or replaces) ~/.emacs-theme.
Stores the name of the current Emacs theme,
for retrieval by remember-theme-load

\(fn)" nil nil)

(autoload 'remember-theme-load "remember-themes" "\
Load the theme used last.
This is stored in the file `remember-theme-emacs-dot-file'.
The last line of `remember-theme-emacs-dot-file' is read as the theme name.

`remember-theme-emacs-dot-file' is created by remember-theme-save
manually creating or editing this file is not recommended.

Also if the theme is no longer available on this site, an error will be thrown.

Any currently loaded themes will be disabled and the theme named in
`remember-theme-emacs-dot-file' will be loaded.

If no `remember-theme-emacs-dot-file' file exists the operation is skipped.

\(fn)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; remember-themes-autoloads.el ends here
