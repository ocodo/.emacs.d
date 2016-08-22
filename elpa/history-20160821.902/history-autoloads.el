;;; history-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "history" "history.el" (22459 4245 250138 82000))
;;; Generated autoloads from history.el

(autoload 'history-add-history "history" "\
Add current position into the database, which is `global-mark-ring'. If
SAVE-THING? is t, it will cache the symbol string at point (if any) and use it as
a comparison in checking algorithm when navigating to it. If they are not matched,
the history will be deleted immediately.

\(fn &optional SAVE-THING\\=\\?)" t nil)

(autoload 'history-show-history "history" "\
Show histories in a pretty way.

\(fn)" t nil)

(autoload 'history-goto-history "history" "\


\(fn)" t nil)

(autoload 'history-kill-histories "history" "\
Discard all the histories.

\(fn)" t nil)

(autoload 'history-prev-history "history" "\
Navigate to previous history.

\(fn)" t nil)

(autoload 'history-next-history "history" "\
Navigate to next history.

\(fn)" t nil)

(autoload 'history-toggle-window-local-history "history" "\
Switch between window-local history or global history mode.
See `history-window-local-history'.

\(fn)" t nil)

(defvar history-mode nil "\
Non-nil if History mode is enabled.
See the `history-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `history-mode'.")

(custom-autoload 'history-mode "history" nil)

(autoload 'history-mode "history" "\
Add menus, toolbar buttons and more.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("history-pkg.el") (22459 4245 254138 76000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; history-autoloads.el ends here
