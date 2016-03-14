;;; evil-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "evil-core" "evil-core.el" (22246 12444 789679
;;;;;;  807000))
;;; Generated autoloads from evil-core.el

(defvar evil-mode nil "\
Non-nil if Evil mode is enabled.
See the command `evil-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `evil-mode'.")

(custom-autoload 'evil-mode "evil-core" nil)

(autoload 'evil-mode "evil-core" "\
Toggle Evil-Local mode in all buffers.
With prefix ARG, enable Evil mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Local mode is enabled in all buffers where
`evil-initialize' would do it.
See `evil-local-mode' for more information on Evil-Local mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("evil-command-window.el" "evil-commands.el"
;;;;;;  "evil-common.el" "evil-digraphs.el" "evil-ex.el" "evil-integration.el"
;;;;;;  "evil-jumps.el" "evil-macros.el" "evil-maps.el" "evil-pkg.el"
;;;;;;  "evil-repeat.el" "evil-search.el" "evil-states.el" "evil-types.el"
;;;;;;  "evil-vars.el" "evil.el") (22246 12444 807267 539000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-autoloads.el ends here
