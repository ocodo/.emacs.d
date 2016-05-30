;;; persp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "persp-mode" "persp-mode.el" (22347 49494 801102
;;;;;;  337000))
;;; Generated autoloads from persp-mode.el

(autoload 'def-auto-persp "persp-mode" "\


\(fn NAME &key BUFFER-NAME FILE-NAME MODE MODE-NAME PREDICATE ON-MATCH AFTER-MATCH HOOKS DYN-ENV GET-BUFFER-EXPR GET-PERSP-EXPR PARAMETERS NOAUTO)" nil (quote macro))

(defvar persp-mode nil "\
Non-nil if Persp mode is enabled.
See the command `persp-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `persp-mode'.")

(custom-autoload 'persp-mode "persp-mode" nil)

(autoload 'persp-mode "persp-mode" "\
Toggle the persp-mode.
When active, keeps track of multiple 'perspectives',
named collections of buffers and window configurations.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; persp-mode-autoloads.el ends here
