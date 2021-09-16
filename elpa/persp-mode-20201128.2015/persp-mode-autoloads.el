;;; persp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "persp-mode" "persp-mode.el" (0 0 0 0))
;;; Generated autoloads from persp-mode.el

(autoload 'persp-def-auto-persp "persp-mode" "\


\(fn NAME &rest KEYARGS &key BUFFER-NAME FILE-NAME MODE MODE-NAME MINOR-MODE MINOR-MODE-NAME PREDICATE HOOKS DYN-ENV GET-NAME GET-BUFFER GET-PERSP SWITCH PARAMETERS NOAUTO WEAK USER-DATA ON-MATCH AFTER-MATCH DONT-PICK-UP-BUFFERS DELETE)" nil nil)

(define-obsolete-function-alias 'def-auto-persp 'persp-def-auto-persp "persp-mode 2.9.6")

(autoload 'persp-def-buffer-save/load "persp-mode" "\


\(fn &rest KEYARGS &key BUFFER-NAME FILE-NAME MODE MODE-NAME MINOR-MODE MINOR-MODE-NAME PREDICATE TAG-SYMBOL SAVE-VARS SAVE-FUNCTION LOAD-FUNCTION AFTER-LOAD-FUNCTION MODE-RESTORE-FUNCTION APPEND)" nil nil)

(define-obsolete-function-alias 'def-persp-buffer-save/load 'persp-def-buffer-save/load "persp-mode 2.9.6")

(defvar persp-mode nil "\
Non-nil if Persp mode is enabled.
See the `persp-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `persp-mode'.")

(custom-autoload 'persp-mode "persp-mode" nil)

(autoload 'persp-mode "persp-mode" "\
Toggle the persp-mode.
When active, keeps track of multiple 'perspectives',
named collections of buffers and window configurations.
Here is a keymap of this minor mode:
\\{persp-mode-map}

If called interactively, enable Persp mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "persp-mode" '("*persp-" "clear-window-persp" "def-" "get-" "ido-toggle-persp-filter" "persp" "safe-persp-" "set-" "window-persp-set-p" "with-persp-ido-hooks")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; persp-mode-autoloads.el ends here
