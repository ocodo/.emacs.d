;;; evil-terminal-cursor-changer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-terminal-cursor-changer" "evil-terminal-cursor-changer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-terminal-cursor-changer.el

(autoload 'evil-terminal-cursor-changer-activate "evil-terminal-cursor-changer" "\
Enable evil terminal cursor changer." t nil)

(defalias 'etcc-on 'evil-terminal-cursor-changer-activate)

(autoload 'evil-terminal-cursor-changer-deactivate "evil-terminal-cursor-changer" "\
Disable evil terminal cursor changer." t nil)

(defalias 'etcc-off 'evil-terminal-cursor-changer-deactivate)

(defvar etcc-mode nil "\
Non-nil if Etcc mode is enabled.
See the `etcc-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `etcc-mode'.")

(custom-autoload 'etcc-mode "evil-terminal-cursor-changer" nil)

(autoload 'etcc-mode "evil-terminal-cursor-changer" "\
Minor mode for changing cursor by mode for evil on terminal.

If called interactively, enable Etcc mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-terminal-cursor-changer" '("etcc-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-terminal-cursor-changer-autoloads.el ends here
