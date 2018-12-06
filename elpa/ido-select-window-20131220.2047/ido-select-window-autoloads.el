;;; ido-select-window-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ido-select-window" "ido-select-window.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ido-select-window.el

(autoload 'ido-select-window "ido-select-window" "\
Switch the focus to another window.

Choose a buffer name in the mini-buffer using ido.  The currently
selected window is excluded from the list of windows.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ido-select-window" '("ido-select-window-prompt")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ido-select-window-autoloads.el ends here
