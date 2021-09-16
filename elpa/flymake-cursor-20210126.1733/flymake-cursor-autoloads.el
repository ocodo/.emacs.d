;;; flymake-cursor-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flymake-cursor" "flymake-cursor.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from flymake-cursor.el

(autoload 'flymake-cursor-mode "flymake-cursor" "\
Minor mode to show `flymake-mode' errors for the current line in the
message area.
When called interactively, toggles the minor mode.
With arg, turn Flymake Cursor mode on if and only if arg is positive.

Usually `flymake-cursor-mode' is enabled and disabled automatically with
`flymake-mode' for the current buffer and you will not need to toggle
the mode directly.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flymake-cursor" '("flymake-cursor-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flymake-cursor-autoloads.el ends here
