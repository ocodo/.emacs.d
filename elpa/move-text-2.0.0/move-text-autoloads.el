;;; move-text-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "move-text" "move-text.el" (22510 29111 0 0))
;;; Generated autoloads from move-text.el

(autoload 'move-text-at-last-line-p "move-text" "\
Predicate, point at the last line?

\(fn)" nil nil)

(autoload 'move-line-up "move-text" "\
Move the current line up.

\(fn)" t nil)

(autoload 'move-line-down "move-text" "\
Move the current line down.

\(fn)" t nil)

(autoload 'move-region "move-text" "\
Move the current region (START END) up or down by N lines.

\(fn START END N)" t nil)

(autoload 'move-region-up "move-text" "\
Move the current region (START END) up by N lines.

\(fn START END N)" t nil)

(autoload 'move-region-down "move-text" "\
Move the current region (START END) down by N lines.

\(fn START END N)" t nil)

(autoload 'move-text-up "move-text" "\
Move the line or region (START END) up by N lines.

\(fn &optional START END N)" t nil)

(autoload 'move-text-down "move-text" "\
Move the line or region (START END) down by N lines.

\(fn &optional START END N)" t nil)

(autoload 'move-text-default-bindings "move-text" "\
Use default bindings for move-text-up and move-text-down (M-up / M-down).

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; move-text-autoloads.el ends here
