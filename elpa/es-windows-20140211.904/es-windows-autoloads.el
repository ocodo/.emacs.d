;;; es-windows-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "es-windows" "es-windows.el" (0 0 0 0))
;;; Generated autoloads from es-windows.el

(autoload 'esw/select-window "es-windows" "\
Query for a window using PROMPT, select and return it.

If SHOW-INTERNAL-WINDOWS is non-nil, show their labels, and accept them as input.
If an internal window is selected, it's children will be deleted.

If ALLOW-SPLITTING is non-nil, provide the user an option to split windows.

\(fn &optional PROMPT SHOW-INTERNAL-WINDOWS ALLOW-SPLITTING)" t nil)

(autoload 'esw/show-buffer "es-windows" "\
Show the selected buffer in the selected window.

\(fn BUFFER)" t nil)

(autoload 'esw/move-window "es-windows" "\
Show current buffer in a different window, and delete the old window.

\(fn WINDOW)" t nil)

(autoload 'esw/delete-window "es-windows" "\
Choose and delete a window.

\(fn)" t nil)

(autoload 'esw/swap-two-windows "es-windows" "\
Choose and swap two windows.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "es-windows" '("esw/")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; es-windows-autoloads.el ends here
