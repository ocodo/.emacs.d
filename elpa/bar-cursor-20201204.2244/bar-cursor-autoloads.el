;;; bar-cursor-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bar-cursor" "bar-cursor.el" (0 0 0 0))
;;; Generated autoloads from bar-cursor.el

(defvar bar-cursor-mode nil "\
Non-nil if Bar-Cursor mode is enabled.
See the `bar-cursor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `bar-cursor-mode'.")

(custom-autoload 'bar-cursor-mode "bar-cursor" nil)

(autoload 'bar-cursor-mode "bar-cursor" "\
Toggle use of 'bar-cursor-mode'.

If called interactively, enable Bar-Cursor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

This global minor mode changes cursor to a bar cursor in insert
mode, and a block cursor in overwrite mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bar-cursor" '("bar-cursor-set-cursor")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bar-cursor-autoloads.el ends here
