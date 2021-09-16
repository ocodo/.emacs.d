;;; ivy-historian-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-historian" "ivy-historian.el" (0 0 0 0))
;;; Generated autoloads from ivy-historian.el

(defvar ivy-historian-mode nil "\
Non-nil if Ivy-Historian mode is enabled.
See the `ivy-historian-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-historian-mode'.")

(custom-autoload 'ivy-historian-mode "ivy-historian" nil)

(autoload 'ivy-historian-mode "ivy-historian" "\
historian minor mode

If called interactively, enable Ivy-Historian mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-historian" '("ivy-historian-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-historian-autoloads.el ends here
