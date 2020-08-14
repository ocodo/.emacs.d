;;; ivy-prescient-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-prescient" "ivy-prescient.el" (0 0 0 0))
;;; Generated autoloads from ivy-prescient.el

(defvar ivy-prescient-mode nil "\
Non-nil if Ivy-Prescient mode is enabled.
See the `ivy-prescient-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-prescient-mode'.")

(custom-autoload 'ivy-prescient-mode "ivy-prescient" nil)

(autoload 'ivy-prescient-mode "ivy-prescient" "\
Minor mode to use prescient.el in Ivy menus.

If called interactively, enable Ivy-Prescient mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-prescient" '("ivy-prescient-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-prescient-autoloads.el ends here
