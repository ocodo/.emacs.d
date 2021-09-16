;;; ido-vertical-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ido-vertical-mode" "ido-vertical-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ido-vertical-mode.el

(defvar ido-vertical-mode nil "\
Non-nil if Ido-Vertical mode is enabled.
See the `ido-vertical-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ido-vertical-mode'.")

(custom-autoload 'ido-vertical-mode "ido-vertical-mode" nil)

(autoload 'ido-vertical-mode "ido-vertical-mode" "\
Makes ido-mode display vertically.

If called interactively, enable Ido-Vertical mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ido-vertical-mode" '("ido-vertical-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ido-vertical-mode-autoloads.el ends here
