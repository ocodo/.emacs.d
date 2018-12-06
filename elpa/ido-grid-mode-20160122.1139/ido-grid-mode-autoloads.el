;;; ido-grid-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ido-grid-mode" "ido-grid-mode.el" (0 0 0 0))
;;; Generated autoloads from ido-grid-mode.el

(defvar ido-grid-mode nil "\
Non-nil if Ido-Grid mode is enabled.
See the `ido-grid-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ido-grid-mode'.")

(custom-autoload 'ido-grid-mode "ido-grid-mode" nil)

(autoload 'ido-grid-mode "ido-grid-mode" "\
Makes ido-mode display candidates in a grid.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ido-grid-mode" '("ido-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ido-grid-mode-autoloads.el ends here
