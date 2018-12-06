;;; perspective-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "perspective" "perspective.el" (0 0 0 0))
;;; Generated autoloads from perspective.el

(defvar persp-mode nil "\
Non-nil if Persp mode is enabled.
See the `persp-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `persp-mode'.")

(custom-autoload 'persp-mode "perspective" nil)

(autoload 'persp-mode "perspective" "\
Toggle perspective mode.
When active, keeps track of multiple 'perspectives',
named collections of buffers and window configurations.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "perspective" '("persp" "quick-perspective-keys" "with-perspective" "make-persp" "check-persp")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; perspective-autoloads.el ends here
