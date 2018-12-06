;;; crisp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "crisp" "crisp.el" (0 0 0 0))
;;; Generated autoloads from crisp.el

(defvar crisp-mode nil "\
Non-nil if Crisp mode is enabled.
See the `crisp-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `crisp-mode'.")

(custom-autoload 'crisp-mode "crisp" nil)

(autoload 'crisp-mode "crisp" "\
Toggle CRiSP/Brief emulation (CRiSP mode).
With a prefix argument ARG, enable CRiSP mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "crisp" '("crisp-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; crisp-autoloads.el ends here
