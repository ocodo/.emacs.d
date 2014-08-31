;;; crisp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "crisp" "crisp.el" (21506 58446 863797 0))
;;; Generated autoloads from crisp.el

(defvar crisp-mode nil "\
Track status of CRiSP emulation mode.
A value of nil means CRiSP mode is not enabled.  A value of t
indicates CRiSP mode is enabled.

Setting this variable directly does not take effect;
use either M-x customize or the function `crisp-mode'.")

(custom-autoload 'crisp-mode "crisp" nil)

(autoload 'crisp-mode "crisp" "\
Toggle CRiSP/Brief emulation (CRiSP mode).
With a prefix argument ARG, enable CRiSP mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

\(fn &optional ARG)" t nil)

(defalias 'brief-mode 'crisp-mode)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; crisp-autoloads.el ends here
