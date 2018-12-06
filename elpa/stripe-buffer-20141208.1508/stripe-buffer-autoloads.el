;;; stripe-buffer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "stripe-buffer" "stripe-buffer.el" (0 0 0 0))
;;; Generated autoloads from stripe-buffer.el

(autoload 'stripe-buffer-mode "stripe-buffer" "\
Stripe buffer mode

\(fn &optional ARG)" t nil)

(autoload 'turn-on-stripe-buffer-mode "stripe-buffer" "\
Turn on `stripe-buffer-mode'.

\(fn)" t nil)

(autoload 'stripe-table-mode "stripe-buffer" "\
Stripe table mode

\(fn &optional ARG)" t nil)

(autoload 'turn-on-stripe-table-mode "stripe-buffer" "\
Turn on `stripe-table-mode'.

\(fn)" t nil)

(autoload 'org-table-stripes-enable "stripe-buffer" "\
Backward compatibility

\(fn)" t nil)

(autoload 'stripe-listify-buffer "stripe-buffer" "\
Turn on `stripe-buffer-mode' and `hl-line-mode'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "stripe-buffer" '("sb/" "stripe-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; stripe-buffer-autoloads.el ends here
