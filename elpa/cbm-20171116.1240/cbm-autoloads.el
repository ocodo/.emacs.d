;;; cbm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cbm" "cbm.el" (0 0 0 0))
;;; Generated autoloads from cbm.el

(autoload 'cbm-cycle "cbm" "\
Cycles through buffers with same `major-mode'.

\(fn)" t nil)

(autoload 'cbm-switch-buffer "cbm" "\
Switch to buffer, filtered by `major-mode'.

\(fn)" t nil)

(autoload 'cbm-find-org-agenda-file "cbm" "\
Switch to a file from `org-agenda-files'.

\(fn)" t nil)

(autoload 'cbm-rcirc-switch-to-channel "cbm" "\
Switch to a rcirc channel.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cbm" '("cbm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cbm-autoloads.el ends here
