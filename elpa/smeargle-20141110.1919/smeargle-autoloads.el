;;; smeargle-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "smeargle" "smeargle.el" (21604 40969 9235
;;;;;;  56000))
;;; Generated autoloads from smeargle.el

(autoload 'smeargle-clear "smeargle" "\
Clear smeargle overlays in current buffer.

\(fn)" t nil)

(autoload 'smeargle "smeargle" "\
Highlight regions by last updated time.

\(fn &optional UPDATE-TYPE)" t nil)

(autoload 'smeargle-commits "smeargle" "\
Highlight regions by age of commits.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; smeargle-autoloads.el ends here
