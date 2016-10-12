;;; hide-region-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "hide-region" "hide-region.el" (0 0 0 0))
;;; Generated autoloads from hide-region.el

(autoload 'hide-region-hide "hide-region" "\
Hides a region by making an invisible overlay over it and save the
overlay on the hide-region-overlays \"ring\"

\(fn)" t nil)

(autoload 'hide-region-unhide "hide-region" "\
Unhide a region at a time, starting with the last one hidden and
deleting the overlay from the hide-region-overlays \"ring\".

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hide-region" '("hide-region-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; hide-region-autoloads.el ends here
