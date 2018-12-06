;;; diffview-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "diffview" "diffview.el" (0 0 0 0))
;;; Generated autoloads from diffview.el

(autoload 'diffview-current "diffview" "\
Show current diff buffer in a side-by-side view.

\(fn)" t nil)

(autoload 'diffview-region "diffview" "\
Show current diff region in a side-by-side view.

\(fn)" t nil)

(autoload 'diffview-message "diffview" "\
Show `message-mode' buffer in a side-by-side view.

This is useful for reading patches from mailing lists.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "diffview" '("diffview-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; diffview-autoloads.el ends here
