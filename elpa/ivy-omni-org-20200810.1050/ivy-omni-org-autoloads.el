;;; ivy-omni-org-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-omni-org" "ivy-omni-org.el" (0 0 0 0))
;;; Generated autoloads from ivy-omni-org.el

(autoload 'ivy-omni-org "ivy-omni-org" "\
Ivy interface to buffers, files, and bookmarks in Org.

When TYPES is a list of symbols, this function limits the content
types to it.

\(fn &key TYPES)" t nil)

(autoload 'ivy-omni-org-bookmarks "ivy-omni-org" "\
Display a list of bookmarks." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-omni-org" '("ivy-omni-org-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-omni-org-autoloads.el ends here
