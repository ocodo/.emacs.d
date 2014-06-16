;;; github-browse-file-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (github-browse-file-blame github-browse-file) "github-browse-file"
;;;;;;  "github-browse-file.el" (21406 24819 0 0))
;;; Generated autoloads from github-browse-file.el

(autoload 'github-browse-file "github-browse-file" "\
Show the GitHub webpage for the current file. The URL for the webpage is
added to the kill ring. With a prefix argument, \"master\" is used
regardless of the current branch.

In Transient Mark mode, if the mark is active, highlight the contents of the
region.

\(fn &optional FORCE-MASTER)" t nil)

(autoload 'github-browse-file-blame "github-browse-file" "\
Show the GitHub blame page for the current file. The URL for the webpage is
added to the kill ring. With a prefix argument, \"master\" is used
regardless of the current branch.

In Transient Mark mode, if the mark is active, highlight the contents of the
region.

\(fn &optional FORCE-MASTER)" t nil)

;;;***

;;;### (autoloads nil nil ("github-browse-file-pkg.el") (21406 24819
;;;;;;  88060 0))

;;;***

(provide 'github-browse-file-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; github-browse-file-autoloads.el ends here
