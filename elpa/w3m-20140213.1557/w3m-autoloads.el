;;; w3m-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (bookmark-w3m-bookmark-jump) "bookmark-w3m" "bookmark-w3m.el"
;;;;;;  (21262 51793 0 0))
;;; Generated autoloads from bookmark-w3m.el

(autoload 'bookmark-w3m-bookmark-jump "bookmark-w3m" "\
Default bookmark handler for w3m buffers.

\(fn BOOKMARK)" nil nil)

;;;***

;;;### (autoloads (mime-w3m-preview-text/html) "mime-w3m" "mime-w3m.el"
;;;;;;  (21262 51793 0 0))
;;; Generated autoloads from mime-w3m.el

(autoload 'mime-w3m-preview-text/html "mime-w3m" "\


\(fn ENTITY SITUATION)" nil nil)

;;;***

;;;### (autoloads (octet-mime-setup mime-view-octet mime-preview-octet
;;;;;;  octet-find-file octet-buffer) "octet" "octet.el" (21262 51793
;;;;;;  0 0))
;;; Generated autoloads from octet.el

(autoload 'octet-buffer "octet" "\
View octet-stream content according to `octet-type-filter-alist'.
Optional NAME is the filename.
If optional CONTENT-TYPE is specified, it is used for type guess.

\(fn &optional NAME CONTENT-TYPE)" t nil)

(autoload 'octet-find-file "octet" "\
Find FILE with octet-stream decoding.

\(fn FILE)" t nil)

(autoload 'mime-preview-octet "octet" "\
A method for mime-view to preview octet message.

\(fn ENTITY SITUATION)" nil nil)

(autoload 'mime-view-octet "octet" "\
A method for mime-view to display octet message.

\(fn ENTITY SITUATION)" nil nil)

(autoload 'octet-mime-setup "octet" "\
Octet setting for MIME module.

\(fn)" nil nil)

;;;***

;;;### (autoloads (w3m-antenna w3m-about-antenna) "w3m-antenna" "w3m-antenna.el"
;;;;;;  (21262 51793 0 0))
;;; Generated autoloads from w3m-antenna.el

(autoload 'w3m-about-antenna "w3m-antenna" "\


\(fn URL &optional NO-DECODE NO-CACHE POST-DATA REFERER HANDLER)" nil nil)

(autoload 'w3m-antenna "w3m-antenna" "\
Report changes of WEB sites, which is specified in `w3m-antenna-sites'.

\(fn &optional NO-CACHE)" t nil)

;;;***

(provide 'w3m-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; w3m-autoloads.el ends here
