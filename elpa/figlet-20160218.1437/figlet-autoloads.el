;;; figlet-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "figlet" "figlet.el" (22218 26455 738551 967000))
;;; Generated autoloads from figlet.el

(autoload 'figlet "figlet" "\
Pass a string through figlet and insert the output at
point. Use a prefix arg to be promted for a font.

\(fn STRING)" t nil)

(autoload 'figlet-comment "figlet" "\
Insert a figlet string just as `figlet' would but comment the
result (using `comment-region')

\(fn STRING)" t nil)

(autoload 'figlet-figletify-region "figlet" "\
Convert the region into a figlet string.

\(fn START END)" t nil)

(autoload 'figlet-figletify-region-comment "figlet" "\
Convert the region into a figlet string as with
`figlet-figletify-region' but comment it out too.

\(fn START END)" t nil)

(autoload 'figlet-preview-fonts "figlet" "\
View an example of each font in a new buffer.

\(fn &optional TEXT)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; figlet-autoloads.el ends here
