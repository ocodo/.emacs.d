;;; mediawiki-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "mediawiki" "mediawiki.el" (22955 53304 801479
;;;;;;  320000))
;;; Generated autoloads from mediawiki.el

(autoload 'mediawiki-site "mediawiki" "\
Set up mediawiki.el for a SITE.
Without an argument, use `mediawiki-site-default'.
Interactively, prompt for a SITE.

\(fn &optional SITE)" t nil)

(autoload 'mediawiki-draft "mediawiki" "\
Open a temporary buffer in mediawiki-mode.
This is for editing a draft.  After finishing the editing either
use \\[mediawiki-draft-buffer] to send the data into the
mediawiki-draft-data-file, or send the buffer using
\\[mediawiki-save] and insert it later into a mediawiki article.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; mediawiki-autoloads.el ends here
