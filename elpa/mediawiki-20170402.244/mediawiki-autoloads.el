;;; mediawiki-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "mediawiki" "mediawiki.el" (0 0 0 0))
;;; Generated autoloads from mediawiki.el

(autoload 'mediawiki-draft "mediawiki" "\
Open a temporary buffer in mediawiki-mode.
This is for editing a draft.  After finishing the editing either
use \\[mediawiki-draft-buffer] to send the data into the
mediawiki-draft-data-file, or send the buffer using
\\[mediawiki-save] and insert it later into a mediawiki article.

\(fn)" t nil)

(autoload 'mediawiki-draft-page "mediawiki" "\
Set the current buffer as a draft buffer.

\(fn)" t nil)

(autoload 'mediawiki-draft-buffer "mediawiki" "\
Mediawiki-draft-buffer sends the contents of the current (temporary)
buffer to the mediawiki-draft-buffer, see the variable
mediawiki-draft-data-file.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mediawiki" '("mediawiki-" "font-mediawiki-" "url-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; mediawiki-autoloads.el ends here
