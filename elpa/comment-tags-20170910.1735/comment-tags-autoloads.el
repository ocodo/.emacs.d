;;; comment-tags-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "comment-tags" "comment-tags.el" (0 0 0 0))
;;; Generated autoloads from comment-tags.el

(autoload 'comment-tags-list-tags-buffer "comment-tags" "\
List all tags in the current buffer.

\(fn)" t nil)

(autoload 'comment-tags-find-tags-buffer "comment-tags" "\
Complete tags in the current buffer and jump to line.

\(fn)" t nil)

(autoload 'comment-tags-list-tags-buffers "comment-tags" "\
List tags for all open buffers.

\(fn)" t nil)

(autoload 'comment-tags-next-tag "comment-tags" "\
Jump to next comment-tag from point.

\(fn)" t nil)

(autoload 'comment-tags-previous-tag "comment-tags" "\
Jump to previous comment-tag from point.

\(fn)" t nil)

(autoload 'comment-tags-mode "comment-tags" "\
Highlight and navigate comment tags.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "comment-tags" '("comment-tags-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; comment-tags-autoloads.el ends here
