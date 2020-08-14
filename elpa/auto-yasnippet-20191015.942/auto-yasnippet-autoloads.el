;;; auto-yasnippet-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "auto-yasnippet" "auto-yasnippet.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from auto-yasnippet.el

(autoload 'aya-create-one-line "auto-yasnippet" "\
A simplistic `aya-create' to create only one mirror.
You can still have as many instances of this mirror as you want.
It's less flexible than `aya-create', but faster.
It uses a different marker, which is `aya-marker-one-line'.
You can use it to quickly generate one-liners such as
menu.add_item(spamspamspam, \"spamspamspam\")" t nil)

(autoload 'aya-create "auto-yasnippet" "\
Create a snippet from the text between BEG and END.
When the bounds are not given, use either the current region or line.

Remove `aya-marker' prefixes, write the corresponding snippet to
`aya-current', with words prefixed by `aya-marker' as fields, and
mirrors properly set up.

\(fn &optional BEG END)" t nil)

(autoload 'aya-expand "auto-yasnippet" "\
Insert the last yasnippet created by `aya-create'." t nil)

(autoload 'aya-open-line "auto-yasnippet" "\
Call `open-line', unless there are abbrevs or snippets at point.
In that case expand them.  If there's a snippet expansion in progress,
move to the next field.  Call `open-line' if nothing else applies." t nil)

(autoload 'aya-yank-snippet "auto-yasnippet" "\
Insert current snippet at point.
To save a snippet permanently, create an empty file and call this." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "auto-yasnippet" '("aya-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-yasnippet-autoloads.el ends here
