;;; lorem-ipsum-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lorem-ipsum" "lorem-ipsum.el" (0 0 0 0))
;;; Generated autoloads from lorem-ipsum.el

(autoload 'lorem-ipsum-use-default-bindings "lorem-ipsum" "\
Use the default keybindings of C-c l [spl]." t nil)

(autoload 'lorem-ipsum-insert-paragraphs "lorem-ipsum" "\
Insert lorem ipsum paragraphs into buffer.
If NUM is non-nil, insert NUM paragraphs.

\(fn &optional NUM)" t nil)

(defalias 'Lorem-ipsum-insert-paragraphs 'lorem-ipsum-insert-paragraphs)

(autoload 'lorem-ipsum-insert-sentences "lorem-ipsum" "\
Insert lorem ipsum sentences into buffer.
If NUM is non-nil, insert NUM sentences.

\(fn &optional NUM)" t nil)

(defalias 'Lorem-ipsum-insert-sentences 'lorem-ipsum-insert-sentences)

(autoload 'lorem-ipsum-insert-list "lorem-ipsum" "\
Insert lorem ipsum list items into buffer.
If NUM is non-nil, insert NUM list items.

\(fn &optional NUM)" t nil)

(defalias 'Lorem-ipsum-insert-list 'lorem-ipsum-insert-list)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lorem-ipsum" '("lorem-ipsum-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lorem-ipsum-autoloads.el ends here
