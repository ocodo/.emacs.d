;;; filladapt-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "filladapt" "filladapt.el" (0 0 0 0))
;;; Generated autoloads from filladapt.el

(autoload 'filladapt-mode "filladapt" "\
Toggle Filladapt minor mode.
With arg, turn Filladapt mode on iff arg is positive.  When
Filladapt mode is enabled, auto-fill-mode and the fill-paragraph
command are both smarter about guessing a proper fill-prefix and
finding paragraph boundaries when bulleted and indented lines and
paragraphs are used.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "filladapt" '("filladapt-" "turn-off-filladapt-mode")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; filladapt-autoloads.el ends here
