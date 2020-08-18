;;; ivy-ycmd-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-ycmd" "ivy-ycmd.el" (0 0 0 0))
;;; Generated autoloads from ivy-ycmd.el

(autoload 'ivy-ycmd-goto-references "ivy-ycmd" "\
Jump to a reference to the symbol at the current point.

This finds all references to the symbol at point, lists them with
ivy, and jumps to the one selected by the user." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-ycmd" '("ivy-ycmd--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-ycmd-autoloads.el ends here
