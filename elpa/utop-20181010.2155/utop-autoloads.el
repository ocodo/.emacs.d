;;; utop-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "utop" "utop.el" (0 0 0 0))
;;; Generated autoloads from utop.el

(autoload 'utop-minor-mode "utop" "\
Minor mode for utop.

\(fn &optional ARG)" t nil)

(autoload 'utop-mode "utop" "\
Set the buffer mode to utop.

\(fn)" t nil)

(autoload 'utop "utop" "\
A universal toplevel for OCaml.

url: https://forge.ocamlcore.org/projects/utop/

utop is a enhanced toplevel for OCaml with many features,
including context sensitive completion.

This is the emacs frontend for utop. You can use the utop buffer
as a standard OCaml toplevel.

To complete an identifier, simply press TAB.

Special keys for utop:
\\{utop-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "utop" '("utop-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; utop-autoloads.el ends here
