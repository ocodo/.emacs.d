;;; tuareg-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ocamldebug" "ocamldebug.el" (0 0 0 0))
;;; Generated autoloads from ocamldebug.el

(autoload 'ocamldebug "ocamldebug" "\
Run ocamldebug on program FILE in buffer *ocamldebug-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for ocamldebug.  If you wish to change this, use
the ocamldebug commands `cd DIR' and `directory'.

\(fn PATH)" t nil)

(defalias 'camldebug 'ocamldebug)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ocamldebug" '("ocamldebug-" "def-ocamldebug")))

;;;***

;;;### (autoloads nil "tuareg" "tuareg.el" (0 0 0 0))
;;; Generated autoloads from tuareg.el
(add-to-list 'auto-mode-alist '("\\.ml[ip]?\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.eliomi?\\'" . tuareg-mode))
(dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"
               ".annot" ".cmt" ".cmti"))
 (add-to-list 'completion-ignored-extensions ext))

(autoload 'tuareg-mode "tuareg" "\
Major mode for editing OCaml code.

Dedicated to Emacs and XEmacs, version 21 and higher.  Provides
automatic indentation and compilation interface.  Performs font/color
highlighting using Font-Lock.  It is designed for OCaml but handles
Caml Light as well.

The Font-Lock minor-mode is used according to your customization
options.

You have better byte-compile tuareg.el.

For customization purposes, you should use `tuareg-mode-hook'
\(run for every file) or `tuareg-load-hook' (run once) and not patch
the mode itself.  You should add to your configuration file something like:
  (add-hook 'tuareg-mode-hook
            (lambda ()
               ... ; your customization code
            ))
For example you can change the indentation of some keywords, the
`electric' flags, Font-Lock colors... Every customizable variable is
documented, use `C-h-v' or look at the mode's source code.

`dot-emacs.el' is a sample customization file for standard changes.
You can append it to your `.emacs' or use it as a tutorial.

`M-x ocamldebug' FILE starts the OCaml debugger ocamldebug on the executable
FILE, with input and output in an Emacs buffer named *ocamldebug-FILE*.

A Tuareg Interactive Mode to evaluate expressions in a toplevel is
included.  Type `M-x tuareg-run-ocaml' or simply `M-x run-ocaml' or see
special-keys below.

Short cuts for the Tuareg mode:
\\{tuareg-mode-map}

Short cuts for interactions with the toplevel:
\\{tuareg-interactive-mode-map}

\(fn)" t nil)

(autoload 'tuareg-run-ocaml "tuareg" "\
Run an OCaml toplevel process.  I/O via buffer `*ocaml-toplevel*'.

\(fn)" t nil)

(defalias 'run-ocaml 'tuareg-run-ocaml)

(add-to-list 'interpreter-mode-alist '("ocamlrun" . tuareg-mode))

(add-to-list 'interpreter-mode-alist '("ocaml" . tuareg-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tuareg" '("tuareg-")))

;;;***

;;;### (autoloads nil "tuareg-light" "tuareg-light.el" (0 0 0 0))
;;; Generated autoloads from tuareg-light.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tuareg-light" '("tuareg2-")))

;;;***

;;;### (autoloads nil "tuareg-site-file" "tuareg-site-file.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from tuareg-site-file.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tuareg-site-file" '("run-ocaml" "camldebug")))

;;;***

;;;### (autoloads nil "tuareg_indent" "tuareg_indent.el" (0 0 0 0))
;;; Generated autoloads from tuareg_indent.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tuareg_indent" '("tuareg-")))

;;;***

;;;### (autoloads nil nil ("tuareg-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; tuareg-autoloads.el ends here
