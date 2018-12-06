;;; basic-c-compile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "basic-c-compile" "basic-c-compile.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from basic-c-compile.el

(autoload 'basic-c-compile-makefile "basic-c-compile" "\
Create a Makefile of the form shown in README.org.
This function uses the variables `basic-c-compile-compiler',
`basic-c-compile-all-files' and `basic-c-compile-compiler-flags'.
It uses `basic-c-compile--files-to-compile' in conjunction with
`basic-c-compiler-all-files' to determine files to be the
Makefile's INFILE.

\(fn)" t nil)

(autoload 'basic-c-compile-file "basic-c-compile" "\
Compile file with or without a Makefile.
A y-or-n prompt is called to determine if you want to use the
Makefile of not.  If you say yes ('y') and there is no Makefile
in the directory then one is make using
`basic-c-compile--makefile'.  The presence of a outfile is
check for, if there is not one then 'rebuild' is called,
otherwise 'build' is called.

\(fn)" t nil)

(autoload 'basic-c-compile-run-c "basic-c-compile" "\
Run the program.
If the C source file is new than the outfile and
`basic-c-compile-auto-comp' is true, then the file will be
compiled before it is run.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "basic-c-compile" '("basic-c-compile-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; basic-c-compile-autoloads.el ends here
