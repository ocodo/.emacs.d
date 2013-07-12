;;; find-things-fast-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ftf-gdb ftf-compile ftf-find-file ftf-grepsource)
;;;;;;  "find-things-fast" "find-things-fast.el" (20959 60141))
;;; Generated autoloads from find-things-fast.el

(autoload 'ftf-grepsource "find-things-fast" "\
Greps the current project, leveraging local repository data
for speed and falling back on a big \"find | xargs grep\"
command if we aren't.

The project's scope is defined first as a directory containing
either a `.dir-locals.el' file or an `.emacs-project' file OR the
root of the current git repository OR a project root defined by
the optional `project-root.el' package OR the default directory
if none of the above is found.

\(fn CMD-ARGS)" t nil)

(autoload 'ftf-find-file "find-things-fast" "\
Prompt with a completing list of all files in the project to find one.

The project's scope is defined first as a directory containing
either a `.dir-locals.el' file or an `.emacs-project' file OR the
root of the current git repository OR a project root defined by
the optional `project-root.el' package OR the default directory
if none of the above is found.

\(fn)" t nil)

(autoload 'ftf-compile "find-things-fast" "\
Run the `compile' function from the project root.

\(fn)" t nil)

(autoload 'ftf-gdb "find-things-fast" "\
Run the `gdb' function from the project root.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("find-things-fast-pkg.el") (20959 60141
;;;;;;  603998))

;;;***

(provide 'find-things-fast-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; find-things-fast-autoloads.el ends here
