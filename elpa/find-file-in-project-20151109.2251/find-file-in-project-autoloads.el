;;; find-file-in-project-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "find-file-in-project" "find-file-in-project.el"
;;;;;;  (22084 1969 380924 815000))
;;; Generated autoloads from find-file-in-project.el

(defvar ffip-filename-rules '(ffip-filename-identity ffip-filename-dashes-to-camelcase ffip-filename-camelcase-to-dashes))

(defvar ffip-find-executable nil "\
Path of GNU find. If nil, we will find `find' path automatically")

(defvar ffip-project-file '(".svn" ".git" ".hg") "\
The file that should be used to define a project root.
May be set using .dir-locals.el. Checks each entry if set to a list.")

(defvar ffip-prefer-ido-mode nil "\
Use ido-mode instead of ivy-mode for displaying candidates.")

(defvar ffip-patterns nil "\
List of patterns to look for with `find-file-in-project'.")

(defvar ffip-prune-patterns '("*/.git/*" "*/.svn/*" "*/.cvs/*" "*/.bzr/*" "*/.hg/*" "*.log" "*/bin/*" "*/.DS_Store/*" "*/tags" "*/TAGS" "*/GTAGS" "*/GPATH" "*/GRTAGS" "*/cscope.files" "*min.js" "*min.css" "*/node_modules/*" "*/bower_components/*" "*.png" "*.jpg" "*.jpeg" "*.gif" "*.bmp" "*.tiff" "*.doc" "*.docx" "*.pdf" "*.obj" "*.o" "*.a" "*.dylib" "*.lib" "*.d" "*.dll" "*.exe" "*/.metadata*" "*/.gradle/*" "*.class" "*.war" "*.jar" "*flymake" "*/#*#" ".#*" "*.swp" "*~" "*.elc" "*/.cask/*" "*.pyc") "\
List of directory/file patterns to not descend into when listing files in `find-file-in-project'.")

(defvar ffip-find-options "" "\
Extra options to pass to `find' when using `find-file-in-project'.

Use this to exclude portions of your project: \"-not -regex \\\".*svn.*\\\"\".")

(defvar ffip-project-root nil "\
If non-nil, overrides the project root directory location.")

(defvar ffip-project-root-function nil "\
If non-nil, this function is called to determine the project root.

This overrides variable `ffip-project-root' when set.")

(autoload 'ffip-project-root "find-file-in-project" "\
Return the root of the project.

\(fn)" nil nil)

(autoload 'ffip-filename-identity "find-file-in-project" "\
Return identical KEYWORD.

\(fn KEYWORD)" nil nil)

(autoload 'ffip-filename-camelcase-to-dashes "find-file-in-project" "\
 HelloWorld => hello-world

\(fn KEYWORD)" nil nil)

(autoload 'ffip-filename-dashes-to-camelcase "find-file-in-project" "\
 hello-world => HelloWorld

\(fn KEYWORD)" nil nil)

(autoload 'ffip-current-full-filename-match-pattern-p "find-file-in-project" "\
Is current full file name (including directory) match the REGEX?

\(fn REGEX)" nil nil)

(autoload 'find-file-in-project "find-file-in-project" "\
Prompt with a completing list of all files in the project to find one.
If NUM is given, only files modified NUM days before will be selected.

The project's scope is defined as the first directory containing
a `ffip-project-file' (It's value is \".git\" by default.

You can override this by setting the variable `ffip-project-root'.

\(fn &optional NUM)" t nil)

(autoload 'ffip-get-project-root-directory "find-file-in-project" "\
Get the full path of project root directory

\(fn)" nil nil)

(autoload 'find-file-in-project-by-selected "find-file-in-project" "\
Similar to find-file-in-project.
But use string from selected region to search files in the project.
If no region is selected, you need provide keyword.

Keyword could be ANY part of the file's full path and support wildcard.
For example, to find /home/john/proj1/test.js, below keywords are valid:
- test.js
- orj1/tes
- john*test

If NUM is given, only files modified NUM days before will be selected.

\(fn &optional NUM)" t nil)

(defalias 'ffip 'find-file-in-project)

(put 'ffip-patterns 'safe-local-variable 'listp)

(put 'ffip-prune-patterns 'safe-local-variable 'listp)

(put 'ffip-filename-rules 'safe-local-variable 'listp)

(put 'ffip-project-file 'safe-local-variable 'stringp)

(put 'ffip-project-root 'safe-local-variable 'stringp)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; find-file-in-project-autoloads.el ends here
