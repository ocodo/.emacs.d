;;; dired-dups-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dired-dups" "dired-dups.el" (0 0 0 0))
;;; Generated autoloads from dired-dups.el

(autoload 'dired-find-duplicates "dired-dups" "\
Find duplicates of files and put them in a dired buffer.
FILES is a list of files which will be compared. DIR is the directory
which will be checked for duplicates of any of the files in the list.
Any matching files will be placed in a new dired buffer with name
*duplicated files*.
When called interactively from a dired buffer, the marked files in that dired buffer will
be treated as the orginals whose duplicates are to be found, and the user will be prompted
for a directory to search for duplicates.
If the function is called with 1 prefix arg then the original files that have duplicates
will be marked for deletion.
With 2 prefix args the files in the *duplicate files* buffer will be marked for deletion.
With 3 prefix args the original files will be placed in the *duplicated files* buffer,
interleaved with the duplicates (original file first, followed by duplicates), and 
the original files will be marked for deletion.
With 4 prefix args the behaviour is the same as with 3 prefix args except that the 
duplicate files will be marked for deletion instead of the originals.

\(fn FILES DIR)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-dups" '("dired-dups-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-dups-autoloads.el ends here
