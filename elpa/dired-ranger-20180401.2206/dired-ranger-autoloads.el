;;; dired-ranger-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dired-ranger" "dired-ranger.el" (0 0 0 0))
;;; Generated autoloads from dired-ranger.el

(autoload 'dired-ranger-copy "dired-ranger" "\
Place the marked items in the copy ring.

With non-nil prefix argument, add the marked items to the current
selection.  This allows you to gather files from multiple dired
buffers for a single paste.

\(fn ARG)" t nil)

(autoload 'dired-ranger-paste "dired-ranger" "\
Copy the items from copy ring to current directory.

With raw prefix argument \\[universal-argument], do not remove
the selection from the stack so it can be copied again.

With numeric prefix argument, copy the n-th selection from the
copy ring.

\(fn ARG)" t nil)

(autoload 'dired-ranger-move "dired-ranger" "\
Move the items from copy ring to current directory.

This behaves like `dired-ranger-paste' but moves the files
instead of copying them.

\(fn ARG)" t nil)

(autoload 'dired-ranger-bookmark "dired-ranger" "\
Bookmark current dired buffer.

CHAR is a single character (a-zA-Z0-9) representing the bookmark.
Reusing a bookmark replaces the content.  These bookmarks are not
persistent, they are used for quick jumping back and forth
between currently used directories.

\(fn CHAR)" t nil)

(autoload 'dired-ranger-bookmark-visit "dired-ranger" "\
Visit bookmark CHAR.

If the associated dired buffer was killed, we try to reopen it
according to the setting `dired-ranger-bookmark-reopen'.

The special bookmark `dired-ranger-bookmark-LRU' always jumps to
the least recently visited dired buffer.

See also `dired-ranger-bookmark'.

\(fn CHAR)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-ranger" '("dired-ranger-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-ranger-autoloads.el ends here
