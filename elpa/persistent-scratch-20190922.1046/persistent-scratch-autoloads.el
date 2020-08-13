;;; persistent-scratch-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "persistent-scratch" "persistent-scratch.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from persistent-scratch.el

(autoload 'persistent-scratch-save "persistent-scratch" "\
Save the current state of scratch buffers.
When FILE is non-nil, the state is saved to FILE; when nil or when called
interactively, the state is saved to `persistent-scratch-save-file'.
What state exactly is saved is determined by `persistent-scratch-what-to-save'.

When FILE is nil and `persistent-scratch-backup-directory' is non-nil, a copy of
`persistent-scratch-save-file' is stored in that directory, with a name
representing the time of the last `persistent-scratch-new-backup' call.

\(fn &optional FILE)" t nil)

(autoload 'persistent-scratch-save-to-file "persistent-scratch" "\
Save the current state of scratch buffers.
The state is saved to FILE.

When called interactively, prompt for the file name, which is the only
difference between this function and `persistent-scratch-save'.

See `persistent-scratch-save'.

\(fn FILE)" t nil)

(autoload 'persistent-scratch-restore "persistent-scratch" "\
Restore the scratch buffers.
Load FILE and restore all saved buffers to their saved state.

FILE is a file to restore scratch buffers from; when nil or when called
interactively, `persistent-scratch-save-file' is used.

This is a potentially destructive operation: if there's an open buffer with the
same name as a saved buffer, the contents of that buffer will be overwritten.

\(fn &optional FILE)" t nil)

(autoload 'persistent-scratch-restore-from-file "persistent-scratch" "\
Restore the scratch buffers from a file.
FILE is a file storing saved scratch buffer state.

When called interactively, prompt for the file name, which is the only
difference between this function and `persistent-scratch-restore'.

See `persistent-scratch-restore'.

\(fn FILE)" t nil)

(autoload 'persistent-scratch-mode "persistent-scratch" "\
Utility mode that remaps `save-buffer' and `write-file' to their
`persistent-scratch' equivalents.

If called interactively, enable Persistent-Scratch mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

This mode cannot be enabled in buffers for which
`persistent-scratch-scratch-buffer-p-function' is nil.

\\{persistent-scratch-mode-map}

\(fn &optional ARG)" t nil)

(defvar persistent-scratch-autosave-mode nil "\
Non-nil if Persistent-Scratch-Autosave mode is enabled.
See the `persistent-scratch-autosave-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `persistent-scratch-autosave-mode'.")

(custom-autoload 'persistent-scratch-autosave-mode "persistent-scratch" nil)

(autoload 'persistent-scratch-autosave-mode "persistent-scratch" "\
Autosave scratch buffer state.
Every `persistent-scratch-autosave-interval' seconds and when Emacs quits, the
state of all active scratch buffers is saved.
This uses `persistent-scratch-save', which see.

Toggle Persistent-Scratch-Autosave mode on or off.
With a prefix argument ARG, enable Persistent-Scratch-Autosave mode if ARG is
positive, and disable it otherwise. If called from Lisp, enable the mode if ARG
is omitted or nil, and toggle it if ARG is `toggle'.
\\{persistent-scratch-autosave-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'persistent-scratch-new-backup "persistent-scratch" "\
Create a new scratch buffer save backup file.
The next time `persistent-scratch-save' is called, it will create a new backup
file and use that file from now on." t nil)

(autoload 'persistent-scratch-setup-default "persistent-scratch" "\
Enable `persistent-scratch-autosave-mode' and restore the scratch buffers.
When an error occurs while restoring the scratch buffers, it's demoted to a
message." nil nil)

(autoload 'persistent-scratch-keep-n-newest-backups "persistent-scratch" "\
Return a backup filter that keeps N newest backups.
The returned function is suitable for `persistent-scratch-backup-filter'.

Note: this function assumes that increasing time values result in
lexicographically increasing file names when formatted using
`persistent-scratch-backup-file-name-format'.

\(fn N)" nil nil)

(autoload 'persistent-scratch-keep-backups-not-older-than "persistent-scratch" "\
Return a backup filter that keeps backups newer than DIFF.
DIFF may be either a number representing the number of second, or a time value
in the format returned by `current-time' or `seconds-to-time'.
The returned function is suitable for `persistent-scratch-backup-filter'.

Note: this function assumes that increasing time values result in
lexicographically increasing file names when formatted using
`persistent-scratch-backup-file-name-format'.

\(fn DIFF)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "persistent-scratch" '("persistent-scratch-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; persistent-scratch-autoloads.el ends here
