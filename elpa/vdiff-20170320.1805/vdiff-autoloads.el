;;; vdiff-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "vdiff" "vdiff.el" (0 0 0 0))
;;; Generated autoloads from vdiff.el

(autoload 'vdiff-files "vdiff" "\
Start a vdiff session. If called interactively, you will be
asked to select two files. HORIZONTAL adjusts the buffer's
initial layout. A prefix argument can be used to set this
variable interactively. ON-QUIT is a function to run on exiting
the vdiff session. It is called with the two vdiff buffers as
arguments.

\(fn FILE-A FILE-B &optional HORIZONTAL ON-QUIT)" t nil)

(autoload 'vdiff-buffers "vdiff" "\
Start a vdiff session. If called interactively, you will be
asked to select two buffers. HORIZONTAL adjusts the buffer's
initial layout. A prefix argument can be used to set this
variable interactively. ON-QUIT is a function to run on exiting
the vdiff session. It is called with the two vdiff buffers as
arguments. The last two options, RESTORE-WINDOWS-ON-QUIT and
KILL-BUFFERS-ON-QUIT restore the previous window configuration
and kill the vdiff buffers after quitting vdiff. Note that if you
are going to kill the buffers you should probably be using a
function for ON-QUIT to do something useful with the result.

\(fn BUFFER-A BUFFER-B &optional HORIZONTAL ON-QUIT RESTORE-WINDOWS-ON-QUIT KILL-BUFFERS-ON-QUIT)" t nil)

(autoload 'vdiff-buffers3 "vdiff" "\
Start a vdiff session. If called interactively, you will be
asked to select two buffers. ON-QUIT is a function to run on
exiting the vdiff session. It is called with the three vdiff
buffers as arguments. The last two options, RESTORE-WINDOWS-ON-QUIT and
KILL-BUFFERS-ON-QUIT restore the previous window configuration
and kill the vdiff buffers after quitting vdiff. Note that if you
are going to kill the buffers you should probably be using a
function for ON-QUIT to do something useful with the result.

\(fn BUFFER-A BUFFER-B BUFFER-C &optional ON-QUIT RESTORE-WINDOWS-ON-QUIT KILL-BUFFERS-ON-QUIT)" t nil)

(autoload 'vdiff-merge-conflict "vdiff" "\
Start vdiff session using merge conflicts marked in FILE.

\(fn FILE &optional RESTORE-WINDOWS-ON-QUIT)" t nil)

(autoload 'vdiff-files3 "vdiff" "\
Start a vdiff session with 3 files. If called interactively,
you will be asked to select two files.

\(fn FILE-A FILE-B FILE-C &optional ON-QUIT)" t nil)

(autoload 'vdiff-current-file "vdiff" "\
Start vdiff between current buffer and its file on disk.
This command can be used instead of `revert-buffer'.  If there is
nothing to revert then this command fails.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vdiff" '("vdiff-")))

;;;***

;;;### (autoloads nil "vdiff-magit" "vdiff-magit.el" (0 0 0 0))
;;; Generated autoloads from vdiff-magit.el
 (autoload 'vdiff-magit-popup "vdiff-magit" nil t)

(autoload 'vdiff-magit-resolve "vdiff-magit" "\
Resolve outstanding conflicts in FILE using vdiff.
FILE has to be relative to the top directory of the repository.

In the rare event that you want to manually resolve all
conflicts, including those already resolved by Git, use
`ediff-merge-revisions-with-ancestor'.

\(fn FILE)" t nil)

(autoload 'vdiff-magit-stage "vdiff-magit" "\
Stage and unstage changes to FILE using vdiff.
FILE has to be relative to the top directory of the repository.

\(fn FILE)" t nil)

(autoload 'vdiff-magit-dwim "vdiff-magit" "\
Compare, stage, or resolve using vdiff.

This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using vdiff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `vdiff-magit-*'
command has to be used explicitly.  If it cannot read the user's
mind at all, then it asks the user for a command to run.

\(fn)" t nil)

(autoload 'vdiff-magit-show-unstaged "vdiff-magit" "\
Show unstaged changes using vdiff.

This only allows looking at the changes; to stage, unstage,
and discard changes using vdiff, use `vdiff-magit-stage'.

FILE must be relative to the top directory of the repository.

\(fn FILE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vdiff-magit" '("vdiff-magit-")))

;;;***

;;;### (autoloads nil nil ("vdiff-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; vdiff-autoloads.el ends here
