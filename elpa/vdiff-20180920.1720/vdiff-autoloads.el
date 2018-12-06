;;; vdiff-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vdiff" "vdiff.el" (0 0 0 0))
;;; Generated autoloads from vdiff.el

(autoload 'vdiff-files "vdiff" "\
Start a vdiff session. If called interactively, you will be
asked to select two files. ROTATE adjusts the buffer's
initial layout. A prefix argument can be used to set this
variable interactively. ON-QUIT is a function to run on exiting
the vdiff session. It is called with the two vdiff buffers as
arguments.

\(fn FILE-A FILE-B &optional ROTATE ON-QUIT)" t nil)

(autoload 'vdiff-buffers "vdiff" "\
Start a vdiff session. If called interactively, you will be
asked to select two buffers. ROTATE adjusts the buffer's
initial layout. A prefix argument can be used to set this
variable interactively. ON-QUIT is a function to run on exiting
the vdiff session. It is called with the two vdiff buffers as
arguments. The last two options, RESTORE-WINDOWS-ON-QUIT and
KILL-BUFFERS-ON-QUIT restore the previous window configuration
and kill the vdiff buffers after quitting vdiff. Note that if you
are going to kill the buffers you should probably be using a
function for ON-QUIT to do something useful with the result.

\(fn BUFFER-A BUFFER-B &optional ROTATE ON-QUIT RESTORE-WINDOWS-ON-QUIT KILL-BUFFERS-ON-QUIT)" t nil)

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

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vdiff-autoloads.el ends here
