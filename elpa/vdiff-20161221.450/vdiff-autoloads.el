;;; vdiff-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "vdiff" "vdiff.el" (22624 31670 752044 242000))
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

(autoload 'vdiff-files3 "vdiff" "\
Start a vdiff session with 3 files. If called interactively,
you will be asked to select two files.

\(fn FILE-A FILE-B FILE-C &optional ON-QUIT)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; vdiff-autoloads.el ends here
