;;; format-all-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "format-all" "format-all.el" (23360 15311 416636
;;;;;;  515000))
;;; Generated autoloads from format-all.el

(autoload 'format-all-buffer "format-all" "\
Auto-format the source code in the current buffer.

No disk files are touched - the buffer doesn't even need to be
saved.  If you don't like the results of the formatting, you can
use ordinary undo to get your code back to its previous state.

You will need to install external programs to do the formatting.
If the command can't find the program that it needs, it will try
to tell you how you might be able to install it on your operating
system.  Only Emacs Lisp is formatted without an external program.

A suitable formatter is selected according to the `major-mode' of
the buffer.  Many popular programming languages are supported,
but not all of them by any means, so unfortunately it's still
likely that your favorite language is missing.  It is fairly easy
to add new languages that have an external formatter.

Any errors/warnings encountered during formatting are shown in a
buffer called *format-all-errors*.  If the formatter made any
changes to the code, point is placed at the first change.

\(fn)" t nil)

(autoload 'format-all-mode "format-all" "\
Toggle automatic source code formatting before save.

When the Format-All minor mode is enabled, `format-all-buffer' is
automatically called each time before you save the buffer.

When called from Lisp, the mode is toggled if ARG is ‘toggle’,
disabled if ARG is a negative integer or zero, and enabled
otherwise.

The mode is buffer-local and needs to be enabled separately each
time a file is visited or a temporary buffer is created.

You may want to use `add-hook' to add a function to your personal
`after-change-major-mode-hook' in your `user-init-file' to enable
the mode based on the buffer's `major-mode' and some
`buffer-file-name' patterns. For example:

    (defun my-after-change-major-mode ()
      (format-all-mode
       (if (and (buffer-file-name)
                (save-match-data
                  (let ((dir (file-name-directory (buffer-file-name))))
                    (or (string-match \"foo\" dir)
                        (string-match \"bar\" dir))))
                (member major-mode '(js-mode python-mode)))
           1 0)))

    (add-hook 'after-change-major-mode-hook 'my-after-change-major-mode)

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; format-all-autoloads.el ends here
