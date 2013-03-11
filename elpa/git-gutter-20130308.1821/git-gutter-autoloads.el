;;; git-gutter-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (global-git-gutter-mode git-gutter-mode git-gutter:toggle
;;;;;;  git-gutter:clear git-gutter git-gutter:previous-hunk git-gutter:next-hunk
;;;;;;  git-gutter:popup-diff git-gutter:revert-hunk) "git-gutter"
;;;;;;  "git-gutter.el" (20795 51845))
;;; Generated autoloads from git-gutter.el

(autoload 'git-gutter:revert-hunk "git-gutter" "\
Revert current hunk.

\(fn)" t nil)

(autoload 'git-gutter:popup-diff "git-gutter" "\
popup current diff hunk

\(fn &optional DIFFINFO)" t nil)

(autoload 'git-gutter:next-hunk "git-gutter" "\
Move to next diff hunk

\(fn ARG)" t nil)

(autoload 'git-gutter:previous-hunk "git-gutter" "\
Move to previous diff hunk

\(fn ARG)" t nil)

(autoload 'git-gutter "git-gutter" "\
Show diff information in gutter

\(fn)" t nil)

(autoload 'git-gutter:clear "git-gutter" "\
clear diff information in gutter

\(fn)" t nil)

(autoload 'git-gutter:toggle "git-gutter" "\
toggle to show diff information

\(fn)" t nil)

(autoload 'git-gutter-mode "git-gutter" "\
Toggle Git-Gutter mode on or off.
With a prefix argument ARG, enable Git-Gutter mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{git-gutter-mode-map}

\(fn &optional ARG)" t nil)

(defvar global-git-gutter-mode nil "\
Non-nil if Global-Git-Gutter mode is enabled.
See the command `global-git-gutter-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-git-gutter-mode'.")

(custom-autoload 'global-git-gutter-mode "git-gutter" nil)

(autoload 'global-git-gutter-mode "git-gutter" "\
Toggle Git-Gutter mode in all buffers.
With prefix ARG, enable Global-Git-Gutter mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Git-Gutter mode is enabled in all buffers where
`(lambda nil (when (and (not (minibufferp)) (buffer-file-name)) (git-gutter-mode 1)))' would do it.
See `git-gutter-mode' for more information on Git-Gutter mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("git-gutter-pkg.el") (20795 51845 116557))

;;;***

(provide 'git-gutter-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; git-gutter-autoloads.el ends here
