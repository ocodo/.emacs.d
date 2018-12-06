;;; git-gutter+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "git-gutter+" "git-gutter+.el" (0 0 0 0))
;;; Generated autoloads from git-gutter+.el

(autoload 'git-gutter+-mode "git-gutter+" "\
Git-Gutter mode

\(fn &optional ARG)" t nil)

(defvar global-git-gutter+-mode nil "\
Non-nil if Global Git-Gutter+ mode is enabled.
See the `global-git-gutter+-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-git-gutter+-mode'.")

(custom-autoload 'global-git-gutter+-mode "git-gutter+" nil)

(autoload 'global-git-gutter+-mode "git-gutter+" "\
Toggle Global Git-Gutter+ mode on or off.
With a prefix argument ARG, enable Global Git-Gutter+ mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{global-git-gutter+-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'git-gutter+-commit "git-gutter+" "\
Commit staged changes. If nothing is staged, ask to stage the current buffer.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-gutter+" '("git-gutter+-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; git-gutter+-autoloads.el ends here
