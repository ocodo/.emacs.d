;;; help-mode+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "help-mode+" "help-mode+.el" (0 0 0 0))
;;; Generated autoloads from help-mode+.el

(when (< emacs-major-version 24) (defun help-mode nil "Major mode for viewing help text and navigating references in it.\nEntry to this mode runs the normal hook `help-mode-hook'.\nCommands:\n\\{help-mode-map}" (interactive) (kill-all-local-variables) (use-local-map help-mode-map) (setq mode-name "Help" major-mode 'help-mode) (view-mode) (make-local-variable 'view-no-disable-on-exit) (setq view-no-disable-on-exit t view-exit-action (lambda (buffer) (or (window-minibuffer-p (selected-window)) (when (eq (window-buffer) (get-buffer "*Help*")) (if (one-window-p t) (delete-frame) (delete-window)))))) (run-mode-hooks 'help-mode-hook)))

(autoload 'help-make-xrefs "help-mode+" "\
Parse and hyperlink documentation cross-references in the given BUFFER.

Find cross-reference information in a buffer and activate such cross
references for selection with `help-follow'.  Cross-references have
the canonical form `...'  and the type of reference may be
disambiguated by the preceding word(s) used in
`help-xref-symbol-regexp'.  Faces only get cross-referenced if
preceded or followed by the word `face'.  Variables without
variable documentation do not get cross-referenced, unless
preceded by the word `variable' or `option'.

If the variable `help-xref-mule-regexp' is non-nil, find also
cross-reference information related to multilingual environment
\(e.g., coding-systems).  This variable is also used to disambiguate
the type of reference as the same way as `help-xref-symbol-regexp'.

A special reference `back' is made to return back through a stack of
help buffers.  Variable `help-back-label' specifies the text for
that.

\(fn &optional BUFFER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "help-mode+" '("help-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; help-mode+-autoloads.el ends here
