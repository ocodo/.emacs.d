;;; evil-surround-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "evil-surround" "evil-surround.el" (22483 30691
;;;;;;  672086 963000))
;;; Generated autoloads from evil-surround.el

(evil-define-command evil-surround-delete (char &optional outer inner) "Delete the surrounding delimiters represented by CHAR.\nAlternatively, the text to delete can be represented with\nthe overlays OUTER and INNER, where OUTER includes the delimiters\nand INNER excludes them. The intersection (i.e., difference)\nbetween these overlays is what is deleted." (interactive "<C>") (cond ((and outer inner) (delete-region (overlay-start outer) (overlay-start inner)) (delete-region (overlay-end inner) (overlay-end outer)) (goto-char (overlay-start outer))) (t (let* ((outer (evil-surround-outer-overlay char)) (inner (evil-surround-inner-overlay char))) (unwind-protect (when (and outer inner) (evil-surround-delete char outer inner)) (when outer (delete-overlay outer)) (when inner (delete-overlay inner)))))))

(evil-define-command evil-surround-change (char &optional outer inner) "Change the surrounding delimiters represented by CHAR.\nAlternatively, the text to delete can be represented with the\noverlays OUTER and INNER, which are passed to `evil-surround-delete'." (interactive "<C>") (cond ((and outer inner) (evil-surround-delete char outer inner) (let ((key (evil-read-key))) (evil-surround-region (overlay-start outer) (overlay-end outer) nil (if (evil-surround-valid-char-p key) key char)))) (t (let* ((outer (evil-surround-outer-overlay char)) (inner (evil-surround-inner-overlay char))) (unwind-protect (when (and outer inner) (evil-surround-change char outer inner)) (when outer (delete-overlay outer)) (when inner (delete-overlay inner)))))))

(autoload 'evil-surround-mode "evil-surround" "\
Buffer-local minor mode to emulate surround.vim.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-surround-mode "evil-surround" "\
Enable evil-surround-mode in the current buffer.

\(fn)" nil nil)

(autoload 'turn-off-evil-surround-mode "evil-surround" "\
Disable evil-surround-mode in the current buffer.

\(fn)" nil nil)

(defvar global-evil-surround-mode nil "\
Non-nil if Global Evil-Surround mode is enabled.
See the `global-evil-surround-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-surround-mode'.")

(custom-autoload 'global-evil-surround-mode "evil-surround" nil)

(autoload 'global-evil-surround-mode "evil-surround" "\
Toggle Evil-Surround mode in all buffers.
With prefix ARG, enable Global Evil-Surround mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Surround mode is enabled in all buffers where
`turn-on-evil-surround-mode' would do it.
See `evil-surround-mode' for more information on Evil-Surround mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-surround-autoloads.el ends here
