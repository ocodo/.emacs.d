;;; use-buffer-clean --- Safe cleanup, probably a package?
;;; Commentary:
;;  Unless the buffer is a Makefile untabify it.
;;
;;  For every buffer, kill trailing whitespace and set the buffer file
;;  coding as utf8.
;;
;;; Code:

;; TODO: add a buffer local var via on load hook to flag a file which
;; is already tabbed, we should not untabify these.

(defcustom suspend-whitespace-cleanup nil "Suspend buffer cleanup if t.")

(defcustom skip-buffer-untabify nil "Skip untabify buffer on save if t.")

(defun safe-buffer-cleanup ()
  "Clean whitespace, kill tabs, set to UTF8."
  (unless suspend-whitespace-cleanup
    (unless (or skip-buffer-untabify (s-contains-p "makefile" (symbol-name (with-current-buffer (current-buffer) major-mode))))
      (untabify (point-min) (point-max)))
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8)))

(add-hook 'before-save-hook 'safe-buffer-cleanup)

(provide 'use-buffer-clean)
;;; use-buffer-clean.el ends here
