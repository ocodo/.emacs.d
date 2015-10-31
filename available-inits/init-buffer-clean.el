;;; init-buffer-clean --- Safe cleanup, probably a package?
;;; Commentary:
;;  Unless the buffer is a Makefile untabify it.
;;
;;  For every buffer, kill trailing whitespace and set the buffer file
;;  coding as utf8.
;;
;;; Code:

; TODO: add a buffer local var via on load hook to flag a file which
; is already tabbed, we should not untabify these.

(defun safe-buffer-cleanup ()
  "Clean whitespace, kill tabs, set to UTF8."
  (unless (eq (with-current-buffer (current-buffer) major-mode) 'makefile-bsdmake-mode)
    (untabify (point-min) (point-max)))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook 'safe-buffer-cleanup)

(provide 'init-buffer-clean)
;;; init-buffer-clean.el ends here
