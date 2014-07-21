;; Safe cleanup, probably a package?
;;;###autoload
(defun safe-buffer-cleanup ()
  "Clean whitespace, kill tabs, set to UTF8"
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook 'safe-buffer-cleanup)

(provide 'init-buffer-clean)
