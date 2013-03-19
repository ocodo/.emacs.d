Usage:

(add-to-list 'load-path "/the/path/to/bdo")

Optional keybinding:

(define-key css-mode-map (kbd "C-x C-s") 'css-refresh)
(defun css-refresh ()
  "Refresh the current CSS file."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (bdo-refresh))
