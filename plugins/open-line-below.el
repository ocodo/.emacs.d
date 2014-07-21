(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline))

(global-set-key (kbd "C-o") 'open-line-below)
