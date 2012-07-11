;; Handy functions, add little helpers in here.

(defun now-is ()
    "Display current time."
    (interactive)
    (insert (format-time-string "%l:%M%P(%z) %Y-%m-%d")))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(provide 'handy-functions)
