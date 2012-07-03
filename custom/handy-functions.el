;; Handy functions, add little helpers in here.

(defun now-is ()
    "Display current time."
    (interactive)
    (insert (format-time-string "%l:%M%P(%z) %Y-%m-%d")))

(provide 'handy-functions)
