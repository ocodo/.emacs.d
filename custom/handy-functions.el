(defun align-number-right (begin end)
  "Align region to equal signs"
   (interactive "r")
   (align-regexp begin end ".* \\([0-9]+\\).*" -1 1 nil))
;; Handy functions, add little helpers in here.
(defun insert-random-in-range (start end)         
  (interactive "nRange start:\nnRange end:")
  (insert (format "%i" (random-in-range start end))))

(defun insert-random-radian () 
  "insert a radian value from 0 to 6.28318 (2PI)"
  (interactive)
  (insert (format "%s" (* (/ float-pi 180) (random 361)))))

(defun fraction-radian (denominator)
  (interactive "nDenomiator:")
  (insert (format "%s" (/ (* float-pi 2) denominator))))

(defun random-in-range (start end) 
  (random t)
  (+ start (random (+ 1 (- end start)))))

(defun insert-radians-for-degrees (degrees)
  "Insert radians for degrees"
  (interactive "ndegrees:")
  (insert (format "%s" (* (/ float-pi 180) degrees))))

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
