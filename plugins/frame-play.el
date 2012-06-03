
(defun set-frame-position-right-hand-side ()
  "Position the current frame to the right hand side of the Display"
  (interactive)
  (if window-system
      (progn 
        (set-frame-position (selected-frame) (- (x-display-pixel-width) (frame-pixel-width)) 0)))) 

(defun set-frame-position-left-hand-side ()
  "Position the current frame to the right hand side of the Display"
  (interactive)
  (if window-system
      (progn 
        (set-frame-position (selected-frame) 0 0))))

(defun set-frame-height-to-display-height ()
  "Size the current frame height to match display height"
  (interactive)
  (if window-system
      (progn 
        (set-frame-height (selected-frame) (/ (x-display-pixel-height) (frame-char-height))))))

(defun set-frame-height-to-85-percent-display-height ()
  "Size the current frame height to 85% of display height"
  (interactive)
  (if window-system
      (progn 
        (set-frame-height (selected-frame) (floor (* (/ (x-display-pixel-height) (frame-char-height)) 0.85))))))

(defun set-frame-width-to-two-thirds-display-width ()
  "Size the current frame to two thirds the display width."
  (interactive)
  (if window-system
      (progn 
        (set-frame-width (selected-frame) (/ (round (* (x-display-pixel-width) 0.66)) (frame-char-width))))))

(defun set-frame-width-to-three-quarters-display-width ()
  "Size the current frame to three quarters the display width."
  (interactive)
  (if window-system
      (progn 
        (set-frame-width (selected-frame) (/ (round (* (x-display-pixel-width) 0.75)) (frame-char-width))))))

(defun set-frame-width-to-half-display-width ()
  "Size the current frame to half the display width."
  (interactive)
  (if window-system
      (progn
        (set-frame-width (selected-frame) (/ (/ (x-display-pixel-width) 2) (frame-char-width))))))

(defun set-frame-width-to-half-display-width ()
  "Size the current frame to half the display width."
  (interactive)
  (if window-system
      (progn
        (set-frame-width (selected-frame) (/ (/ (x-display-pixel-width) 2) (frame-char-width))))))

(defun set-frame-width-to-display-width ()
  "Size the current frame to half the display width."
  (interactive)
  (if window-system
      (progn
        (set-frame-width (selected-frame) (/ (x-display-pixel-width) (frame-char-width))))))


(defun big-left-1 ()
  "thisandthat."
  (interactive)
  (if window-system
      (progn 
        (set-frame-height-to-display-height)
        (set-frame-width-to-two-thirds-display-width)
        (set-frame-position-left-hand-side)
        )))


(defun big-left-2 ()
  "thisandthat."
  (interactive)
  (if window-system
      (progn 
        (set-frame-height-to-display-height)
        (set-frame-width-to-three-quarters-display-width)
        (set-frame-position-left-hand-side)
        )))

(defun big-left-3 ()
  "thisandthat."
  (interactive)
  (if window-system
      (progn 
        (set-frame-height-to-85-percent-display-height)
        (set-frame-width-to-three-quarters-display-width)
        (set-frame-position-left-hand-side)
        )))



(defun big-right-1 ()
  "thisandthat."
  (interactive)
  (if window-system
      (progn 
        (set-frame-height-to-display-height)
        (set-frame-width-to-two-thirds-display-width)
        (set-frame-position-right-hand-side)
        )))


(defun big-right-2 ()
  "thisandthat."
  (interactive)
  (if window-system
      (progn 
        (set-frame-height-to-display-height)
        (set-frame-width-to-three-quarters-display-width)
        (set-frame-position-right-hand-side)
        )))

(defun big-right-3 ()
  "thisandthat."
  (interactive)
  (if window-system
      (progn 
        (set-frame-height-to-85-percent-display-height)
        (set-frame-width-to-three-quarters-display-width)
        (set-frame-position-right-hand-side)
        )))


(provide 'frame-play)
