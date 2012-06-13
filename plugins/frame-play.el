;; a simple set of frame positioning macros...
;; frame width and height 100% values are approximate,
;; and tuned for Emacs24.1 on OS X 

(defun set-frame-position-right-hand-side ()

  "Position the current frame to the top/right of the Display"

  (interactive)
  (if window-system
      (progn 
        (set-frame-position (selected-frame) (- (x-display-pixel-width) (frame-pixel-width) ) 0)))) 

(defun set-frame-position-left-hand-side ()

  "Position the current frame to the top/left of the Display"

  (interactive)
  (if window-system
      (progn 
        (set-frame-position (selected-frame) 0 0))))

(defun set-frame-height-to-display-height ()

  "Size the current frame height to match display height - nb:approximately, Emacs sets frame height by char rows."

  (interactive)
  (if window-system
      (progn 
        (set-frame-height (selected-frame) (/ (- (x-display-pixel-height) 50) (frame-char-height) )))))


(defun set-frame-height-to-85-percent-display-height ()

  "Size the current frame height to 85% of display height - nb:approximately, Emacs sets frame height by char rows."

  (interactive)
  (if window-system
      (progn 
        (set-frame-height (selected-frame) (floor (* (/ (x-display-pixel-height) (frame-char-height)) 0.85))))))


(defun set-frame-width-to-two-thirds-display-width ()

  "Size the current frame to two thirds the display width. - nb:approximately, Emacs sets frame width by char columns."

  (interactive)
  (if window-system
      (progn 
        (set-frame-width (selected-frame) (/ (round (* (x-display-pixel-width) 0.66)) (frame-char-width))))))

(defun set-frame-width-to-three-quarters-display-width ()

  "Size the current frame to three quarters the display width. - nb:approximately, Emacs sets frame width by char columns."

  (interactive)
  (if window-system
      (progn 
        (set-frame-width (selected-frame) (/ (round (* (x-display-pixel-width) 0.75)) (frame-char-width))))))

(defun set-frame-width-to-half-display-width ()

  "Size the current frame to half the display width. - nb:approximately, Emacs sets frame width by char columns."

  (interactive)
  (if window-system
      (progn
        (set-frame-width (selected-frame) (/ (/ (x-display-pixel-width) 2) (frame-char-width))))))

(defun set-frame-width-to-display-width ()

  "Size the current frame to half the display width. - nb:approximately, Emacs sets frame width by char columns."

  (interactive)
  (if window-system
      (progn
        (set-frame-width (selected-frame) (/ (- (x-display-pixel-width) 30) (frame-char-width) )))))


(defun set-frame-big-left-1 ()

  "A convenient preset frame size & position - 66% width, 100% height, top/left position."

  (interactive)
  (if window-system
      (progn 
        (set-frame-height-to-display-height)
        (set-frame-width-to-two-thirds-display-width)
        (set-frame-position-left-hand-side)
        )))


(defun set-frame-big-left-2 ()

  "A convenient preset frame size & position - 75% width, 100% height, top/left position."

  (interactive)
  (if window-system
      (progn 
        (set-frame-height-to-display-height)
        (set-frame-width-to-three-quarters-display-width)
        (set-frame-position-left-hand-side)
        )))

(defun set-frame-big-left-3 ()

  "A convenient preset frame size & position - 75% width, 85% height, top/left position."

  (interactive)
  (if window-system
      (progn 
        (set-frame-height-to-85-percent-display-height)
        (set-frame-width-to-three-quarters-display-width)
        (set-frame-position-left-hand-side)
        )))

(defun set-frame-big-right-1 ()

  "A convenient preset frame size & position - 66% width, 100% height, top/right position."

  (interactive)
  (if window-system
      (progn 
        (set-frame-height-to-display-height)
        (set-frame-width-to-two-thirds-display-width)
        (set-frame-position-right-hand-side)
        )))

(defun set-frame-big-right-2 ()

  "A convenient preset frame size & position - 75% width, 100% height, top/right position."

  (interactive)
  (if window-system
      (progn 
        (set-frame-height-to-display-height)
        (set-frame-width-to-three-quarters-display-width)
        (set-frame-position-right-hand-side)
        )))

(defun set-frame-big-right-3 ()

  "A convenient preset frame size & position - 75% width, 85% height, top/right position."

  (interactive)
  (if window-system
      (progn 
        (set-frame-height-to-85-percent-display-height)
        (set-frame-width-to-three-quarters-display-width)
        (set-frame-position-right-hand-side)
        )))

(provide 'frame-play)
