;;
;; a simple set of frame positioning macros...
;; frame width and height 100% values are approximate,
;; and tuned for Emacs24.1 on OS X - should work everywhere though.
;;
;; This collection will let you frig with the window position and
;; size, locking to the top left, or top right of the current
;; display. Sizing to max (char dimensions) and also a
;; set-frame-to-footer for editing while using the top half of the
;; screen for some other purpose, ie. playing video to be trasncribed
;; or just for use with the various Chrome or Firefox Textarea ->
;; Emacs extensions
;;

(defun set-frame-position-top-right-hand-side ()
  "Position the current frame to the top/right of the Display"
  (interactive)
  (if window-system
      (progn
        (set-frame-position (selected-frame) (- (x-display-pixel-width) (frame-pixel-width)) 0))))

(defun set-frame-position-top-left-hand-side ()
  "Position the current frame to the top/left of the Display"
  (interactive)
  (if window-system
      (progn
        (set-frame-position (selected-frame) 0 0))))

(defun set-frame-height-to-display-height ()
  "Size the current frame height to match display height -
nb:approximately, Emacs sets frame height by char rows."
  (interactive)
  (if window-system
      (progn
        (set-frame-height (selected-frame) (/ (- (x-display-pixel-height) 50) (frame-char-height)))
        )
    )
)

(defun set-frame-height-to-85-percent-display-height ()
  "Size the current frame height to 85% of display height -
nb:approximately, Emacs sets frame height by char rows."
  (interactive)
  (if window-system
      (progn
        (set-frame-height (selected-frame) (floor (* (/ (x-display-pixel-height) (frame-char-height)) 0.85))))))


(defun set-frame-width-to-two-thirds-display-width ()
  "Size the current frame to two thirds the display width. -
nb:approximately, Emacs sets frame width by char columns."
  (interactive)
  (if window-system
      (progn
        (set-frame-width (selected-frame) (/ (round (* (x-display-pixel-width) 0.66)) (frame-char-width))))))

(defun set-frame-width-to-three-quarters-display-width ()
  "Size the current frame to three quarters the display width. -
nb:approximately, Emacs sets frame width by char columns."
  (interactive)
  (if window-system
      (progn
        (set-frame-width (selected-frame) (/ (round (* (x-display-pixel-width) 0.75)) (frame-char-width))))))

(defun set-frame-width-to-half-display-width ()
  "Size the current frame to half the display width. -
nb:approximately, Emacs sets frame width by char columns."
  (interactive)
  (if window-system
      (progn
        (set-frame-width (selected-frame) (/ (/ (x-display-pixel-width) 2) (frame-char-width))))))

(defun set-frame-width-to-display-width ()
  "Size the current frame to half the display width. -
nb:approximately, Emacs sets frame width by char columns."
  (interactive)
  (if window-system
      (progn
        (set-frame-width (selected-frame) (/ (- (x-display-pixel-width) 30) (frame-char-width))))))

(defun set-frame-maximize ()
  "Quit worrying about ns-fullscreen or Lion's shitty native
fullscreen (aka. multiscreen hijack) and just maximize to the
current display."
  (interactive)
  (if window-system
      (progn
        (set-frame-position-top-left-hand-side)
        (set-frame-width-to-display-width)
        (set-frame-height-to-display-height))))

(defun set-frame-big-left-1 ()
  "A convenient preset frame size & position - 66% width, 100%
height, top/left position."
  (interactive)
  (if window-system
      (progn
        (set-frame-height-to-display-height)
        (set-frame-width-to-two-thirds-display-width)
        (set-frame-position-top-left-hand-side))))

(defun set-frame-big-left-2 ()
  "A convenient preset frame size & position - 75% width, 100%
height, top/left position."
  (interactive)
  (if window-system
      (progn
        (set-frame-height-to-display-height)
        (set-frame-width-to-three-quarters-display-width)
        (set-frame-position-top-left-hand-side))))

(defun set-frame-big-left-3 ()
  "A convenient preset frame size & position - 75% width, 85%
height, top/left position."
  (interactive)
  (if window-system
      (progn
        (set-frame-height-to-85-percent-display-height)
        (set-frame-width-to-three-quarters-display-width)
        (set-frame-position-top-left-hand-side)
        )))

(defun set-frame-big-right-1 ()
  "A convenient preset frame size & position - 66% width, 100%
height, top/right position."
  (interactive)
  (if window-system
      (progn
        (set-frame-height-to-display-height)
        (set-frame-width-to-two-thirds-display-width)
        (set-frame-position-top-right-hand-side)
        )))

(defun set-frame-big-right-2 ()
  "A convenient preset frame size & position - 75% width, 100%
height, top/right position."
  (interactive)
  (if window-system
      (progn
        (set-frame-height-to-display-height)
        (set-frame-width-to-three-quarters-display-width)
        (set-frame-position-top-right-hand-side)
        )))

(defun set-frame-big-right-3 ()
  "A convenient preset frame size & position - 75% width, 85%
height, top/right position."
  (interactive)
  (if window-system
      (progn
        (set-frame-height-to-85-percent-display-height)
        (set-frame-width-to-three-quarters-display-width)
        (set-frame-position-top-right-hand-side)
        )))

(defun set-frame-height-percent-of-display-height ( percent )
  "Size the current frame height to a given percentage of the
current display height - nb:approximately, Emacs sets frame
height by char rows, so we calculate from the current frame
character height"
  (interactive "nPercent: ")
  (if window-system
      (progn
        (set-frame-height (selected-frame) (floor (* (/ (x-display-pixel-height) (frame-char-height)) (/ percent 100.0)))))))

(defun set-frame-to-footer ()
  "Set the frame to a convenient position at the bottom of the
screen, changing the height and width too. Width will be set to
100%. Height set to 37%"
  (interactive)
  (if window-system
      (progn
        (set-frame-height-percent-of-display-height (* 0.37 100))
        (set-frame-width-to-display-width)
        (set-frame-position (selected-frame) 0
                            (floor (* (x-display-pixel-height) 0.62))))))

;; Some defaults for quick access. (24 == C-x)

(global-set-key [24 M-left] 'set-frame-big-left-1)
(global-set-key [24 M-right] 'set-frame-big-right-1)
(global-set-key [24 M-down] 'set-frame-to-footer)
(global-set-key [24 M-up] 'set-frame-maximize)

(provide 'frame-adjust)
