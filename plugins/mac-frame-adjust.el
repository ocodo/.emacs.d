;; Emacs Mac Port - frame adjust - this is for personal use only,
;; because it's not very good ;)

;; I recommend using Zephyros (or Slate) instead

(require 'cl)
(require 's)
(require 'dash)

(defun mac-calculate-displays-list ()
  "List of displays on a Mac, each display is represented as (w h)
   e.g. (1280 1024)"
  (-map
   (lambda (str)
     (setq i (s-index-of "@" str))
     (when i
       (setq str (car (s-split "@" str))))
     (-map
      (lambda (str)
        (string-to-number str)) (s-split "x" str)))
   (s-lines
    (s-replace
     " " ""
     (s-replace
      "Resolution: " ""
      (s-trim
       (s-chomp
        (shell-command-to-string

         "system_profiler  SPDisplaysDataType | grep -E 'Resolution' | tr -s ' '"))))))))

(defvar mac-displays-list (mac-calculate-displays-list)
  "memo of mac-calculate-displays-list - List of displays on a
   Mac, each display is represented as (w h) e.g. (1280 1024)")

(defvar mac-max-height-correct 0
  "Set to the amount you need to slice off the max display
  height, for example, if setting frame height to full display
  height, sets the frame too large, and the title bar is
  positioned behind the mac menu bar, set this variable to set
  make the max height a little smaller, eg. 60-90 will fix
  it. Often the value required will depend on the default font
  you're using. - default is 0

  currently only used by mac-set-frame-height-to-display-height
  however this is used by several other functions" )

(defun mac-largest-display ()
  "return the dimensions of the largest Mac display/screen"
  (let*
      ((d-list (-map
                (lambda (d)
                  (--reduce (+ acc it) d))
                mac-displays-list))
       (d (reduce 'max d-list))
       (i (position d d-list )))
    (nth i mac-displays-list)))


(defun mac-largest-display-pixel-width ()
  "pixel width of largest disply"
  (car (mac-largest-display)))

(defun mac-largest-display-pixel-height ()
  "pixel height of largest disply"
  (nth 1 (mac-largest-display)))

(defun mac-set-frame-position-top-right-hand-side ()
  "Position the current frame to the top/right of the Display"
  (interactive)
  (if window-system
      (progn
        (set-frame-position (selected-frame) (- (mac-largest-display-pixel-width) (frame-pixel-width)) 0))))

(defun mac-set-frame-position-top-center ()
  "Position the current frame to the top/center of the Display"
  (interactive)
  (if window-system
      (progn
        (set-frame-position (selected-frame) (- (/ (mac-largest-display-pixel-width) 2) (/ (frame-pixel-width) 2)) 1))))

(defun mac-set-frame-position-top-left-hand-side ()
  "Position the current frame to the top/left of the Display"
  (interactive)
  (if window-system
      (progn
        (set-frame-position (selected-frame) 0 0))))

(defun mac-set-frame-height-to-display-height ()
  "Size the current frame height to match display height -
nb:approximately, Emacs sets frame height by char rows."
  (interactive)
  (if window-system
      (progn
        (set-frame-height (selected-frame) (/ (- (mac-largest-display-pixel-height) mac-max-height-correct) (frame-char-height))))))

(defun mac-set-frame-height-to-85-percent-display-height ()
  "Size the current frame height to 85% of display height -
nb:approximately, Emacs sets frame height by char rows."
  (interactive)
  (if window-system
      (progn
        (set-frame-height (selected-frame) (floor (* (/ (mac-largest-display-pixel-height) (frame-char-height)) 0.85))))))

(defun mac-set-frame-width-to-two-thirds-display-width ()
  "Size the current frame to two thirds the display width. -
nb:approximately, Emacs sets frame width by char columns."
  (interactive)
  (if window-system
      (progn
        (set-frame-width (selected-frame) (/ (round (* (mac-largest-display-pixel-width) 0.66)) (frame-char-width))))))

(defun mac-set-frame-width-to-three-quarters-display-width ()
  "Size the current frame to three quarters the display width. -
nb:approximately, Emacs sets frame width by char columns."
  (interactive)
  (if window-system
      (progn
        (set-frame-width (selected-frame) (/ (round (* (mac-largest-display-pixel-width) 0.75)) (frame-char-width))))))

(defun mac-set-frame-width-to-half-display-width ()
  "Size the current frame to half the display width. -
nb:approximately, Emacs sets frame width by char columns."
  (interactive)
  (if window-system
      (progn
        (set-frame-width (selected-frame) (/ (/ (mac-largest-display-pixel-width) 2) (frame-char-width))))))

(defun mac-set-frame-width-to-quarter-display-width ()
  "Size the current frame to quarter the display width. -
nb:approximately, Emacs sets frame width by char columns."
  (interactive)
  (if window-system
      (progn
        (set-frame-width (selected-frame) (/ (/ (mac-largest-display-pixel-width) 4) (frame-char-width))))))

(defun mac-set-frame-width-to-third-display-width ()
  "Size the current frame to quarter the display width. -
nb:approximately, Emacs sets frame width by char columns."
  (interactive)
  (if window-system
      (progn
        (set-frame-width (selected-frame) (/ (/ (mac-largest-display-pixel-width) 3) (frame-char-width))))))

(defun mac-set-frame-width-to-display-width ()
  "Size the current frame to half the display width. -
nb:approximately, Emacs sets frame width by char columns."
  (interactive)
  (if window-system
      (progn
        (set-frame-width (selected-frame) (/ (- (mac-largest-display-pixel-width) 30) (frame-char-width))))))

(defun mac-set-frame-maximize ()
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-position-top-left-hand-side)
        (mac-set-frame-width-to-display-width)
        (mac-set-frame-height-to-display-height))))

(defun mac-toggle-kiosk-mode ()
  "Toggle full screen"
  (interactive)
  (when window-system
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(defun mac-set-frame-left-w25-h100-percent ()
  "A convenient preset frame size & position - 25% width, 100%
height, top/left position."
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-height-to-display-height)
        (mac-set-frame-width-to-quarter-display-width)
        (mac-set-frame-position-top-left-hand-side))))

(defun mac-set-frame-right-w25-h100-percent ()
  "A convenient preset frame size & position - 25% width, 100%
height, top/right position."
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-height-to-display-height)
        (mac-set-frame-width-to-quarter-display-width)
        (mac-set-frame-position-top-right-hand-side))))

(defun mac-set-frame-right-w30-h100-percent ()
  "A convenient preset frame size & position - 30% width, 100%
height, top/right position."
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-height-to-display-height)
        (mac-set-frame-width-to-third-display-width)
        (mac-set-frame-position-top-right-hand-side))))

(defun mac-set-frame-left-w50-h100-percent ()
  "A convenient preset frame size & position - 50% width, 100%
height, top/left position."
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-height-to-display-height)
        (mac-set-frame-width-to-half-display-width)
        (mac-set-frame-position-top-left-hand-side))))

(defun mac-set-frame-left-w66-h100-percent ()
  "A convenient preset frame size & position - 66% width, 100%
height, top/left position."
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-height-to-display-height)
        (mac-set-frame-width-to-two-thirds-display-width)
        (mac-set-frame-position-top-left-hand-side))))

(defun mac-set-frame-left-w75-h100-percent ()
  "A convenient preset frame size & position - 75% width, 100%
height, top/left position."
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-height-to-display-height)
        (mac-set-frame-width-to-three-quarters-display-width)
        (mac-set-frame-position-top-left-hand-side))))

(defun mac-set-frame-left-w75-h85-percent ()
  "A convenient preset frame size & position - 75% width, 85%
height, top/left position."
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-height-to-85-percent-display-height)
        (mac-set-frame-width-to-three-quarters-display-width)
        (mac-set-frame-position-top-left-hand-side))))

(defun mac-set-frame-right-w50-h100-percent ()
  "A convenient preset frame size & position - 50% width, 100%
height, top/right position."
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-height-to-display-height)
        (mac-set-frame-width-to-half-display-width)
        (mac-set-frame-position-top-right-hand-side))))

(defun mac-set-frame-right-w66-h100-percent ()
  "A convenient preset frame size & position - 66% width, 100%
height, top/right position."
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-height-to-display-height)
        (mac-set-frame-width-to-two-thirds-display-width)
        (mac-set-frame-position-top-right-hand-side))))

(defun mac-set-frame-right-w75-h100-percent ()
  "A convenient preset frame size & position - 75% width, 100%
height, top/right position."
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-height-to-display-height)
        (mac-set-frame-width-to-three-quarters-display-width)
        (mac-set-frame-position-top-right-hand-side))))

(defun mac-set-frame-right-w75-h85-percent ()
  "A convenient preset frame size & position - 75% width, 85%
height, top/right position."
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-height-to-85-percent-display-height)
        (mac-set-frame-width-to-three-quarters-display-width)
        (mac-set-frame-position-top-right-hand-side))))

(defun mac-set-frame-top-w100-h50-percent ()
  "A convenient preset frame size & position - 100% width, 50%
height, top of screen."
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-height-percent-of-display-height 50)
        (mac-set-frame-width-to-display-width)
        (mac-set-frame-position-top-left-hand-side))))

(defun mac-set-frame-bottom-w100-h50-percent ()
  "A convenient preset frame size & position - 100% width, 50%
height, bottom of screen."
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-height-percent-of-display-height 47)
        (mac-set-frame-width-to-display-width)
        (set-frame-position (selected-frame) 0
                            (floor (* (mac-largest-display-pixel-height) 0.51))))))

(defun mac-set-frame-height-percent-of-display-height ( percent )
  "Size the current frame height to a given percentage of the
current display height - nb:approximately, Emacs sets frame
height by char rows, so we calculate from the current frame
character height"
  (interactive "nPercent: ")
  (if window-system
      (progn
        (set-frame-height (selected-frame) (floor (* (/ (mac-largest-display-pixel-height) (frame-char-height)) (/ percent 100.0)))))))

(defun mac-set-frame-to-footer ()
  "Set the frame to a convenient position at the bottom of the
screen, changing the height and width too. Width will be set to
100%. Height set to 37%"
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-height-percent-of-display-height 37)
        (mac-set-frame-width-to-display-width)
        (set-frame-position (selected-frame) 0
                            (floor (* (mac-largest-display-pixel-height) 0.61))))))

(defun mac-set-frame-center-w50-h100-percent ()
  "A convenient preset frame size & position - 50% width, 100%
height, top/center position."
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-height-to-display-height)
        (mac-set-frame-width-to-half-display-width)
        (mac-set-frame-position-top-center))))

(defun mac-set-frame-center-w75-h100-percent ()
  "A convenient preset frame size & position - 50% width, 100%
height, top/center position."
  (interactive)
  (if window-system
      (progn
        (mac-set-frame-height-to-display-height)
        (mac-set-frame-width-to-three-quarters-display-width)
        (mac-set-frame-position-top-center))))

;; Some defaults for quick access. (24 == C-x)

(require 'bind-key)
(bind-keys
 ("C-x M-<left>"     . mac-set-frame-left-w66-h100-percent)
 ("C-x M-<right>"    . mac-set-frame-right-w66-h100-percent)
 ("C-x M-<down>"     . mac-set-frame-to-footer)
 ("C-x M-<up>"       . mac-toggle-kiosk-mode)
 ("C-x C-M-<left>"   . mac-set-frame-left-w50-h100-percent)
 ("C-x C-M-<right>"  . mac-set-frame-right-w50-h100-percent)
 ("C-x C-M-<up>"     . mac-set-frame-top-w100-h50-percent)
 ("C-x C-M-<down>"   . mac-set-frame-bottom-w100-h50-percent)
 ("C-x M-s-<right>"  . mac-set-frame-right-w30-h100-percent)
 ("C-x M-s-<left>"   . mac-set-frame-left-w25-h100-percent)
 ("C-x M-s-<up>"     . mac-set-frame-center-w50-h100-percent)
 ("C-x M-s-<down>"   . mac-set-frame-center-w75-h100-percent))

(provide 'mac-frame-adjust)
