;;; visual-progress-mode.el --- progress indicator shows position in current buffer in mode-line.
;;; 
;;; Author: Jason Milkins
;;; Version: 0.1
;;; Keywords: visual-progress
;;; 
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LICENSE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Some TODOs
;;; * Investigate why wavy leftside didn't work on Antoszka's computer.
;;; * Refactor-out :set lambdas in customs if possible.
;;; * MAYBE add something to protect users from going to 0 with visual-progressbar width?
;;; * Add credits for used images.
(defgroup visual-progress nil
  "Customization group for `visual-progress-mode'."
  :group 'frames)

(defun visual-progress-refresh ()
  "Refresh after option changes if loaded."
  (when (featurep 'visual-progress-mode)
    (when (and (boundp 'visual-progress-mode)
               visual-progress-mode)
      (visual-progress-mode -1)
      (visual-progress-mode 1))))

(defcustom visual-progress-animation-frame-interval 0.2
  "Number of seconds between animation frames."
  :set (lambda (sym val)
         (set-default sym val)
         (visual-progress-refresh))
  :group 'visual-progress)

(defvar visual-progress-animation-timer nil)

(defun visual-progress-start-animation ()
  (interactive)
  (when (not visual-progress-animate-visual-progress)
    (setq visual-progress-animation-timer (run-at-time "1 sec"
                                            visual-progress-animation-frame-interval
                                            'visual-progress-swich-anim-frame))
    (setq visual-progress-animate-visual-progress t)))

(defun visual-progress-stop-animation ()
  (interactive)
  (when visual-progress-animate-visual-progress
    (cancel-timer visual-progress-animation-timer)
    (setq visual-progress-animation-timer nil)
    (setq visual-progress-animate-visual-progress nil)))


;;; FIXME bug, doesn't work for antoszka.
(defcustom visual-progress-wavy-trail nil
  "If enabled, progress indicator's leftside trail will be wavy."
  :type '(choice (const :tag "Enabled" t)
                 (const :tag "Disabled" nil))
  :set (lambda (sym val)
         (set-default sym val)
         (visual-progress-refresh))
  :group 'visual-progress)

(defcustom visual-progress-bar-length 32
  "Length of progress indicator bar in units; each unit is equal to an 8px
  image. Minimum of 3 units are required for progress indicator."
  :set (lambda (sym val)
         (set-default sym val)
         (visual-progress-refresh))
  :group 'visual-progress)

(defcustom visual-progress-animate-visual-progress nil
  "Enable animation for progress indicator.
This can be t or nil."
  :type '(choice (const :tag "Enabled" t)
                 (const :tag "Disabled" nil))
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (visual-progress-start-animation)
           (visual-progress-stop-animation))
         (visual-progress-refresh))
  :group 'visual-progress)


(defconst +visual-progress-directory+ (file-name-directory (or load-file-name buffer-file-name)))

(defconst +visual-progress-indicator-size+ 3)

(defconst +visual-progress-indicator-image+ (concat +visual-progress-directory+ "img/visual-progress.png"))
(defconst +visual-progress-leftside-image+ (concat +visual-progress-directory+ "img/leftside.png"))
(defconst +visual-progress-rightside-image+ (concat +visual-progress-directory+ "img/rightside.png"))

;;; Load images of progress indicator an it's leftside.
(defvar visual-progress-indicator-image (create-image +visual-progress-indicator-image+ 'xpm nil :ascent 'center))

(defvar visual-progress-animation-frames (mapcar (lambda (id)
                                        (create-image (concat +visual-progress-directory+ (format "img/visual-progress-frame-%d.png" id))
                                                      'xpm nil :ascent 95))
                                      '(1 2 3 4 5 6)))
(defvar visual-progress-current-frame 0)

(defun visual-progress-swich-anim-frame ()
  (setq visual-progress-current-frame (% (+ 1 visual-progress-current-frame) 6))
  (redraw-modeline))

(defun visual-progress-get-anim-frame ()
  (if visual-progress-animate-visual-progress
      (nth visual-progress-current-frame visual-progress-animation-frames)
    visual-progress-indicator-image))

(defun visual-progress-wavy-leftside-ascent (number)
  (if visual-progress-animate-visual-progress
      (min 100 (+ 90
                  (* 3 (abs (- (/ 6 2)
                               (% (+ number visual-progress-current-frame)
                                  6))))))
      (if (zerop (% number 2)) 80 'center)))

(defun visual-progress-number-of-leftsides ()
  (round (/ (* (round (* 100
                         (/ (- (float (point))
                               (float (point-min)))
                            (float (point-max)))))
               (- visual-progress-bar-length +visual-progress-indicator-size+))
          100)))

(defun visual-progress-create ()
  (let* ((leftsides (visual-progress-number-of-leftsides))
         (rightsides (- visual-progress-bar-length leftsides +visual-progress-indicator-size+))
         (leftside-string "")
         (visual-progress-string (propertize "[]*"
                                     'display (visual-progress-get-anim-frame)))
         (rightside-string ""))
    (dotimes (number leftsides)
      (setq leftside-string (concat leftside-string
                                   (propertize "|"
                                               'display (create-image +visual-progress-leftside-image+ 'xpm nil :ascent (or (and visual-progress-wavy-trail

                                                                                                                     (visual-progress-wavy-leftside-ascent number))
                                                                                                                (if visual-progress-animate-visual-progress 95 'center)))))))
    (dotimes (number rightsides)
      (setq rightside-string (concat rightside-string
                                      (propertize "-"
                                                  'display (create-image +visual-progress-rightside-image+ 'xpm nil :ascent (if visual-progress-animate-visual-progress 95 'center))))))
    (concat leftside-string
            visual-progress-string
            rightside-string)))

(defvar visual-progress-old-car-mode-line-position nil)

;;;###autoload
(define-minor-mode visual-progress-mode
  "Use visual progress mode to show buffer size and position in mode-line.
You can customize this minor mode, see option `visual-progress-mode'.

Note: If you turn this mode on then you probably want to turn off
option `scroll-bar-mode'."
  :global t
  :group 'visual-progress
  (if visual-progress-mode
      (progn
        (unless visual-progress-old-car-mode-line-position
          (setq visual-progress-old-car-mode-line-position (car mode-line-position)))
        (setcar mode-line-position '(:eval (list (visual-progress-create)))))
    (setcar mode-line-position visual-progress-old-car-mode-line-position)))


(provide 'visual-progress-mode)

;;; visual-progress-mode.el ends here
