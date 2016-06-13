;;; trr-sessions --- (C) 1996 Yamamoto Hirotaka <ymmt@is.s.u-tokyo.ac.jp>

;;; Commentary:

;; This file is a part of trr, a type training package for GNU Emacs.
;; See the copyright notice in trr.el

;; for now, there is only one type of session is supported.

;;; Code:

(require 'picture) ; picture-move-down

(defvar trr-japanese)
(defvar trr-list-of-eval)
(defvar trr-typist-flag)
(defvar trr-small-window-flag)
(defvar trr-win-conf-typist)
(defvar trr-start-flag)
(defvar trr-text-lines)
(defvar trr-correct-char-count)
(defvar trr-whole-char-count)
(defvar trr-start-time)
(defvar trr-return-is-space)
(defvar trr-ch)
(defvar trr-correct-char-count)
(defvar trr-ding-when-miss)
(defvar trr-end-time)
(defvar trr-win-conf)

(declare-function trr-write-graph "trr.el")
(declare-function trr-print-log "trr.el")
(declare-function trr-display-buffer "trr.el")
(declare-function trr-trainer-menu-buffer "trr.el")

(defun trr-get-event ()
  (let ((ev (trr-read-event)))
    (while (listp ev)
      (message (if trr-japanese "ずるは駄目だよう" "Don't play foul!"))
      (ding)
      (setq ev (trr-read-event)))
    (if (integerp ev)
	(if (/= ev 12)
	    ev
	  (redraw-display)
	  (trr-get-event))
      (cond ((eq ev 'return) ?\r)
	    ((eq ev 'tab) ?\t)
	    ((eq ev 'backspace) ?\b)
	    ((eq ev 'newline) ?\n)
	    ((eq ev 'escape) ?\e)
	    (t ?\a)))))

(defun trr-read-event ()
  ;;(cond
  ;; ((fboundp 'read-event)
    (read-event))
  ;; ((fboundp 'next-command-event)
  ;;  (let (char)
  ;;    (while (null (setq char (event-to-character
  ;;      		       (next-command-event)))))
  ;;    (char-to-int char)))
  ;; (t
  ;;  (error "no read-event"))))

(defun trr-one-session ()
  (other-window -1)
  (trr-write-graph trr-list-of-eval 0
		   (if trr-japanese
		       "今回の得点グラフ"
		     "Score Graph for this time"))
  (other-window -1)
  (trr-print-log)
  (other-window 2)
  (if (or trr-typist-flag trr-small-window-flag)
      (set-window-configuration trr-win-conf-typist))
  (erase-buffer)
  (with-current-buffer (trr-display-buffer)
    (if (not trr-start-flag)
	(setq trr-start-flag t))
    (copy-to-buffer (trr-trainer-menu-buffer)
		    (point)
		    (progn
		      (forward-line (* 3 trr-text-lines))
		      (point))))
  (goto-char (point-min))
  (forward-line 1)
  (setq trr-correct-char-count 0)
  (setq trr-whole-char-count 0)
  (if (eobp) t
    (let ((inhibit-quit 't)
	  (echo-keystrokes 0)
	  (lines (/ (count-lines (point-min) (point-max)) 3))
	  (text-pos (save-excursion (forward-line -1) (point)))
	  (started nil))
      (message (if trr-japanese "ようい!" "Ready!"))
      (setq trr-ch (trr-get-event))
      (message (if trr-japanese "スタート!" "start!"))
      (setq trr-start-time (current-time-string))
      (while (and (> lines 0)
		  (/= trr-ch 18)   ;; if trr-ch = ^R then restart
		  (/= trr-ch 3))   ;; if trr-ch = ^C then quit
	(setq trr-whole-char-count (1+ trr-whole-char-count))
	(if (if trr-return-is-space ; if correct typed
		(if (= (char-after text-pos) 13)
		    (or (= trr-ch 13)
			(= trr-ch 32))
		  (= trr-ch (char-after text-pos)))
	      (= trr-ch (char-after text-pos)))
	    (progn
	      (setq trr-correct-char-count (+ trr-correct-char-count 1))
	      (setq text-pos (1+ text-pos))
	      (if (if trr-return-is-space
		      (if (= (char-after (1- text-pos)) 13)
			  (and (/= trr-ch 13)
			       (/= trr-ch 32))
			(/= trr-ch 13))
		    (/= trr-ch 13))
		  (progn
		    (insert-char trr-ch 1)
		    (and window-system
			 (put-text-property (1- (point)) (point) 'face
					    'trr-correct-face))
		    (setq trr-ch (trr-get-event)))
		(insert-char 13 1)         ; cr mark
		(and window-system
		     (put-text-property (1- (point)) (point) 'face
					'trr-correct-face))
		(setq lines (1- lines))
		(if (/= lines 0)
		    (progn (forward-line 3)
			   (setq text-pos (save-excursion
					    (forward-line -1) (point)))
			   (setq trr-ch (trr-get-event))))))
	  (if (= trr-ch 10)
	      (insert " ")
	    (if (= trr-ch 7)
		(setq quit-flag 'nil))
	    (insert-char trr-ch 1))
	  (and trr-ding-when-miss (ding))
	  (backward-char 1) ; if incorrect typed
	  (while (and (if trr-return-is-space
			  (if (= (char-after text-pos) 13)
			      (and (/= trr-ch 13)
				   (/= trr-ch 32))
			    (/= trr-ch (char-after text-pos)))
			(/= trr-ch (char-after text-pos)))
		      (/= trr-ch 18)
		      (/= trr-ch  3))
	    (and window-system
		 (put-text-property (point) (1+ (point))
				    'face
				    'trr-miss-face))
	    (setq trr-ch (trr-get-event))
	    (delete-char 1)
	    (if (= trr-ch 10)
		(insert " ")
	      (if (= trr-ch 7)
		  (setq quit-flag 'nil))
	      (insert-char trr-ch 1))
	    (backward-char 1)) ; end of while
	  (picture-move-down 1)
	  (insert "^")
	  (forward-line -1)
	  (end-of-line)
	  (backward-char 1)
	  (delete-char 1)
	  (setq text-pos (1+ text-pos))
	  (if (if trr-return-is-space
		  (if (= (char-after (1- text-pos)) 13)
		      (and (/= trr-ch 13)
			   (/= trr-ch 32))
		    (/= trr-ch 13))
		(/= trr-ch 13))
	      (if (or (= trr-ch 3) (= trr-ch 18))
		  (setq lines 0)
		(insert-char trr-ch 1)        ; blink or reverse mode
		(and window-system
		     (put-text-property (1- (point))  (point)
					'face 'trr-miss-face))
		(setq trr-ch (trr-get-event)))
	    (setq lines (1- lines))
	    (insert-char 13 1)             ; cr mark
	    (and window-system
		 (put-text-property (1- (point))  (point)
				    'face 'trr-miss-face))
	    (if (/= lines 0)
		(progn (forward-line 3)
		       (setq text-pos (save-excursion
					(forward-line -1) (point)))
		       (setq trr-ch (trr-get-event)))))))
      ;; dummy
      (setq trr-end-time (current-time-string))
      (recenter -2)))
  (if (or trr-typist-flag trr-small-window-flag)
      (set-window-configuration trr-win-conf)))

(provide 'trr-sess)
;;; trr-sess.el ends here
