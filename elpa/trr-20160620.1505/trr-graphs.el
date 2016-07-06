;;; trr-graphs --- (C) 1996 Yamamoto Hirotaka <ymmt@is.s.u-tokyo.ac.jp>

;;; Commentary:

;; This file is a part of trr, a type training package for GNU Emacs.
;; See the copyright notice in trr.el

;; Variables for writing graphs

;;; Code:

(require 'picture) ; picture-move-up

(defvar trr-skipped-step         0 "The number of skipped steps.")
					; スキップしたステップ数
(defvar trr-list-of-eval       nil)
(defvar trr-list-of-speed      nil)
(defvar trr-list-of-miss       nil)
(defvar trr-list-of-average    nil)
(defvar trr-list-of-time       nil)
(defvar trr-list-of-times      nil)
(defvar trr-list-of-value      nil)

(defvar trr-japanese)
(defvar trr-score-file)

(declare-function trr-print-result   "trr.el")
(declare-function trr-print-data     "trr.el")
(declare-function trr-print-log      "trr.el")
(declare-function trr-print-message  "trr.el")
(declare-function trr-display-buffer "trr.el")
(declare-function trr-record-buffer  "trr.el")
(declare-function trr-evaluate-point "trr.el")

(defun trr-display-variables-message-graph ()
  (other-window 1)
  (trr-print-result)
  (other-window 1)
  (trr-print-data)
  (other-window 1)
  (trr-print-message)
  (other-window 1)
  (trr-print-log)
  (other-window 1)
  (trr-write-graph trr-list-of-eval 0
		   (if trr-japanese
		       "今回の得点グラフ"
		     "Score Graph for this play"))
  (other-window 1))


(defun trr-write-graph (data-list skip string)
  (erase-buffer)
  (insert string "\n")
  (let ((fill-column (window-width)))
    (center-region (point-min) (point)))
  (let ((max 0)
	(min 10000)
	(rest data-list)
	(revlist nil)
	(move-count 0)
	(scale-x 1)
	(steps 0)
	;;steps
        temp graph-steps horizontal-steps scale-y)
    (if (not rest) t
      (setq steps 1)
      (setq temp (car rest))
      (setq rest (cdr rest))
      (setq max (max max temp))
      (setq min (min min temp))
      (setq revlist (cons temp revlist)))
    (while rest
      (setq steps (1+ steps))
      (setq temp (car rest))
      (setq rest (cdr rest))
      (setq max (max max temp))
      (setq min (min min temp))
      (setq revlist (cons temp revlist)))
    (setq graph-steps (/ (- (window-height) 5) 2)       ; row
	  horizontal-steps (/ (- (window-width) 8) 4)   ; column
	  fill-column (- (window-width) 4))
    (and (> steps horizontal-steps)
	 (let ((diff (- steps (* horizontal-steps 2))))
	   (setq scale-x 2)
	   (if (> diff 0)
	       (progn
		 (while (> diff 0)
		   (setq revlist (cdr revlist))
		   (setq move-count (1+ move-count))
		   (setq diff (1- diff)))
		 (setq steps (* horizontal-steps 2))
		 (setq rest revlist)
		 (setq max 0)
		 (setq min 10000)
		 (while rest
		   (setq max (max max (car rest)))
		   (setq min (min min (car rest)))
		   (setq rest (cdr rest)))))))
    (if (> min max) (setq min 0)
      (setq min
	    (cond
	     ((< (- max min) 10) (- min (% min 10)))
	     ((< (- max min) 20) (- min (% min 20)))
	     (t (- min (% min 50))))))
    (setq scale-y (max 1
		       (if (= (% (- max min) graph-steps) 0)
					; if (- max min) is mutiple of 10
			   (/ (- max min) graph-steps)
					; then draw just in display.
			 (1+ (/ (- max min) graph-steps)))))
    ;; 1 2 3 4 5 6 8 10 12 15 20 25 30 40 50 60 70 80 90 100 120 140 160 180..
    (cond ((> scale-y 100)                                 ;; round by 20
	   (if (= (% scale-y 20) 0)
	       (setq scale-y (* (/ scale-y 20) 20))
	     (setq scale-y (+ (* (/ scale-y 20) 20) 20))))
	  ((> scale-y 30)                                  ;; round by 10
	   (if (= (% scale-y 10) 0)
	       (setq scale-y (* (/ scale-y 10) 10))
	     (setq scale-y (+ (* (/ scale-y 10) 10) 10))))
	  ((> scale-y 13)                                  ;; round by 5
	   (if (= (% scale-y 5) 0)
	       (setq scale-y (* (/ scale-y 5) 5))
	     (setq scale-y (+ (* (/ scale-y 5) 5) 5))))
	  ((> scale-y 6)                                   ;; round by 2
	   (if (= (% scale-y 2) 0)
	       (setq scale-y (* (/ scale-y 2) 2))
	     (setq scale-y (+ (* (/ scale-y 2) 2) 2)))))
    (if (< graph-steps 2)
	t
      (let ((i graph-steps))
	(while (> i 0)
	  (insert (if trr-japanese
		      "     ┃\n"
		      "      |\n")
		  (format "%4d" (+ min (* i scale-y)))
		  (if trr-japanese
		      " ┣\n"
		    "  +\n"))
	  (setq i (1- i)))
	(insert (if trr-japanese
		    "     ┃\n"
		  "      |\n")
		(format "%4d" min)
		(if trr-japanese
		    " ┗"
		  "  +"))
	(while (< i horizontal-steps)
	  (insert (if trr-japanese
		      "━┻"
		    "---+"))
	  (setq i (1+ i)))
	(insert (format "\n   %4d" move-count))
	(setq i 1)
	(while (<= i horizontal-steps)
	  (insert (format "%4d" (+ (* i scale-x) skip move-count)))
	  (setq i (1+ i))))
      (goto-char (point-max))
      (beginning-of-line)
      (forward-char 5)
      (let ((times (/ 4 scale-x))
	    (inter (max 1 (/ scale-x 4)))
	    (templist revlist)
	    (i 1)
	    data height)
	(save-excursion
	  (while templist
	    (setq data (car templist))
	    (setq templist (cdr templist))
	    (let ((th (/ (* (- data min) 4) scale-y)))
	      (setq height (+ (/ th 2) (% th 2))))
	    (forward-char times)
	    (save-excursion
	      (or trr-japanese
		  (forward-char))
	      (picture-move-up (1+ height))
	      (if (and (= i 1) (= times 1))
		  (progn
		    (delete-char 1)
		    (insert " ")))
	      (let ((j nil))
		(if (= height 0)
		    (progn (delete-char 1)
			   (setq j (point))
			   (insert (if trr-japanese
				       "*"
				     "*")))
		  (setq j (point))
		  (insert (if trr-japanese
			      "★"
			    "*")))
		(and window-system
		     ;;trr-graph-color-name
		     j
		     (put-text-property j (point)
					;;'face trr-top-face-name))))
					'face 'trr-graph-face))))
	    (setq i (1+ i)))))))
  (switch-to-buffer (get-buffer (current-buffer))))


(defun trr-show-ranking ()
  (set-buffer (get-buffer-create (trr-display-buffer)))
  (erase-buffer)
  (insert (if trr-japanese
	      "\
順位\tスコア\tログイン名\tstep\t総回数\t総時間\t  日付,   時間\n"
	    "\
Order\tScore\tName\t\tstep\ttimes\ttime\tdate,     hour\n"))
  (insert-file-contents trr-score-file)
  (goto-char (point-min))
  (forward-line 1)
  ;; trr graphs :: spaces -> TAB
  (while (re-search-forward " " nil t) (replace-match "\t"))
  (goto-char (point-min))
  (forward-line 1)
  (let ((i 1)
	(j 0)
	(self nil))
    (while (not (eobp))
      (insert (format "%d\t" i))
      (if (looking-at (format "%s\t" (user-login-name)))
	  (progn
	    (beginning-of-line)
	    (while (not (looking-at "\t")) (forward-char 1))
	    (forward-char 1)
	    (insert "> ")
	    (setq self (point))
	    (while (not (looking-at "\t")) (forward-char 1))
	    (and window-system
		 (put-text-property self (point) 'face
				    'trr-self-face))
	    (insert " <")
	    (and (< (length (user-login-name)) 4)
		 (insert "\t"))
	    (forward-char 1)
	    (if (looking-at "\t") (delete-char 1))))
      (forward-line 1)
      (setq i (1+ i)))
    (goto-char (point-min))
    (forward-line 1)
    (beginning-of-line)
    (setq j (point))
    (end-of-line)
    (and window-system
	 (/= j (point))
	 (put-text-property j (1+ (point)) 'face
			    'trr-graph-face))
    (switch-to-buffer (trr-display-buffer))
    (if self
	(progn
	  (goto-char self)
	  (beginning-of-line))
      (goto-char (point-min)))))


(defun trr-get-graph-points ()
  (setq trr-skipped-step 0)
  (setq trr-list-of-speed nil)
  (setq trr-list-of-miss nil)
  (setq trr-list-of-time nil)
  (setq trr-list-of-times nil)
  (setq trr-list-of-value nil)
  (setq trr-list-of-average nil)
  (with-current-buffer (get-buffer-create (trr-record-buffer))
    (goto-char (point-min))
    (let ((curstep 1)
	  (curpoint (point))
	  curtime wc mc)
      (while (not (eobp))
	(setq wc (string-to-number
		  (buffer-substring
		   (+ curpoint 16) (+ curpoint 22))))
	(setq mc (string-to-number
		  (buffer-substring
		   (+ curpoint 23) (+ curpoint 28))))
	(setq curtime (string-to-number
		       (buffer-substring
			(+ curpoint 9) (+ curpoint 15))))
	(if (= curtime 0)
	    (setq trr-skipped-step (1+ trr-skipped-step))
	  (setq trr-list-of-value
		(cons
		 (string-to-number
		  (buffer-substring
		   curpoint (+ curpoint 3)))
		 trr-list-of-value))
	  (setq trr-list-of-times
		(cons
		 (string-to-number
		  (buffer-substring
		   (+ curpoint 4) (+ curpoint 8)))
		 trr-list-of-times))
	  (setq trr-list-of-time
		(cons
		 (/ curtime 60)
		 trr-list-of-time))
	  (setq trr-list-of-speed
		(cons
		 (if (= curtime 0) 0 (/ (* wc 60) curtime))
		 trr-list-of-speed))
	  (setq trr-list-of-miss
		(cons
		 (if (= wc 0) 0 (/ (* mc 1000) wc))
		 trr-list-of-miss))
	  (setq trr-list-of-average
		(cons
		 (trr-evaluate-point wc mc curtime)
		 trr-list-of-average)))
 	(forward-line)
	(setq curpoint (point))
	(setq curstep (+ curstep 1))))))


(provide 'trr-graphs)
;;; trr-graphs.el ends here
