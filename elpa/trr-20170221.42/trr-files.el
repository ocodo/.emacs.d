;;; trr-files --- (C) 1996 Yamamoto Hirotaka <ymmt@is.s.u-tokyo.ac.jp>

;;; Commentary:

;; This file is a part of trr, a type training package for GNU Emacs.
;; See the copyright notice in trr.el

;; files for a session.

;;; Code:

(defvar  trr-text-file nil "* text file.")
(defvar  trr-record-file nil "* result file for each text.")
(defvar  trr-score-file nil "* score file.")
(defvar  trr-score-file-name nil "* score file name.")
(defvar  trr-text-name "" "* text name from CONTENTS.")

(defvar  trr-errbuf (generate-new-buffer " *update-game-score loss*"))

;; trr.el variables
(defvar trr-text-file-buffer)
(defvar trr-japanese)
(defvar trr-start-flag)
(defvar trr-quit-flag)
(defvar trr-update-flag)
(defvar trr-pass-flag)
(defvar trr-cheat-flag)
(defvar trr-beginner-flag)
(defvar trr-random-flag)
(defvar trr-secret-flag)
(defvar trr-typist-flag)
(defvar trr-small-window-flag)
(defvar trr-skip-session-flag)
(defvar trr-eval)
(defvar trr-whole-char-count)
(defvar trr-correct-char-count)
(defvar trr-start-time)
(defvar trr-end-time)
(defvar trr-miss-type-ratio)
(defvar trr-type-speed)
(defvar trr-steps)
(defvar trr-times-of-current-step)
(defvar trr-time-of-current-step)
(defvar trr-whole-chars-of-current-step)
(defvar trr-whole-miss-of-current-step)
(defvar trr-total-times)
(defvar trr-total-time)
(defvar trr-high-score)
(defvar trr-high-score-old)
(defvar trr-elapsed-time)
(defvar trr-select-text-file)
(defvar trr-directory)
(defvar trr-record-dir)
(defvar trr-score-dir)
(defvar trr-number-of-text-lines)
(defvar trr-un-hyphenate)
(defvar trr-update-program)
(defvar trr-use-update-program)

(declare-function trr-record-buffer "trr.el")
(declare-function trr-trainer-menu-buffer "trr.el")
(declare-function trr-random-num "trr.el")
(declare-function trr-display-buffer "trr.el")


(defun trr-debug (file)
  "Debug FILE."
  (interactive "F")
  (save-excursion
    (find-file file)
    ;;(find-file (expand-file-name (concat "~/src/trr19/" file)))
    (erase-buffer)
    (insert (format "trr-flags\ntrr-start-flag\t%s\ntrr-quit-flag\t%s\ntrr-update-flag\t%s\ntrr-pass-flag\t%s\ntrr-cheat-flag\t%s\ntrr-beginner-flag\t%s\ntrr-random-flag\t%s\ntrr-secret-flag\t%s\ntrr-typist-flag\t%s\ntrr-small-window-flag\t%s\ntrr-skip-session-flag\t%s\n\n" trr-start-flag trr-quit-flag trr-update-flag trr-pass-flag trr-cheat-flag trr-beginner-flag trr-random-flag trr-secret-flag trr-typist-flag trr-small-window-flag trr-skip-session-flag))
    (insert (format "Variables for Session\ntrr-eval\t%s\ntrr-whole-char-count\t%s\ntrr-correct-char-count\t%s\ntrr-start-time\t%s\ntrr-end-time\t%s\ntrr-miss-type-ratio\t%s\ntrr-type-speed\t%s\n\n" trr-eval trr-whole-char-count trr-correct-char-count trr-start-time trr-end-time trr-miss-type-ratio trr-type-speed))
    (insert (format "Variables for STEP\ntrr-steps\t%s\ntrr-times-of-current-step\t%s\ntrr-time-of-current-step\t%s\ntrr-whole-chars-of-current-step\t%s\ntrr-whole-miss-of-current-step\t%s\n\n" trr-steps trr-times-of-current-step trr-time-of-current-step trr-whole-chars-of-current-step trr-whole-miss-of-current-step))
    (insert (format "Others\ntrr-total-times\t%s\ntrr-total-time\t%s\ntrr-high-score\t%s\ntrr-high-score-old\t%s\ntrr-elapsed-time\t%s\n" trr-total-times trr-total-time trr-high-score trr-high-score-old trr-elapsed-time))
    (save-buffer)
    (kill-buffer (current-buffer))))


(defun trr-initiate-files ()
  "Initialize files."
  (with-current-buffer (get-buffer-create trr-text-file-buffer)
    (erase-buffer)
    (insert-file-contents trr-text-file)
    ;;(or (file-exists-p trr-score-file)
    ;;    (call-process  trr-update-program nil 0 nil trr-score-file-name))
    (unless (file-exists-p trr-score-file)
      ;; touch `trr-score-file'
      (make-directory (file-name-directory trr-score-file) t)
      (with-temp-file trr-score-file))
    (find-file-read-only trr-score-file)
    (find-file trr-record-file)
    (set-buffer (get-buffer-create (trr-record-buffer)))
    (erase-buffer)
    (if (file-exists-p trr-record-file)
	(insert-file-contents trr-record-file)
      (if trr-japanese
	  (insert "  0    0      0      0     0 ふぁいと!\n")
	(insert "  0    0      0      0     0 cheers!  \n")))))


(defun trr-decide-trr-text (num)
  "Decide to text NUM."
  (switch-to-buffer
   (get-buffer-create (trr-trainer-menu-buffer)))
  (erase-buffer)
  (insert-file-contents trr-select-text-file)
  (untabify (point-min) (point-max))
  (goto-char (point-min))
  (let ((kill-whole-line t))
    (while (not (eobp))
      (if (or (= (char-after (point)) ?#) ; comment line begins with #
	       (= (char-after (point)) ?\n))
	  (kill-line)
	(forward-line))))
  (goto-char (point-min)) (forward-line (1- num)) ; (goto-line num)
  (let (temp temp-file temp-id)
    (setq temp (point))
    (while (not (= (char-after (point)) 32)) (forward-char))
    (setq trr-text-name (buffer-substring temp (point)))
    (with-current-buffer (get-buffer-create (trr-trainer-menu-buffer))
      (setq mode-line-format (list "   "
				   (trr-trainer-menu-buffer)
				   "                "
				   trr-text-name))
      (force-mode-line-update))
    (while      (= (char-after (point)) 32)  (forward-char))
    (while (not (= (char-after (point)) 32)) (forward-char))
    (while      (= (char-after (point)) 32)  (forward-char))
    (while (not (= (char-after (point)) 32)) (forward-char)) ; need comment
    (while      (= (char-after (point)) 32)  (forward-char))
    (setq temp (point))
    (while (not (= (char-after (point)) 32)) (forward-char))
    (setq temp-file (buffer-substring temp (point)))
    (while      (= (char-after (point)) 32)  (forward-char))
    (setq temp (point))
    (while (not (or (eobp)
		    (= (char-after (point)) 32)
		    (= (char-after (point)) ?\n))) (forward-char))
    (setq temp-id (buffer-substring temp (point)))
    (setq trr-text-file (expand-file-name
                         (concat "text/" temp-file ".formed")
                          trr-directory))
    (setq trr-text-file-buffer (file-name-nondirectory trr-text-file))
    (and (not (and (file-exists-p (expand-file-name trr-text-file))
		   (file-newer-than-file-p trr-text-file
					   (expand-file-name
					    (concat "text/" temp-file) trr-directory))))
	 (or (file-exists-p (expand-file-name (concat "text/" temp-file)
                                              trr-directory))
	     (error "%s does not exists!" temp-file))
	 (message (if trr-japanese "初期化ファイルを作ってるの。ちょっとまってね。"
		    "Making preformatted file. please wait..."))
	 ;;(call-process trr-format-program nil nil nil temp-file))
         (trr-format-program temp-file))
    (or (file-directory-p trr-record-dir)
	(progn
	  ;;(call-process "/bin/rm" nil 0 nil "-f" trr-record-dir)
          (if (file-exists-p trr-record-dir)
              (delete-file trr-record-dir))
	  (make-directory trr-record-dir t)))
    (setq trr-record-file
          (expand-file-name (concat (user-login-name) "-" temp-id (trr-file-string))
                            trr-record-dir))
    (setq trr-score-file-name (concat "SCORES-" temp-id (trr-file-string)))
    (setq trr-score-file (expand-file-name (concat "record/" trr-score-file-name)
                                           trr-score-dir))))


(defun trr-format-program (temp-file)
  "Format program with TEMP-FILE."
  (with-temp-file (expand-file-name (concat "text/" temp-file ".formed")
                                    trr-directory)
    (insert-file-contents (expand-file-name (concat "text/" temp-file)
                                    trr-directory))
    (delete-matching-lines "^[ \t]*$")
    (goto-char (point-min))
    (while (re-search-forward "\\([.?!;]\\) *$" nil t) (replace-match (match-string 1)))
    (goto-char (point-min))
    (while (re-search-forward "^[ 	]+" nil t) (replace-match ""))))


(defun trr-file-string ()
  (cond
   (trr-secret-flag "-secret")
   (trr-typist-flag "-typist")
   ((not trr-random-flag) "-easy")
   (t "")))


(defun trr-kill-file (file)
  (if file
      (let ((tfile (get-file-buffer (expand-file-name file))))
	(if tfile (kill-buffer tfile)))))

(defun trr-save-file (buffer file)
  (let ((tb (get-buffer buffer))
	tfile)
    (if tb
	(with-current-buffer (get-buffer-create buffer)
	  (and (buffer-modified-p)
	       (progn (write-file file)
		      (setq tfile (concat file "~"))
		      (if (file-exists-p tfile) (delete-file tfile))))))))


(defun trr-read-file ()
  (with-current-buffer trr-text-file-buffer
    (goto-char (point-min))
    (forward-line
     (if trr-random-flag (1- (trr-random-num))
       (1- (* trr-steps (/ trr-number-of-text-lines 64)))))
    ;; (if trr-random-flag
    ;;     (goto-line (trr-random-num))
    ;;   (goto-line
    ;;    (* trr-steps (/ trr-number-of-text-lines 64))))
    (let (fill-column text start ch)
      (setq ch (char-after (point)))
      (while (not (or (= ch ?!)
		      (= ch ?.)
		      (= ch ?\;)
		      (= ch ?\?)
		      (= ch ?\)) ; )
		      (= ch ?\}))) ; }
	(forward-char)
	(setq ch (char-after (point))))
      (forward-char)
      (setq ch (char-after (point)))
      (while (or
	      (= ch ?\n) ; newline
	      (= ch ?\ ) ; space
	      (= ch ?!)
	      (= ch ?\))
	      (= ch ?.)
	      (= ch ?,)
	      (= ch ?\;)
	      (= ch ?\?)
	      (= ch ?\})
	      (and (= ch ?\")
		   (or (= (char-after (1+ (point))) ?\n)
		       (= (char-after (1+ (point))) ?\ )))
	      (and (= ch ?\')
		   (or (= (char-after (1+ (point))) ?\n)
		       (= (char-after (1+ (point))) ?\ ))))
	(forward-char)
	(setq ch (char-after (point))))
      (setq start (point))
      (forward-line 18)
      (setq text (buffer-substring start (point)))
      (switch-to-buffer (get-buffer-create (trr-display-buffer)))
      (erase-buffer)
      (insert text)
      (untabify (point-min) (point-max))
      (and trr-un-hyphenate
	   (progn
	     (goto-char (point-min))
	     (while (re-search-forward "\\([a-zA-z]\\)-\n *\\([a-zA-Z]\\)"
				       nil t)
	       (replace-match "\\1\\2" t))))
      (setq fill-column (- (window-width) 5))
      (fill-region (point-min) (point-max))
      (goto-char (point-min))
      (while (re-search-forward "\\([^ ]\\) *$" nil t)
	(replace-match "\\1" t))
      (goto-char (point-min))
      (while (not (eobp))
	(end-of-line)
	(insert "\n\n")
	(forward-line 1))
      (and window-system
	   (put-text-property (point-min) (point-max) 'face
			      'trr-text-face))
      (goto-char (point-min)))))

(defun trr-update-score-file (score)
  "Write SCORE to `trr-score-file'."
  (if (not trr-use-update-program)
      (if (file-writable-p trr-score-file)
          (with-temp-file trr-score-file
            (insert-file-contents trr-score-file)
            (insert (format "%s %s %s %s %s\n"
                            (number-to-string score)
                            (user-login-name)
                            (number-to-string trr-steps)
                            (number-to-string trr-total-times)
                            (number-to-string (/ trr-total-time 60))))
            (sort-numeric-fields 1 (point-min) (point-max))
            (reverse-region (point-min) (point-max)))
        (error "Score file (%s) is not writable!  Please check `trr-score-dir' variable"
               trr-score-file))
    (unless (file-exists-p trr-score-file)
      (error "Score file (%s) does not exist!  Please check `trr-score-dir' variable"
             trr-score-file))
    (unless (file-executable-p trr-update-program)
      (error "Helper program (%s) is not executable" trr-update-program))
    (let
        ((result
          (call-process trr-update-program nil trr-errbuf nil
                        "-r" "-m" "50" "-d" (expand-file-name "record" trr-score-dir)
                        trr-score-file-name
                        (number-to-string score)
                        (format "%s %s %s"
                                (number-to-string trr-steps)
                                (number-to-string trr-total-times)
                                (number-to-string (/ trr-total-time 60))))))
      (if (/= result 0)
          (error "Failed to write score file (%s) by helper program (%s)! Please check permissions"
                 trr-score-file trr-update-program)))))
  ;;(call-process trr-update-program nil 0 nil
  ;;      	trr-score-file-name
  ;;      	(user-login-name)
  ;;      	(int-to-string score)
  ;;      	(int-to-string trr-steps)
  ;;      	(int-to-string trr-total-times)
  ;;		(int-to-string (/ trr-total-time 60))))


(defun trr-get-high-score (&optional file)
  (if (not (file-exists-p trr-record-file))
      -1
    (find-file trr-record-file)
    (let ((maxp -1))
      (while (not (eobp))
	(or (zerop (string-to-number (buffer-substring (+ (point) 4)
                                                       (+ (point) 8))))
	    (setq maxp
		  (max maxp (string-to-number (buffer-substring (point)
                                                                (+ (point) 3))))))
	(forward-line 1))
      (kill-buffer (current-buffer))
      maxp)))


(provide 'trr-files)
;;; trr-files.el ends here
