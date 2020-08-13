;;; trr.el --- a type-writing training program on GNU Emacs.
;;; Copyright (C) 1996 YAMAMOTO Hirotaka <ymmt@is.s.u-tokyo.ac.jp>

;; Author: YAMAMOTO Hirotaka <ymmt@is.s.u-tokyo.ac.jp>
;;	KATO Kenji <kato@suri.co.jp>
;;		*Original Author
;;	INAMURA You <inamura@icot.or.jp>
;;		*Original Author
;; Created: 1 July 1996
;; Modified: 2019-10-19
;; Keywords: games faces

;;; Commentary:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'gamegrid)
(require 'picture)

(defconst trr-installator "shuji.narazaki@gmail.com"
  "Instllators name or e-mail address.")

;; User Customizable Variables.

(defgroup trr nil
  "Play a game of trr."
  :prefix "trr-"
  :group 'games)

(defface trr-text-face '((((background dark)) (:foreground "LightBlue"))
                            (t (:foreground "blue")))
  "Face for text characters." :group 'trr)

(defface trr-correct-face '((((background dark)) (:foreground "RoyalBlue"))
                            (t (:foreground "RoyalBlue")))
  "Face for correct typed character." :group 'trr)

(defface trr-miss-face '((((background dark)) (:foreground "red"))
                        (t (:foreground "red")))
  "Face for correct typed character." :group 'trr)

(defface trr-graph-face '((((background dark)) (:foreground "blue"))
                        (t (:foreground "blue")))
  "Face for the stars in Graph." :group 'trr)

(defface trr-self-face '((((background dark)) (:foreground "aquamarine"))
                        (t (:foreground "aquamarine")))
  "Face for highlighting the name of yourself." :group 'trr)

(defcustom trr-un-hyphenate t "Whether deny hyphenations."
  :type 'boolean :group 'trr)		; ハイフネーションを消すかどうか

(defcustom trr-return-is-space nil "Whether return & space is equal."
  :type 'boolean :group 'trr)		; リターンをスペースで代用するかどうか

(defcustom trr-ding-when-miss t "Whether enabled to `ding' when miss type."
  :type 'boolean :group 'trr)		; 間違えた時に ding するかどうか

(defcustom trr-japanese nil "If t, trr talk to you in Japanese."
  :type 'boolean :group 'trr)		; trrのメッセージを日本語にするかどうか

(defcustom trr-use-update-program nil
  "If t, use `update-game-score' program to write score file."
  :type 'boolean :group 'trr)

(defcustom trr-record-dir ; 個人レコード
  (expand-file-name "trrscores" gamegrid-user-score-file-directory)
  "Directory where personal records are stored."
  :type 'directory :group 'trr)

(defcustom trr-score-dir  ; グループスコア
  (expand-file-name "trrscores"
   (or gamegrid-user-score-file-directory shared-game-score-directory))
  "Directory where group score records are stored.
If `trr-use-update-program' is t, score files in this directory must exist
and be writable by `update-game-score' helper program."
  :type 'directory :group 'trr)

;; Hooks
(defvar trr-load-hook nil
  "*This hook is run only when trr files are loaded.")
(defvar trr-start-hook nil
  "*This hook is run everytime trr start.")
(defvar trr-end-hook nil
  "*This hook is run when trr is finished.")

;; trr flags
(defvar trr-start-flag nil "Non-nil means trr is running.")
					; trrが始まっているかどうか
(defvar trr-quit-flag nil "Non-nil means trr will be finished.")
					; trrを終了させるかどうか
(defvar trr-update-flag nil "Non-nil means the record is broken.")
					; 記録更新したかどうか
(defvar trr-pass-flag nil "Non-nil means current-step is passed.")
					; ステップをパスしたかどうか
(defvar trr-cheat-flag nil "Non-nil means trr text is too easy.")
					; いんちきしたかどうか
(defvar trr-beginner-flag nil "Non-nil means this play is the first time.")
					; 初めてtrrをしたかどうか
(defvar trr-random-flag t "Non-nil means trr choose the position in file at random.")
					; 文書の位置をランダムに選ぶかどうか
(defvar trr-secret-flag t "Non-nil means the record will be updated.")
					; updateしないかどうか
(defvar trr-typist-flag t "Non-nil means you are Typist.")
					; タイピストを目指すかどうか
(defvar trr-small-window-flag nil "Non-nil means the frame is too narrow.")
					; ウィンドウが小さいかどうか
(defvar trr-skip-session-flag nil "Non-nil means trr skip session.")
					; セッションを実行しないかどうか

;; Buffer names
(defun trr-trainer-menu-buffer ()
  (if trr-japanese "タイプ＆メニュー" "Type & Menu"))
(defun trr-result-buffer ()
  (if trr-japanese "-----結果-----" "----Result----"))
(defun trr-data-buffer ()
  (if trr-japanese "成績表示" "Display Record"))
(defun trr-message-buffer ()
  (if trr-japanese "メッセージ" "trr Message"))
(defun trr-log-buffer ()
  (if trr-japanese "過去の成績" "Past Records"))
(defun trr-graph-buffer ()
  (if trr-japanese "グラフ" "Display Graph"))
(defun trr-record-buffer ()
  (if trr-japanese "実行記録!!" "Result!!"))
(defun trr-display-buffer ()
  (if trr-japanese "各種表示" "trr Display"))
(defvar trr-text-file-buffer "")

;; Window Configuration
(defvar trr-prev-win-conf    nil)
(defvar trr-win-conf         nil)
(defvar trr-win-conf-typist  nil)
(defvar trr-win-conf-display nil)

;; Variables for Session
(defvar trr-eval               -1 "Score.")
					; 今回出した点数
(defvar trr-whole-char-count   0 "The number of characters of the text.")
					; テキストの文字数
(defvar trr-correct-char-count 0 "The number of correct typed characters.")
					; 正しく入力した文字数
(defvar trr-start-time         0 "Start time for a session.")
					; trr(1 session)を初めた時間
(defvar trr-end-time           0 "End time for a session.")
					; trr(1 session)を終えた時間
(defvar trr-miss-type-ratio    0 "Miss type ratio.")
					; ミス率 (/1000)
(defvar trr-type-speed         0 "The number of characters typed per minute.")
					;タイピング速度（文字数／分）
(defvar trr-ch                 0)

;; Variables for STEP
(defvar trr-steps                       0 "Current step.")
					; 現在のステップ
(defvar trr-times-of-current-step       0
  "Total times of the execution of trr in this step.")
					; このステップでの実行回数
(defvar trr-time-of-current-step        0
  "Total time of the execution of trr in this step.")
					; このステップでの実行時間
(defvar trr-whole-chars-of-current-step 0 "The number of typing in this step.")
					; このステップでのタイプ回数
(defvar trr-whole-miss-of-current-step  0
   "The number of miss typing in this step.")
					; このステップでのミスタイプ回数
(defvar trr-times-for-message                  0 "Used in trr-message.el.")
					; trr-message.elで使用する

;; other variables
(defcustom trr-directory
  (file-name-directory (or load-file-name
                           buffer-file-name))
  "Directory where text contents are stored."
  :type 'directory :group 'trr)

(defvar trr-number-of-text-lines 0 "(the number of lines in text) - 18.")
					; テキストの行数 - 18
(defvar trr-text-lines           0
   "The number of lines which should be displayed.") ; 表示すべきテキストの行数
(defvar trr-total-times          0 "Times of execution of trr.")
					; 今までの実行回数
(defvar trr-total-time           0 "Total time of execution of trr.")
					; 今までの実行時間
(defvar trr-high-score           -1 "User's High Score.")
					; 今回までの最高得点
(defvar trr-high-score-old       -1  "User's previous High Score.")
					; 前回までの最高得点
(defvar trr-elapsed-time         0)
(defvar trr-debug		 nil)

;; other functions
(defun trr-history-of-trr ()
  (if trr-japanese
      " Original(Pascal&C) -- 守山 貢\n\
 Original Author -- 加藤研児\n\
 Rewritten by -- 山本泰宇 \n\
 trr Emacs19 Ver.1.1 Apr. 1996"
    " Original(Pascal&C) -- Moriyama Mitugu\n\
 Original Author -- Katou Kenji\n\
 Rewritten by -- Yamamoto Hirotaka\n\
 trr Emacs19 Ver.1.1 Apr. 1996"))
(defun trr-current-trr ()
  (cond
   (trr-typist-flag (if trr-japanese "上級者" "Typist"))
   (trr-secret-flag (if trr-japanese "秘密主義者" "Sealed"))
   (trr-random-flag (if trr-japanese "中級者" "Trainee"))
   (t               (if trr-japanese "初級者" "Novice"))))
(defun trr-random-num ()
  (let (num
	(num1 (random))
	(num2 (random t)))
    (if (> num1 0) t
      (setq num1 (- num1)))
    (if (> num2 0) t
      (setq num2 (- num2)))
    (setq num (+ (/ num1 2) (/ num2 2)))
    (% num trr-number-of-text-lines)))

;; files used by trr
(defvar trr-update-program (expand-file-name "update-game-score" exec-directory))
(defvar trr-select-text-file)

;;; load files
(require 'trr-mesg)
(require 'trr-files)
(require 'trr-menus)
(require 'trr-graphs)
(require 'trr-sess)
(run-hooks 'trr-load-hook)

;;; Scoring function
;;; (number_of_type-(number_of_typo*10))*60/seconds  regular
;;; (number_of_type-(number_of_typo*50))*60/seconds  for Typist
(defun trr-evaluate-point (whole miss time)
  (if trr-typist-flag
      (max 0 (if (= time 0) 0 (/ (* (- whole (* miss 50)) 60) time)))
    (max 0 (if (= time 0) 0 (/ (* (- whole (* miss 10)) 60) time)))))

;;;###autoload
(defun trr ()
  "Start trr."
  (interactive)
  (or trr-prev-win-conf
      (setq trr-prev-win-conf (current-window-configuration)))
  (setq trr-skip-session-flag nil)
  (delete-other-windows)
  (if (or (< (window-height) 20) (< (window-width) 60))
      (message (if trr-japanese
		   "ウィンドウが小さすぎるわ。"
		 "Too narrow to execute trr!"))
    (unwind-protect
	(progn
	  (trr-start)
	  (setq trr-ch 0)
	  (while (trr-play-p)
	    (set-window-configuration trr-win-conf)
	    (if trr-skip-session-flag
		(progn
		  (setq trr-start-flag t)
		  (setq trr-skip-session-flag nil)
		  (set-window-configuration trr-win-conf-display)
		  (set-buffer (get-buffer-create (trr-display-buffer)))
		  (trr-print-log-for-display))
	      (trr-one-session)
	      (if (= trr-ch 3)              ; interrupt!
		  (setq trr-quit-flag t)
		(if (= trr-ch 18)           ; restart!
		    (progn
		      (trr-finish t)
		      (trr))
		  (trr-update-variables)
		  (trr-display-variables-message-graph)
		  (if (and trr-update-flag (not trr-secret-flag))
		      (trr-update-score-file trr-eval))
		  (widen))))))
      (trr-finish))))


(defun trr-start ()
  (message (if trr-japanese "ちょっと待って..." "Wait a moment!"))
  (setq trr-select-text-file
        (expand-file-name (if trr-japanese "CONTENTS.ja" "CONTENTS") trr-directory))
  (setq trr-secret-flag nil)
  (setq trr-random-flag t)
  (setq trr-typist-flag nil)
  (setq trr-start-flag nil)
  (setq trr-quit-flag nil)
  (trr-prepare-buffers)
  (picture-move-down 1)
  (picture-move-up 1)
  (run-hooks 'trr-start-hook))


(defun trr-play-p ()
  (and (not trr-quit-flag)
       (if (not trr-start-flag) ; if trr have not started
	   (progn (trr-select-text) ; then let player select text
		  (not trr-quit-flag))
	 (trr-select-menu) ; if started, let player select menu
	 (not trr-quit-flag))))


(defun trr-prepare-buffers ()
  (delete-other-windows)
  (switch-to-buffer (get-buffer-create (trr-trainer-menu-buffer)))
  (setq mode-line-format "   %b")
  (force-mode-line-update)
  (split-window (get-buffer-window (current-buffer)) 5) ; create 4 lines buffer
  (other-window 1)
  (switch-to-buffer (get-buffer-create (trr-display-buffer)))
  (setq mode-line-format "   %b")
  (force-mode-line-update)
  (setq trr-win-conf-display (current-window-configuration)) ; 4 lines + others
  (delete-other-windows)
  (switch-to-buffer (get-buffer-create (trr-trainer-menu-buffer)))
  (setq trr-win-conf-typist (current-window-configuration)) ; full screen
  (let* ((height (- (window-height) 5))
	 (text-buffer-height (1+ (- (/ height 2) (% (/ height 2) 3)))))
    (if trr-typist-flag
	(setq trr-text-lines (/ (1- (window-height)) 3)); 1 line + 2 null lines
      (setq trr-text-lines (/ (1- text-buffer-height) 3))
      (if (< trr-text-lines 3)
	  (progn
	    (setq trr-small-window-flag t)
	    (setq trr-text-lines (/ (1- (window-height)) 3)))
	(setq trr-small-window-flag nil)))
    (switch-to-buffer (get-buffer-create (trr-trainer-menu-buffer)))
    (split-window (get-buffer-window (current-buffer))
		  text-buffer-height)
    (set-buffer (get-buffer-create (trr-result-buffer)))
    (setq truncate-lines t)
    (setq mode-line-format "   %b")
    (force-mode-line-update)
    (erase-buffer)
    (trr-print-first-message-as-result)
    (switch-to-buffer-other-window (trr-result-buffer))
    (split-window (get-buffer-window (current-buffer)) 5)
    (split-window-horizontally 24)
    (other-window 1)
    (switch-to-buffer (get-buffer-create (trr-data-buffer)))
    (setq truncate-lines t)
    (setq mode-line-format "   %b")
    (force-mode-line-update)
    (trr-print-data)
    (split-window-horizontally 20)
    (other-window 1)
    (switch-to-buffer (get-buffer-create (trr-message-buffer)))
    (setq mode-line-format "   %b")
    (force-mode-line-update)
    (delete-region (point-min) (point-max))
    (insert (trr-history-of-trr))
    (other-window 1)
    (switch-to-buffer (get-buffer-create (trr-log-buffer)))
    (setq truncate-lines t)
    (setq mode-line-format "   %b")
    (force-mode-line-update)
    (split-window-horizontally 32)
    (trr-print-log)
    (other-window 1)
    (switch-to-buffer (get-buffer-create (trr-graph-buffer)))
    (setq mode-line-format "   %b")
    (force-mode-line-update)
    (trr-write-graph trr-list-of-eval 0
		     (if trr-japanese
			 "今回の得点グラフ"
		       "Score Graph for this time"))
    (recenter -1) ; move point to the last line of the graph-buffer
    (other-window 1)
    (setq trr-win-conf (current-window-configuration))))


(defun trr-print-data ()
  (save-excursion
    (switch-to-buffer (get-buffer-create (trr-data-buffer)))
    (erase-buffer)
    (insert
     (format (if trr-japanese
		 "\
 ステップ：%5d\n\
  目  標 ：%5d点\n\
 最高記録：%5d点\n\
  trr回数：%5d回"
	       "\
  STEP  : %5d\n\
 Target : %5dpt.\n\
  High  : %5dpt.\n\
  TIMES : %5d")
	     (1+ trr-steps)
	     (* (1+ trr-steps) 10)
	     (if (< trr-high-score 0) 0 trr-high-score)
	     trr-total-times))))


(defun trr-print-log ()
  (save-excursion
    (switch-to-buffer (get-buffer-create (trr-log-buffer)))
    (erase-buffer)
    (let (curstep       curtimes                curtime
       ;; current step  exec.times by each step total time by each step
	  wc          mc         curdate     curmiss curspeed curpoint)
       ;; whole count miss count update date
      (switch-to-buffer (get-buffer-create (trr-record-buffer)))
      (goto-char (point-min))
      (setq curstep 1)
      (while (not (eobp)) ; if point is not at the end of the buffer
	(setq curpoint (point))
	(setq curtimes (string-to-number
			   (buffer-substring
			    (+ curpoint 4) (+ curpoint  8))))
	(setq curtime (string-to-number
			  (buffer-substring
			   (+ curpoint 9) (+ curpoint 15))))
	(setq wc (string-to-number
			  (buffer-substring
			   (+ curpoint 16) (+ curpoint 22))))
	(setq mc (string-to-number
			  (buffer-substring
			   (+ curpoint 23) (+ curpoint 28))))
	(setq curmiss
	      (if (= wc 0) 0 (/ (* mc 1000) wc)))
	(setq curspeed
	      (if (= curtime 0) 0 (/ (* wc 60) curtime)))
	(let (j)
	  (save-excursion
	    (end-of-line)
	    (setq j (point)))
	  (setq curdate (buffer-substring (+ curpoint 29) j)))
	(forward-line) ; this causes no error if there's no line.
	(switch-to-buffer (get-buffer-create (trr-log-buffer)))
	(if (not (= wc 0))
	    (insert (format "%2d:%4d%4d%5d%4d %s\n"
			    curstep
			    curtimes
			    (/ curtime 60)
			    curspeed
			    curmiss
			    curdate)))
	(setq curstep (+ curstep 1))
	(switch-to-buffer (get-buffer-create (trr-record-buffer)))))
      (switch-to-buffer (get-buffer-create (trr-log-buffer)))
      (forward-line (- 3 (window-height))) ; if lines flood
      (delete-region (point-min) (point)) ; then delete heading records
      (if trr-japanese
	  (insert (trr-current-trr) "向けタイプトレーナ\n")
	(insert "trr for " (trr-current-trr) "\n"))
      (if trr-japanese
	  (insert "step 回  分   速  率   突破日\n")
	(insert "step tms min spd rate date\n"))))


(defun trr-print-log-for-display ()
  (save-excursion
    (switch-to-buffer (get-buffer-create (trr-display-buffer)))
    (erase-buffer)
    (let (curstep curtimes curtime curpoint passpoint
		  avepoint curmiss curspeed wc mc curdate)
      (switch-to-buffer (get-buffer-create (trr-record-buffer)))
      (goto-char (point-min))
      (setq curstep 1)
      (while (not (eobp))
	(setq curpoint (point))
	(setq passpoint (string-to-number
			   (buffer-substring
			    curpoint (+ curpoint 3))))
	(setq curtimes (string-to-number
			   (buffer-substring
			    (+ curpoint 4) (+ curpoint  8))))
	(setq curtime (string-to-number
			  (buffer-substring
			   (+ curpoint 9) (+ curpoint 15))))
	(setq wc (string-to-number
			  (buffer-substring
			   (+ curpoint 16) (+ curpoint 22))))
	(setq mc (string-to-number
			  (buffer-substring
			   (+ curpoint 23) (+ curpoint 28))))
	(setq curmiss
	      (if (= wc 0) 0 (/ (* mc 1000) wc)))
	(setq curspeed
	      (if (= curtime 0) 0 (/ (* wc 60) curtime)))
	(setq avepoint (if (= curtime 0) 0
			 (trr-evaluate-point wc mc curtime)))
	(let (j)
	  (save-excursion
	    (end-of-line)
	    (setq j (point)))
	  (setq curdate (buffer-substring (+ curpoint 29) j)))
	(forward-line)
	(switch-to-buffer (get-buffer-create (trr-display-buffer)))
	(if (not (= wc 0))
	    (insert (format (if trr-japanese
				"\
 %2d:  %3d回  %3d分  %4d字/分  %3d.%d%%  %4d点   %s   %4d\n"
			      "\
 %2d:  %3dtms %3dmin %4dtyp/mn %3d.%d%%      %4dpts. %s   %4d\n")
			    curstep
			    curtimes
			    (/ curtime 60)
			    curspeed
			    (/ curmiss 10)
			    (% curmiss 10)
			    avepoint
			    curdate
			    passpoint)))
	(setq curstep (+ curstep 1))
	(switch-to-buffer (get-buffer-create (trr-record-buffer)))))
      (switch-to-buffer (get-buffer-create (trr-display-buffer)))
      (forward-line (- 6 (window-height))) ; if lines flood
      (delete-region (point-min) (point)) ; then delete lines.
      (insert
       (format (if trr-japanese
		   "\
最高記録：%d点,   総実行回数：%d回,   総実行時間：%d分\n"
		 "\
HighScore: %dpts, total times: %dtimes, total time: %dmin\n")
	       (if (< trr-high-score 0) 0 trr-high-score)
	       trr-total-times
	       (/ trr-total-time 60)))
      (and window-system
	   ;; trr-graph-color-name
	   (put-text-property (point-min) (point) 'face
			      ;;trr-top-face-name))
			      'trr-graph-face))
      (if trr-japanese
	  (insert (concat (trr-current-trr)
			  "用での "
			  trr-text-name
			  " の記録だよ\n\n\
step   実行   実行     平均     平均     平均     突破日   突破\n\
       回数   時間   入力速度  ミス率    得点              得点\n\
---------------------------------------------------------------\n"))
	(insert (concat "trr for "
			(trr-current-trr)
			" with "
			trr-text-name
			"\n\n\
step   times  minutes  speed  miss-ratio avg-score date  the Score\n\
------------------------------------------------------------------\n")))))


(defun trr-print-result ()
  (erase-buffer)
  (insert
   (format (if trr-japanese
	       "\
 所要時間：%4d 秒\n\
  ミス率 ：%2d.%1d %%\n\
 字数／分：%4d\n\
  評  価 ：%4d %s"
	     "\
  Time   : %4d seconds\n\
miss rate: %2d.%1d %%\n\
speed    : %4d\n\
  Score  : %4d %s")
	   trr-elapsed-time
	   (/ trr-miss-type-ratio 10)
	   (% trr-miss-type-ratio 10)
	   trr-type-speed
	   trr-eval
	   (if trr-pass-flag " Pass" "Retry")))
  (goto-char (point-min)))


(defun trr-finish (&optional fl)
  (trr-kill-buffer (trr-trainer-menu-buffer))
  (trr-kill-buffer (trr-result-buffer))
  (trr-kill-buffer (trr-graph-buffer))
  (trr-kill-buffer (trr-message-buffer))
  (trr-kill-buffer (trr-data-buffer))
  (trr-kill-buffer (trr-log-buffer))
  (trr-kill-buffer (trr-display-buffer))
  (trr-save-file (trr-record-buffer) trr-record-file)
  (trr-kill-file trr-record-file)
  (trr-kill-file trr-score-file)
  (trr-kill-buffer (trr-record-buffer))
  (trr-kill-file trr-record-file)
  (or (zerop (length trr-text-file-buffer))
      (kill-buffer (get-buffer-create trr-text-file-buffer)))
  (and trr-prev-win-conf
       (set-window-configuration trr-prev-win-conf))
  (or fl
      (progn
	(setq trr-prev-win-conf nil)
	(run-hooks 'trr-end-hook)
	(message (if trr-japanese "また会う日まで...." "See you later...")))))


(defun trr-kill-buffer (buffer)
  (let ((tb (get-buffer buffer)))
    (if tb (kill-buffer tb))))


(defun trr-cheat-p ()
  (setq trr-cheat-flag
	(or
	 (if trr-typist-flag
	     (< trr-whole-char-count 520)
	   (< trr-whole-char-count 270))
	 (> trr-eval 750))))


(defun trr-update-variables ()
  (setq trr-quit-flag nil)
  (let ((started-from (trr-convert-time-string-to-second trr-start-time))
	(ended-at (trr-convert-time-string-to-second trr-end-time)))
    (setq trr-elapsed-time (- ended-at started-from))
    (if (< ended-at started-from)
	(setq trr-elapsed-time (+ trr-elapsed-time 86400))))
  (setq trr-eval
	(trr-evaluate-point trr-whole-char-count
			    (- trr-whole-char-count trr-correct-char-count)
			    trr-elapsed-time))
  (setq trr-list-of-eval (cons trr-eval trr-list-of-eval))
  (setq trr-pass-flag
	(and (>= trr-eval (* 10 (1+ trr-steps)))
	     (not (trr-cheat-p))))
  (setq trr-update-flag
        (and (or (> trr-eval trr-high-score)
        	 (< trr-high-score-old 0))
  	     (not (trr-cheat-p))))
  ;; (setq trr-update-flag t) ; for debugging
  (setq trr-beginner-flag (< trr-high-score-old 0))
  (if trr-cheat-flag nil
    (setq trr-total-time (+ trr-total-time trr-elapsed-time))
    (setq trr-total-times (1+ trr-total-times))
    (setq trr-time-of-current-step
	  (+ trr-time-of-current-step trr-elapsed-time))
    (setq trr-times-of-current-step (1+ trr-times-of-current-step))
    (setq trr-whole-chars-of-current-step
	  (+ trr-whole-char-count trr-whole-chars-of-current-step))
    (setq trr-whole-miss-of-current-step
	  (+ (- trr-whole-char-count trr-correct-char-count)
	     trr-whole-miss-of-current-step)))
  (or trr-cheat-flag (trr-write-current-data))
  (if trr-pass-flag
      (progn
	(setq trr-times-for-message trr-times-of-current-step)
	(setq trr-time-of-current-step 0)
	(setq trr-times-of-current-step 0)
	(setq trr-whole-chars-of-current-step 0)
	(setq trr-whole-miss-of-current-step 0)
	(if trr-beginner-flag
	    (setq trr-steps (/ trr-eval 10))
	  (setq trr-steps (1+ trr-steps)))))
  (if trr-update-flag
      (progn
	(setq trr-high-score-old trr-high-score)
	(setq trr-high-score trr-eval)))
  (and (< trr-high-score-old 0)
       (not trr-cheat-flag)
       (setq trr-high-score-old trr-eval))
  (let (diff)
    (setq diff (- trr-whole-char-count trr-correct-char-count))
    (setq trr-miss-type-ratio (/ (* 1000 diff) trr-whole-char-count)))
  (setq trr-type-speed (/ (* trr-whole-char-count 60) trr-elapsed-time)))


(defun trr-write-current-data ()
  (with-current-buffer (get-buffer-create (trr-record-buffer))
    (if trr-beginner-flag
	(let ((count (/ trr-eval 10)))
	  (erase-buffer)
	  (while (> count 0)
	    (if trr-japanese
		(insert "  0    0      0      0     0 ふぁいと!\n")
	      (insert "  0    0      0      0     0 cheers!  \n"))
	    (setq count (1- count)))))
    (goto-char (point-max))
    (forward-line -1)
    (delete-region (point) (point-max))
    (insert (format "%3d %4d %6d %6d %5d %s\n"
		    trr-eval
		    trr-times-of-current-step
		    trr-time-of-current-step
		    trr-whole-chars-of-current-step
		    trr-whole-miss-of-current-step
		    (trr-get-date)))
    (if trr-pass-flag ; if current step is passed, new entry should be added.
	(if trr-japanese
	    (insert "  0    0      0      0     0 ふぁいと!\n")
	  (insert "  0    0      0      0     0 cheers!  \n")))))


(defun trr-initiate-variables ()
  (setq trr-total-times 0)
  (setq trr-total-time  0)
  (setq trr-times-of-current-step 0)
  (setq trr-time-of-current-step  0)
  (setq trr-whole-chars-of-current-step 0)
  (setq trr-whole-miss-of-current-step 0)
  (set-buffer (get-buffer-create trr-text-file-buffer))
  (setq trr-number-of-text-lines (- (count-lines (point-min) (point-max)) 18))
  (set-buffer (get-buffer-create (trr-display-buffer)))
  (erase-buffer)
  (trr-read-file)
  (setq trr-high-score (trr-get-high-score))
  (setq trr-high-score-old trr-high-score)
  (setq trr-beginner-flag (< trr-high-score-old 0))
  (set-buffer (get-buffer-create (trr-record-buffer)))
  (setq trr-steps (1- (count-lines (point-min) (point-max))))
  (goto-char (point-min))
  (while (not (eobp))
    (setq trr-times-of-current-step
	     (string-to-number
	      (buffer-substring (+ (point) 4) (+ (point) 8))))
    (setq trr-total-times
	  (+ trr-total-times trr-times-of-current-step))
    (setq trr-time-of-current-step
	     (string-to-number
	      (buffer-substring (+ (point) 9) (+ (point) 15))))
    (setq trr-total-time
	  (+ trr-total-time trr-time-of-current-step))
    (setq trr-whole-chars-of-current-step
	     (string-to-number
	      (buffer-substring (+ (point) 16) (+ (point) 22))))
    (setq trr-whole-miss-of-current-step
	     (string-to-number
	      (buffer-substring (+ (point) 23) (+ (point) 28))))
    (forward-line)))


(defun trr-get-date ()
  (if (not trr-pass-flag) ; in not passed
      (if trr-japanese
	  "ふぁいと!"
	"cheers!  ")
    (let ((in-string (current-time-string))
	  (out-string nil))
      (setq out-string (concat out-string (substring in-string 22 24))); year
      (setq out-string (concat out-string "/"))
      (setq out-string (concat out-string (substring in-string 4 7))); month
      (setq out-string (concat out-string "/"))
      (if (= (string-to-char (substring in-string 8 10)) 32)
	  (setq out-string (concat out-string (substring in-string 9 10) " "))
	(setq out-string (concat out-string (substring in-string 8 10)))))))

(defun trr-convert-time-string-to-second (st)
  (let ((hr (string-to-number (substring st 11 13))) ; hour
	(min (string-to-number (substring st 14 16))) ; minute
	(sec (string-to-number (substring st 17 19)))) ; second
    (+ sec (* 60 (+ min (* 60 hr))))))

(provide 'trr)
;;; trr.el ends here

;; Local Variables:
;; time-stamp-pattern: "12/Modified:[ \t]+%:y-%02m-%02d\\\\?\n"
;; End:
