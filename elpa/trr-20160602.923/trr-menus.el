;;; trr-menus --- (C) 1996 Yamamoto Hirotaka <ymmt@is.s.u-tokyo.ac.jp>

;;; Commentary:

;; This file is a part of trr, a type training package for GNU Emacs.
;; See the copyright notice in trr.el

;; answer getting function

;;; Code:

(defvar trr-select-text-file)
(defvar trr-japanese)
(defvar trr-quit-flag)
(defvar trr-installator)
(defvar trr-skip-session-flag)
(defvar trr-win-conf)
(defvar trr-return-is-space)
(defvar trr-ding-when-miss)
(defvar trr-un-hyphenate)
(defvar trr-random-flag)
(defvar trr-typist-flag)
(defvar trr-secret-flag)
(defvar trr-win-conf-display)
(defvar trr-list-of-speed)
(defvar trr-skipped-step)
(defvar trr-text-name)
(defvar trr-list-of-miss)
(defvar trr-list-of-average)
(defvar trr-list-of-time)
(defvar trr-list-of-times)
(defvar trr-text-lines)
(defvar trr-record-file)
(defvar trr-text-file-buffer)
(defvar trr-score-file)

(declare-function trr-trainer-menu-buffer "trr.el")
(declare-function trr-current-trr "trr.el")
(declare-function trr-decide-trr-text "trr.el")
(declare-function trr-initiate-files "trr.el")
(declare-function trr-initiate-variables "trr.el")
(declare-function trr-print-log "trr.el")
(declare-function trr-print-data "trr.el")
(declare-function trr-finish "trr.el")
(declare-function trr-prepare-buffers "trr.el")
(declare-function trr-read-file "trr.el")
(declare-function trr-display-buffer "trr.el")
(declare-function trr-show-ranking "trr.el")
(declare-function trr-get-graph-points "trr.el")
(declare-function trr-write-graph "trr.el")
(declare-function trr-print-log-for-display "trr.el")
(declare-function trr-save-file "trr.el")
(declare-function trr-record-buffer "trr.el")
(declare-function trr-kill-file "trr.el")

(defun trr-get-answer (string1 string2 max)
  (let ((answer (string-to-number (read-from-minibuffer string1))))
    (while (or (<= answer 0) (> answer max))
      (message string2)
      (sleep-for 1.2)
      (setq answer (string-to-number (read-from-minibuffer string1))))
    answer))

(defun trr-select-text ()
  "Menus definition."
  (delete-other-windows)
  (switch-to-buffer (get-buffer-create (trr-trainer-menu-buffer)))
  (erase-buffer)
  (insert-file-contents trr-select-text-file)
  (untabify (point-min) (point-max))
  (goto-char (point-min))
  (let ((kill-whole-line t))
    (while (not (eobp))
      (if (or (= (char-after (point)) 35) ; comment begins with #
	      (= (char-after (point)) 10))
	  (kill-line)
	(forward-line))))
  (goto-char (point-min))
  (let ((num 1) max-num)
    (while (not (eobp))
      (insert
       (format "%4d. " num))
      (while (not (= (char-after (point)) 32)) (forward-char))
      (while      (= (char-after (point)) 32)  (forward-char))
      (while (not (= (char-after (point)) 32)) (forward-char))
      (while      (= (char-after (point)) 32)  (forward-char)) ; need comment
      (while (not (= (char-after (point)) 32)) (forward-char))
      (kill-line)
      (forward-line 1)
      (setq num (1+ num)))
    (setq max-num num)
    (goto-char (point-min))
    (if trr-japanese
	(insert (trr-current-trr)
		"向けタイプトレーナー：\n\nテキストの選択：\n")
      (insert "trr for " (trr-current-trr) ": \n\nChoose a text: \n"))
    (goto-char (point-max))
    (insert (if trr-japanese
		(concat "\n\n  何か入れて欲しい document があれば\n "
			trr-installator
			" までお問い合わせ下さい。\n")
	      (concat "\n  If you have some document to use in trr, consult with\n "
		      trr-installator
		      "\n")))
    (insert (if trr-japanese
		"\n各種の設定：\n  97. trrの終了\n  98. 設定値の変更\n"
	      "\nSet up: \n  97. Quit trr\n  98. Change options.\n"))
    (if (not trr-skip-session-flag)
	(insert (if trr-japanese
		    "  99. テキスト選択後メニュー画面に移行\n"
		  "  99. move to menu after choose a text\n"))
      (insert (if trr-japanese
		  "\nテキスト選択後メニュー画面に移行"
		"\nmove to menu after choose a text")))
    (setq num
	  (if trr-japanese
	      (trr-get-answer "どれにする？ " "はっきりしなさい！" 99)
	    (trr-get-answer "Input an integer: " "Don't hesitate!" 99)))
    (if (and (or (< num 0) (> num max-num))
	     (/= num 97)
	     (/= num 98)
	     (or trr-skip-session-flag
		 (/= num 99)))
	(setq num (if trr-japanese
		      (trr-get-answer "もう一度選んで "
				      "テキストしか選べないわ"
				      max-num)
		    (trr-get-answer "Choose again: "
				    "Text is the only way left to you!"
				    max-num))))
    (cond
     ((= num 97) (setq trr-quit-flag t))
     ((= num 98) (trr-change-flag) (trr-select-text))
     ((= num 99)
      (setq trr-skip-session-flag t)
      (trr-select-text))
     (t
      (trr-decide-trr-text num)
      (trr-initiate-files)
      (trr-initiate-variables)
      (trr-print-log)
      (trr-print-data)
      (bury-buffer)
      (set-window-configuration trr-win-conf)))))


(defun trr-change-flag (&optional loop)
  (delete-other-windows)
  (switch-to-buffer (get-buffer-create (trr-trainer-menu-buffer)))
  (erase-buffer)
  (let (num)
    (insert (if trr-japanese
		(concat "\
次の中から選んで下さい。\n\
\n\
1. 初級者向けのタイプトレーナ\n\
   評価関数は（打文字数－（誤打数＊１０））＊６０／（秒数）\n\
   テキストはステップ毎に同じものを表示\n\
\n\
2. 中級者向けのタイプトレーナ（デフォールトはこれに設定される）\n\
   評価関数は（打文字数－（誤打数＊１０））＊６０／（秒数）\n\
\n\
3. 上級者向けのタイプトレーナ\n\
   評価関数は（打文字数－（誤打数＊５０））＊６０／（秒数）\n\
   １回の実行で必要なタイプ量が多い\n\
\n\
4. 秘密主義者向けのタイプトレーナ\n\
   初級者向けのタイプトレーナと同じであるが、\n\
   ハイスコアの登録を行なわない\n\
\n"
			(if trr-return-is-space
			    "5. [toggle] 行末のリターンはスペースで代替できる\n\n"
			  "5. [toggle] 行末のリターンはリターンを押さなければならない\n\n")
			"\
6. [toggle] メッセージは日本語で表示\n\n"
			(if trr-ding-when-miss
			    "7. [toggle] 間違えた時に ding(音を鳴らす) する\n\n"
			  "7. [toggle] 間違えた時に ding(音を鳴らす) しない\n\n")
			(if trr-un-hyphenate
			    "8. [toggle] ハイフネートされた単語を元に戻す\n\n"
			  "8. [toggle] ハイフネーションを残したままにする\n\n")
			"9. 最初のメニューに戻る\n\n")
	      (concat "Select a number (1 - 9)\n\n\
1. trr for Novice.\n\
The function which evaluate your score is,\n\
(key - (wrong * 10)) * 60 / time\n\
where key is the number of your key stroke,\n\
wrong is the number of your miss type, and\n\
time is the seconds that is taken your finishing typing the text.\n\
In every STEP, trr shows the same text.\n\
\n\
2. trr for Trainee.\n\
The function which evaluate your score is the same as Novice.\n\
This is the default level.\n\
\n\
3. trr for Typist.\n\
The function which evaluate your score is,\n\
(key - (wrong * 50)) * 60 / time\n\
In this level you have to type much more than Trainee or Novice.\n\
\n\
4. trr in Secret mode.\n\
same as Novice, except that your score won't be recorded\n\
in Score-file.\n\n"
(if trr-return-is-space
    "5. If select, trr will require that you type RET for the return code\n\
at the end of a line.\n\n"
  "5. If select, trr will allow you to type SPC for the return code\n\
at the end of a line.\n\n")
"6. [toggle] Display messages in English\n\n"
(if trr-ding-when-miss
    "7. Make trr shut when miss-type\n\n"
  "7. Make trr ding when miss-type\n\n")
(if trr-un-hyphenate
    "8. [toggle] deny hyphenation from text\n"
    "8. [toggle] leave hyphenated words untouched\n")
    "9. Back to top menu\n\n")))
  (setq num (if trr-japanese
                (trr-get-answer "どれにする？ " "いったいどれにするの？" 9)
                (trr-get-answer "which do you want to change? "
                                "Don't waver!" 9)))
  (cond
   ((= num 1)
    (setq trr-random-flag nil)
    (setq trr-typist-flag nil)
    (setq trr-secret-flag nil))
   ((= num 2)
    (setq trr-random-flag t)
    (setq trr-typist-flag nil)
    (setq trr-secret-flag nil))
   ((= num 3)
    (setq trr-random-flag t)
    (setq trr-typist-flag t)
    (setq trr-secret-flag nil))
   ((= num 4)
    (setq trr-random-flag nil)
    (setq trr-typist-flag nil)
    (setq trr-secret-flag t))
   ((= num 5)
    (setq trr-return-is-space (not trr-return-is-space))
    (trr-change-flag t))
   ((= num 6)
    (trr-finish t)
    (setq trr-japanese (not trr-japanese))
    (trr-prepare-buffers)
    (trr-change-flag t))
   ((= num 7)
    (setq trr-ding-when-miss (not trr-ding-when-miss))
    (trr-change-flag t))
   ((= num 8)
    (setq trr-un-hyphenate (not trr-un-hyphenate))
    (trr-change-flag t)))
  (if (or (not loop) (= num 9))
      (progn
        (switch-to-buffer (get-buffer-create (trr-trainer-menu-buffer)))
        (let* ((height (- (window-height) 5))
	       (text-buffer-height (1+ (- (/ height 2) (% (/ height 2) 3)))))
          (if trr-typist-flag
	      (setq trr-text-lines (/ (1- (window-height)) 3))
            (setq trr-text-lines (/ (1- text-buffer-height) 3))))))))


(defun trr-select-menu ()
  (set-buffer (get-buffer-create (trr-trainer-menu-buffer)))
  (erase-buffer)
  (if trr-japanese
      (insert "\
 1. 終了               2. 実行                3. ハイスコア\n\
 4. 平均速度グラフ     5. 平均ミス率グラフ    6. 平均得点グラフ\n\
 7. 実行時間グラフ     8. 実行回数グラフ      9. 突破点数グラフ\n\
10. 過去の成績        11. テキストの変更     12. 設定の変更")
    (insert "\
 1. Quit	       2. Execute again	       3. High Scores\n\
 4. Typing Speed Graph 5. Freq. of Typo Graph  6. Score Graph\n\
 7. Time Graph	       8. Num. of Trial Graph  9. Breaking Score Graph\n\
10. Past results      11. Choose another text 12. Change options"))
  (let ((num (if trr-japanese
		 (trr-get-answer "どうするの？ " "はっきりしなさい！" 12)
	       (trr-get-answer "What do you want to do? "
			       "Commit yourself!" 12))))
    (cond
     ((= num 1) (setq trr-quit-flag t) nil)
     ((= num 2)
      (trr-read-file) ;  Read next text
      t)
     ((= num 3)
      (set-window-configuration trr-win-conf-display)
      (switch-to-buffer (get-buffer-create (trr-display-buffer)))
      (trr-show-ranking)
      (trr-select-menu))
     ((= num 4)
      (set-window-configuration trr-win-conf-display)
      (switch-to-buffer (get-buffer-create (trr-display-buffer)))
      (trr-get-graph-points)
      (trr-write-graph trr-list-of-speed trr-skipped-step
		       (concat (if trr-japanese
				   "ステップ－平均スピード（文字／分）グラフ"
				 "STEP <-> SPEED(type / minute) Graph")
			       "\t" trr-text-name))
      (trr-select-menu))
     ((= num 5)
      (set-window-configuration trr-win-conf-display)
      (switch-to-buffer (get-buffer-create (trr-display-buffer)))
      (trr-get-graph-points)
      (trr-write-graph trr-list-of-miss trr-skipped-step
		       (concat (if trr-japanese
				   "ステップ－平均ミス率（/1000）グラフ"
				 "STEP <-> avg.Miss-ratio(/1000) Graph")
			       "\t" trr-text-name))
      (trr-select-menu))
     ((= num 6)
      (set-window-configuration trr-win-conf-display)
      (switch-to-buffer (get-buffer-create (trr-display-buffer)))
      (trr-get-graph-points)
      (trr-write-graph trr-list-of-average trr-skipped-step
		       (concat (if trr-japanese
				   "ステップ－平均得点グラフ"
				 "STEP <-> avg.SCORE Graph")
			       "\t" trr-text-name))
      (trr-select-menu))
     ((= num 7)
      (set-window-configuration trr-win-conf-display)
      (switch-to-buffer (get-buffer-create (trr-display-buffer)))
      (trr-get-graph-points)
      (trr-write-graph trr-list-of-time trr-skipped-step
		       (concat (if trr-japanese
				   "ステップ－実行時間（分）グラフ"
				 "STEP <-> TIME(min) Graph")
			       "\t" trr-text-name))
      (trr-select-menu))
     ((= num 8)
      (set-window-configuration trr-win-conf-display)
      (switch-to-buffer (get-buffer-create (trr-display-buffer)))
      (trr-get-graph-points)
      (trr-write-graph trr-list-of-times trr-skipped-step
		       (concat (if trr-japanese
				   "ステップ－実行回数グラフ"
				 "STEP <-> times (the number of execution of trr) Graph")
			       "\t" trr-text-name))
      (trr-select-menu))
     ((= num 9)
      (set-window-configuration trr-win-conf-display)
      (set-buffer (get-buffer-create (trr-display-buffer)))
      (trr-get-graph-points)
      (trr-write-graph trr-list-of-value trr-skipped-step
		       (concat (if trr-japanese
				   "ステップ－突破点数グラフ"
				 "STEP <-> ACHIEVEMENT_SCORE Graph")
			       "\t" trr-text-name))
      (trr-select-menu))
     ((= num 10)
      (set-window-configuration trr-win-conf-display)
      (set-buffer (get-buffer-create (trr-display-buffer)))
      (trr-print-log-for-display)
      (trr-select-menu))
     ((= num 11)
      (trr-save-file (trr-record-buffer) trr-record-file)
      (trr-kill-file trr-record-file)
      (trr-kill-file trr-score-file)
      (trr-kill-file trr-record-file)
      (or (zerop (length trr-text-file-buffer))
	  (kill-buffer trr-text-file-buffer))
      (trr-select-text)
      (not trr-quit-flag))
     ((= num 12)
      (trr-save-file (trr-record-buffer) trr-record-file)
      (trr-kill-file trr-record-file)
      (trr-kill-file trr-score-file)
      (trr-kill-file trr-record-file)
      (or (zerop (length trr-text-file-buffer))
	  (kill-buffer trr-text-file-buffer))
      (trr-change-flag)
      (trr-select-text)
      (not trr-quit-flag))
     )))


(provide 'trr-menus)
;;; trr-menus.el ends here
