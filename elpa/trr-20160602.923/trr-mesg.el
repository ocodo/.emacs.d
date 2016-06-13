;;; trr-message.el --- (C) 1996 Yamamoto Hirotaka <ymmt@is.s.u-tokyo.ac.jp>

;;; Commentary:

;; This file is a part of trr, a type training package for GNU Emacs.
;; See the copyright notice in trr.el

;; メッセージは以下の変数の値によって決められる。
;; 暇な人が現れてもっと適切なメッセージ体系を構築してくれることを望む。
;; trr decide its messages according to the following variables.
;; I hope you build more proper messaging system.

;; trr-beginner-flag         初めて trr をしたかどうか
;;			     whether this is the first play or not
;; trr-random-flag           テキストがランダムかどうか
;;			     whether random selecting is enabled or not
;; trr-update-flag           記録更新したかどうか
;;			     whether there's necessity of updating high scores
;; trr-pass-flag             ステップをパスしたかどうか
;;			     whether the player achieved the mark point
;; trr-secret-flag           秘密主義者かどうか
;;			     whether trr won't record the player's score or not
;; trr-cheat-flag            何らかの疑わしき行為をしたかどうか
;;			     whether there's something doubtful or not
;; trr-typist-flag           タイピストを目指すかどうか
;;			     whether trr runs in typist mode or not
;; trr-steps                 現在のステップ
;;			     the player's current step
;; trr-eval                  今回出した点数
;;			     the player's current mark
;; trr-whole-char-count      テキストの文字数
;;			     the number of characters in the text
;; trr-high-score-old        前回までの最高得点
;;			     the previous high score record
;; trr-high-score            今回までの最高得点
;;			     high score record
;; trr-miss-type-ratio       ミス率 (千分率)
;;			     miss type ratio
;; trr-type-speed            タイピング速度（文字数／分）
;;			     the number of characters typed per minute
;; trr-total-times           今までの累積実行回数
;;			     the total number of trr trials
;; trr-total-time            今までの累積実行時間
;;			     the total time spends in trr trials
;; trr-times-for-message     このステップでの累積実行回数
;;			     the total number of trr trials in the current step

;;; Code:

(defvar trr-japanese)
(defvar trr-steps)
(defvar trr-eval)
(defvar trr-cheat-flag)
(defvar trr-secret-flag)
(defvar trr-typist-flag)
(defvar trr-beginner-flag)
(defvar trr-update-flag)
(defvar trr-pass-flag)
(defvar trr-high-score)
(defvar trr-high-score-old)
(defvar trr-miss-type-ratio)
(defvar trr-times-for-message)

(defun trr-print-first-message-as-result ()
  (insert  (if trr-japanese
	       " ようこそtrrの世界へ！\n\
 昼休みや仕事の後に、\n\
 或いは仕事の最中にも\n\
 頑張ってtrrに励もう！"
	     " Welcome to trr world! \n\
 Let's play trr\n\
 After lunch, class,\n\
 Even during works!")))


(defun trr-print-message ()
  (let ((fill-column (- (window-width) 3)))
    (delete-region (point-min) (point-max))
    (insert "  ")
    (trr-print-message-main)
    (fill-region (point-min) (point-max))
    (goto-char (point-min))))


(defun trr-print-message-main ()
  (let ((diff (- (* (1+ trr-steps) 10) trr-eval)))
    (cond
     (trr-cheat-flag
      (trr-message-for-cheater))
     (trr-secret-flag
      (trr-message-for-secret-player))
     (trr-typist-flag
      (trr-message-for-typist))
     (trr-beginner-flag
      (trr-message-for-beginner))
     ((and trr-update-flag trr-pass-flag)
      (insert
       (format (if trr-japanese
		   "ステップ%d突破そして記録更新おめでとう。"
		 "Great. You've cleared the step %d with the new record!")
	       trr-steps))
      (if (< (% trr-high-score 100) (% trr-high-score-old 100))
	  (progn
	    (trr-message-specially-for-record-breaker))
	(trr-message-for-record-breaker))
      (setq trr-high-score-old trr-high-score))
     (trr-update-flag
      (insert (if trr-japanese
		  "記録更新おめでとう！"
		"Congratulations! You've marked the new record!"))
      (trr-message-for-record-breaker)
      (setq trr-high-score-old trr-high-score))
     (trr-pass-flag
      (insert
       (format (if trr-japanese
		   "ステップ%d突破おめでとう。"
		 "Nice! You've cleared the step %d.")
	       trr-steps))
      (trr-message-for-success))
     ((= trr-eval 0)
      (insert (if trr-japanese
		  "０点なんて恥ずかしいわ。もっと努力しなさい。"
		"Arn't you ashmed of having marked such an amazing score 0!")))
     ((< diff  60)
      (trr-message-for-failed-one-1 diff))
     ((or (< diff 100) (> trr-miss-type-ratio 30))
      (trr-message-for-failed-one-2 diff))
     (t
      (trr-message-for-failed-one-3 diff)))))


(defun trr-message-for-cheater ()
  (cond
   ((> trr-eval 750)
    (insert (if trr-japanese
		"そんなことでいいの？恥を知りなさい。"
	      "Aren't you ashamed of having done such a thing?")))
   ((< trr-whole-char-count 270)
    (insert (if trr-japanese
		"卑怯よ。テキストが少な過ぎるわ。それでうれしい？"
	      "That's not fair! Too few letters in the text!")))
   ((and (< trr-whole-char-count 520) trr-typist-flag)
    (insert (if trr-japanese
		"卑怯よ。テキストが少な過ぎるわ。それでうれしい？"
	      "That's not fair! Too few letters in the text!")))))


(defun trr-message-for-secret-player ()
  (cond
   (trr-pass-flag
    (setq trr-secret-flag nil)
    (setq trr-update-flag nil)
    (setq trr-beginner-flag nil)
    (trr-print-message-main)
    (setq trr-secret-flag t))
   ((> trr-eval 300)
    (insert (if trr-japanese
		"こんな高い得点を出す方がどうして秘密にしておくの？"
	      "What a good typist you are! You'd unveil your score.")))
   ((> trr-eval 200)
    (insert (if trr-japanese
		"業界標準を越えてるわ。秘密にする必要は全くないわよ。"
	      "Your score now reaches to the World standard. Go out public trr world!")))
   ((> trr-eval 120)
    (insert (if trr-japanese
		"恥ずかしくない点だわ。秘密にするのはもうやめましょう。"
	      "Good score! Put an end to play in this secret world.")))
   (t
    (insert (if trr-japanese
		"公開するとちょっと恥ずかしい点だわ。しばらく秘密で続けましょう。"
	      "Keep your score secret for a while.")))))


(defun trr-message-for-beginner ()
  (cond
   ((= trr-eval 0)
    (insert (if trr-japanese
		"０点というのは問題だわ。これからかなりの努力が必要よ。道のりは長いけど頑張りましょう。"
	      "0point... hopeless it is! You have to do much effort to step your level up.")))
   ((< trr-eval 40)
    (insert (if trr-japanese
		"少なくとも英文字ぐらいは絶対覚えること。業界必須の100点に向けてこれから頑張りましょう。"
	      "You need to learn at least the position of the alphabet keys. Set your sights on 100pt: the World indispensable point.")))
   ((< trr-eval 80)
    (insert (if trr-japanese
		"キー配置も大分覚えたようだけどまだまだだわ。業界必須の100点に向けてこれから頑張りましょう。"
	      "Yes, you've learned the positions of keys; but still more! Set your sights on 100pt: the World indispensable point.")))
   ((< trr-eval 130)
    (insert (if trr-japanese
		"基礎的な技術は身に付けているようだけどまだまだだわ。業界標準の200点に向けてこれから頑張りましょう。"
	      "You've learned some basic techniques; but still more! Go forward to 200pt: the World standard point.")))
   ((< trr-eval 180)
    (insert (if trr-japanese
		"なかなかの実力ね。でもスピードと正確さが少し足りないわ。業界標準の200点に向けてもう少し頑張りましょう。"
	      "Your typing skill is rather high. More speedy & exactly! Go forward to 200pt: the World standard point.")))
   ((< trr-eval 280)
    (insert (if trr-japanese
		"なかなかやるわね。もう少し頑張れば業界目標の300点をきっと突破できるわ。"
	      "Nice. With some effort, you will surely reach 300pt: the World highly standard.")))
   ((< trr-eval 380)
    (insert (if trr-japanese
		"すごいわね。初めてでこれぐらい出せれば十分だわ。でも業界一流の400点に向けてもう少し頑張りましょう。"
	      "Great. You have had sufficient skill. But push yourself to 400pt: the World firstclass.")))
   ((< trr-eval 480)
    (insert (if trr-japanese
		"すっごい！こんな点を出す人は滅多にいないわよ。ひょっとしてプロではないかしら？"
	      "Wonderful score! You may be a proffesional typist?")))
   (t
    (insert (if trr-japanese
		"あまりにも超人的だわ。きっとギネスブックに載るわよ。"
	      "Too high score. You are sure to get a entry of the Guiness Book.")))))


(defun trr-message-for-success ()
  (cond
   ((>= (- trr-eval (* 10 (1+ trr-steps))) 100)
    (insert (if trr-japanese
		"あなたには簡単過ぎたようね。"
	      "This step must have been quite easy for you.")))
   ((<= trr-times-for-message 2)
    (insert (if trr-japanese
		"軽く突破したわね。"
	      "You made it effortlessly.")))
   ((<= trr-times-for-message 4)
    (insert (if trr-japanese
		"わりと簡単に突破したわね。"
	      "You made it!")))
   ((<= trr-times-for-message 8)
    (insert (if trr-japanese
		"ちょっとてこずったようね。"
	      "You carried out with a little trouble.")))
   ((<= trr-times-for-message 16)
    (insert (if trr-japanese
		"だいぶてこずったようね。"
	      "With much trouble, you accomplished this step's mark!")))
   ((<= trr-times-for-message 32)
    (insert (if trr-japanese
		"よく頑張ったわね。"
	      "You've sweat it out. Nice record.")))
   ((<= trr-times-for-message 64)
    (insert (if trr-japanese
		"随分苦労したようね。"
	      "You've had a very hard time.")))
   ((<= trr-times-for-message 128)
    (insert (if trr-japanese
		"苦しみぬいたわね。"
	      "You've gone through all sorts of hardships. ")))
   (t
    (insert
     (format (if trr-japanese
		 "%d回も挑戦するなんてすごいわ。執念でやりとげたわね。"
	       "You've challenged this step %d times. Great efforts! ")
	     trr-times-for-message)))))


(defun trr-message-for-failed-one-1 (diff)
  (cond
   ((< diff 10)
    (insert (if trr-japanese
		"あとほんの少しだったのに....本当に惜しかったわね。"
	      "Your score is slightly lower than the mark... How maddening!")))
   ((< diff 20)
    (insert (if trr-japanese
		"惜しかったわね。"
	      "Disappointing!")))
   ((< diff 30)
    (insert (if trr-japanese
		"その調子よ。"
	      "That's it!")))
   ((< diff 40)
    (insert (if trr-japanese
		"もう一息だわ。でも息抜きはだめよ。"
	      "Just one more effort. Don't goof off!")))
   ((< diff 50)
    (insert (if trr-japanese
		"頑張ればきっとできるわ。"
	      "With much effort, and you will make it.")))
   (t
    (insert (if trr-japanese
		"努力あるのみよ。"
	      "What you have to do is nothing but making all possible effort.")))))


(defun trr-message-for-failed-one-2 (diff)
  (cond
   ((> trr-miss-type-ratio 60)
    (insert (if trr-japanese
		"ミスがあまりにも多過ぎるからダメなのよ。とにかく正確に打つ練習に励みなさい。もうそれしか方法はないわ。"
	      "Your hopeless point is based on your enormous misses! Practice the typing paying attention to correctness of typing keys.")))
   ((> trr-miss-type-ratio 40)
    (insert (if trr-japanese
		"ミスが多過ぎるわ。初心に帰って一つ一つ慎重に打つ練習をしなさい。"
	      "Too many wrong types! Remember your original purpose.")))
   ((> trr-miss-type-ratio 24)
    (insert (if trr-japanese
		"ミスが多いわ。正確に打つ練習をしなさい。"
	      "You failed frequently. Type accurate!")))
   ((> trr-miss-type-ratio  8)
    (insert (if trr-japanese
		"練習に練習を重ねなさい。"
	      "Keep in practice.")))
   (t
    (insert (if trr-japanese
		"正確に打ってるようだけどスピードが遅すぎるわ。速く打つ練習に励みなさい。"
	      "You typed accurately, but too slow! Type more quickly.")))))


(defun trr-message-for-failed-one-3 (diff)
  (cond
   ((< diff 110)
    (insert (if trr-japanese
		"「trrの道は一日にしてならず」"
	      "\"trr was not built in a day.\"")))
   ((< diff 120)
    (insert (if trr-japanese
		"「trrに王道なし」"
	      "\"There is no royal road to trring.\"")))
   ((< diff 130)
    (insert
     (format (if trr-japanese
		 "あらまぁ。%d点を出した人がたったの%d点なんていったいどうしたのよ。"
	       "Oh, no! Your best is %d, however marked %d point this time! What on earth be with you?")
	     trr-high-score trr-eval)))
   ((< diff 140)
    (insert
     (format (if trr-japanese
		 "%d点はまぐれだったの？"
	       "Is the fact once you marked %d point an illusion?")
	     trr-high-score)))
   (t
    (insert (if trr-japanese
		"あなたの実力ってこの程度だったのね。"
	      "Your real ability is no more than this point. isn't it?")))))


(defun trr-message-specially-for-record-breaker ()
  (cond
   ((< trr-high-score-old 100)
    (insert (if trr-japanese
		"ついに業界必須の100点突破ね！これからは業界標準の200点を目指して頑張りましょう。"
	      "Congratulations! You reaches 100pt: the World indispensable. Next your target is 200pt: the World standard.")))
   ((< trr-high-score-old 200)
    (insert (if trr-japanese
		"ついに業界標準の200点突破ね！これからは業界目標の300点を目指して頑張りましょう。"
	      "Congratulations! You reaches 200pt: the World standard. Next your target is 300pt: the World highly standard.")))
   ((< trr-high-score-old 300)
    (insert (if trr-japanese
		"ついに業界目標の300点突破ね！これからは業界一流の400点を目指して頑張りましょう。"
	      "Congratulations! You reaches 300pt: the World highly standard. Next your target is 400pt: the World firstclass.")))
   ((< trr-high-score-old 400)
    (insert (if trr-japanese
		"ついに業界一流の400点突破ね！これからは業界超一流の500点を目指して頑張りましょう。"
	      "Congratulations! You reaches 400pt: the World firstclass. Next your target is 500pt: the world superclass.")))
   ((< trr-high-score-old 500)
    (insert (if trr-japanese
		"ついに業界超一流の500点突破ね！これからは業界頂点の600点を目指して頑張りましょう。"
	      "Congratulations! You reaches 500pt: the world superclass. Next your target is 600pt: the World supreme.")))
   (t
    (insert (if trr-japanese
		"あなたのようなすごい人は初めてよ。プロになれるわ。"
	      "You are the most marvelous typist I've ever met. The title \"trrer\" suits you well!")))))


(defun trr-message-for-record-breaker ()
  (cond
   ((< trr-high-score  67)
    (insert (if trr-japanese
		"業界必須の100点指して頑張って。"
	      "Keep aiming at 100pt: the World indispensable.")))
   ((< trr-high-score 100)
    (insert (if trr-japanese
		"業界必須の100点までもうすぐよ。"
	      "You are close to 100pt: the World indispensable.")))
   ((< trr-high-score 167)
    (insert (if trr-japanese
		"業界標準の200点目指して頑張って。"
	      "Keep aiming at 200pt: the World standard.")))
   ((< trr-high-score 200)
    (insert (if trr-japanese
		"業界標準の200点までもうすぐよ。"
	      "You are close to 200pt: the World standard.")))
   ((< trr-high-score 267)
    (insert (if trr-japanese
		"業界目標の300点目指して頑張って。"
	      "Keep aiming at 300pt: the World highly standard.")))
   ((< trr-high-score 300)
    (insert (if trr-japanese
		"業界目標の300点までもうすぐよ。"
	      "You are close to 300pt: the World highly standard.")))
   ((< trr-high-score 367)
    (insert (if trr-japanese
		"業界一流の400点目指して頑張って。"
	      "Keep aiming at 400pt: the World firstclass.")))
   ((< trr-high-score 400)
    (insert (if trr-japanese
		"業界一流の400点までもうすぐよ。"
	      "You are close to 400pt: the World firstclass.")))
   ((< trr-high-score 467)
    (insert (if trr-japanese
		"業界超一流の500点目指して頑張って。"
	      "Keep aiming at 500pt: the world superclass.")))
   ((< trr-high-score 500)
    (insert (if trr-japanese
		"業界超一流の500点までもうすぐよ。"
	      "You are close to 500pt: the world superclass.")))
   ((< trr-high-score 567)
    (insert (if trr-japanese
		"業界頂点の600点まで目指して頑張って。"
	      "Keep aiming at 600pt: the World supreme.")))
   ((< trr-high-score 600)
    (insert (if trr-japanese
		"業界頂点の600点までもうすぐよ。"
	      "You are close to 600pt: the World supreme.")))
   (t
    (insert (if trr-japanese
		"よくここまでやるわね。あなたの目標は一体何なの？"
	      "What is interesting to you? What you are aiming at?")))))


(defun trr-message-for-typist ()
  (cond
   (trr-beginner-flag
    (insert (if trr-japanese
		"タイピストへの道は険しいわよ。少なくとも300点をコンスタントに出すように頑張って。"
	      "The way to the typist is severe. Keep makeing 300pt every time."))
    (setq trr-beginner-flag nil))
   ((and trr-pass-flag (not trr-update-flag))
    (setq trr-typist-flag nil)
    (trr-print-message-main)
    (setq trr-typist-flag t))
   ((and trr-update-flag trr-pass-flag)
    (insert (if trr-japanese
		"記録更新そして"
	      "You've marked a new record. And "))
    (setq trr-typist-flag nil)
    (setq trr-update-flag nil)
    (trr-print-message-main)
    (setq trr-typist-flag t))
   (trr-update-flag (insert (if trr-japanese
				"記録更新おめでとう！"
			      "Nice! You've marked a new record.")))
   ((> trr-miss-type-ratio 30)
    (insert (if trr-japanese
		"あなたには無理よ。タイピストになろうなんて当分考えないことね。"
	      "You are not up to Typist mode. Leave here for a while.")))
   ((> trr-miss-type-ratio 20)
    (insert (if trr-japanese
		"０点なんて恥ずかしいわね。この屈辱を胸に深く刻み込みなさい。"
	      "0pt! Aren't you ashamed?  Engrave this humiliation deeply engraved on my mind.")))
   ((> trr-miss-type-ratio 15)
    (insert (if trr-japanese
		"ミスがあまりにも多過ぎるわ。石橋を叩いて渡るようにタイプしなさい。"
	      "Excessively many miss types! Make assurance double sure.")))
   ((> trr-miss-type-ratio 10)
    (insert (if trr-japanese
		"ミスが多過ぎるわ。もっと慎重にタイプしなさい。"
	      "Too many typos. Type more carefully.")))
   ((> trr-miss-type-ratio 6)
    (insert (if trr-japanese
		"ミスが多いわ。もっと慎重にタイプした方がいいわよ。"
	      "Many typos. Take more care of typing.")))
   (t
    (setq trr-typist-flag nil)
    (trr-print-message-main)
    (setq trr-typist-flag t))))


(provide 'trr-mesg)
;;; trr-mesg.el ends here
