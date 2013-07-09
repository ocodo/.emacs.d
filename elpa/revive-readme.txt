		Resume Emacs:		revive.el

[What is `revive'?]

	  Revive.el  saves current editing  status including  the window
	splitting   configuration,   which   can't   be   recovered   by
	`desktop.el' nor by `saveconf.el', into a file  and reconstructs
	that status correctly.

[Installation]

	  Put revive.el into your elisp  directory included in load-path
	and write the next expressions.

	  (autoload 'save-current-configuration "revive" "Save status" t)
	  (autoload 'resume "revive" "Resume Emacs" t)
	  (autoload 'wipe "revive" "Wipe Emacs" t)

	And define favorite keys to those functions.  Here is a sample.

	  (define-key ctl-x-map "S" 'save-current-configuration)
	  (define-key ctl-x-map "F" 'resume)
	  (define-key ctl-x-map "K" 'wipe)

[How to use]

	 Call `save-current-configuration' (`C-x S' if you define key as
	above) when  you want to   save current editing status  and call
	`resume' to restore it.  Numerical prefix  arg to them specifies
	the buffer number in which the editing status will be saved.

		[Sample Operations]
		C-u 2 C-x S		;save status into the buffer #2
		C-u 3 C-x F		;load status from the buffer #3

[Save Variables]

	  Revive.el can save global or local variables.  The default
	variables to be saved are in revive:save-variables-global-default
	and revive:save-variables-local-default.  If you want to save other
	global/local variables, define them in
	revive:save-variables-global-private or
	revive:save-variables-local-private in a form of a list.  If you
	are using `gmhist', the next expression in ~/.emacs is useful.

		(setq revive:save-variables-global-private
		      '(file-history buffer-history minibuffer-history))

[Restoring abnormal buffers]

	  The variable revive:major-mode-command-alist-default holds the
	list of  major-mode  name  vs.   command  name.   For  example,
	mh-rmail  command sees the directory ~/Mail/inbox/ and sets  the
	major-mode  to  'mh-folder-mode.   And  gnus  command  sets  the
	major-mode to  'gnus-Group-mode.   If  you want to define  other
	relations between major-mode and command,  set the user variable
	revive:major-mode-command-alist-private as follows:

		(setq revive:major-mode-command-alist-private
		  '((your-own-mode	. your-own)
		    (foo-mode		. foo)
		    ("*Hanoi*"		. hanoi)

	it tells revive.el to execute the corresponding command when the
	saved configuration has the buffer with that major-mode. To know
	the major-mode of a certain buffer, type `M-ESC' and `major-mode
	CR'.   And as  you  see above, buffer-name  string  can be  used
	instead of major-mode symbol.

	  For  other  special modes that  cannot be resumed by executing
	certain function,  define a function to resume and declare it in
	the  variable revive:major-mode-command-alist-private.  In  that
	function, the following values are available.

		(revive:prop-file-name x)	;file name
		(revive:prop-buffer-name x)	;buffer name
		(revive:prop-major-mode x)	;major-mode
		(revive:prop-point x)		;(point)
		(revive:prop-mark x)		;(mark)
		(revive:prop-varlist x)		;alist of pairs of (var . val)

[For programmers]

	  This program provides two powerful functions:

		* current-window-configuration-printable
		* restore-window-configuration

	When you want to save a screen your program manages into a file,
	and  restore  it,  save  the  return  value  of  current-window-
	configuration-printable, and read and give it to restore-window-
	configuration.

	*Sample*
	;;To save
	(insert (prin1-to-string (current-window-configuration-printable)))
	;;To restore
	(restore-window-configuration (read (current-buffer)))

	Since set-window-configuration cannot set the configuration of
	other frame, the program as below should be useful.

	*Sample*
	(defun frame-copy-configuration (nth)
	  "Copy confinguration of current frame to NTH next frame."
	  (let ((config (current-window-configuration-printable)))
	    (other-frame nth)
	    (restore-window-configuration config)))

[Copying]

	  This program is distributed as a free  software. The author is
	not responsible  for  any  possible   defects   caused  by  this
	software.

	  Comments  and bug   reports  are welcome. Don't  hesitated  to
	report.  My possible e-mail address is following.

							yuuji@gentei.org

Japanese Document follows:

【reviveとは】

	  revive.el を使うと、Emacs 使用時の状態をファイルにセーブして、
	次回 Emacs を起動する時にその状態に復帰することができます。もち
	ろんウィンドウの分割状態も復元されるので saveconf や desktop で
	いらいらしていた人にもお勧めです。

【組み込み方】

	  revive.el を load-path の通ったディレクトリに入れ、~/.emacs に
	以下の記述を入れてください。

	  (autoload 'save-current-configuration "revive" "Save status" t)
	  (autoload 'resume "revive" "Resume Emacs" t)
	  (autoload 'wipe "revive" "Wipe emacs" t)

	そして上の関数を好きなキーに割り当ててください。以下は例です。

	  (define-key ctl-x-map "S" 'save-current-configuration)
	  (define-key ctl-x-map "F" 'resume)
	  (define-key ctl-x-map "K" 'wipe)

【使い方】

	  上の define-key をした場合には、C-x S で現在の編集状態をセーブ
	することができます。save-current-configuration 関数に数引数をつ
	けると複数の状態を別々にセーブできます。「C-u 2 C-x S」とすると2
	番のバッファに現状をセーブできます。これをロードする時も同様に
	「C-u 2 C-x F」とタイプすると2番のバッファから状態をロードします。

【変数のセーブ】

	  変数の値もセーブしておくことができます。デフォルトでセーブする
	global 変数は revive:save-variables-global-default に、local 変
	数は revive:save-variables-local-default に定義されています。ほ
	かの変数も保存したい場合は、revive:save-variables-global-private
	に global 変数名を、revive:save-variables-local-private に local
	変数名をそれぞれリストの形で定義しておきます。例えば gmhist を使っ
	ている場合には、

		(setq revive:save-variables-global-private
		      '(file-history buffer-history minibuffer-history))

	などと ~/.emacs に書いておくと快適でしょう。

【普通でないバッファの扱い】

	  mh-rmail ではカレントバッファが mh-folder-mode, gnus ではカレ
	ントバッファが gnus-Group-mode になります。この対応関係は、変数
	revive:major-mode-command-alist-default に書かれています。この変
	数に登録されている以外のものを定義したい場合は、

		(setq revive:major-mode-command-alist-private
		  '((hogehoge-mode	. hoge)
		    (herohero-mode	. herohero)
		    ("タイプ＆メニュー"	. trr)))

	のように revive:major-mode-command-alist-private の値を設定する
	と次回 resume した時に自動的に対応するコマンドが起動されます。ま
	た上の例にあるように、major-mode(シンボル)の代わりに buffer-name
	(文字列)を指定することもできます。

	  また、SKKの辞書のようにリジュームするとうまく動かなくなってし
	まうバッファがある場合は、変数 revive:ignore-buffer-pattern にそ
	のバッファ名がマッチするような正規表現を設定してください。

【プログラムから使う】

	  英語版ドキュメント [For programmers] の項を参照してください。

【あとがき】

	  最初は resume というファイル名だったのですが、Emacs 19 のディ
	レクトリに resume.el というファイルがあってショックを受けました。
	こちらはコマンドラインで何回 emacs と打っても、既に起動している
	emacs にファイルを渡すというだけの(ピーー)プログラムで「どこが
	resume やねん」と言いたくなりましたが我慢して revive.el にリネー
	ムしました。ああまったく、saveconf でも desktop でもなし得なかっ
	たウィンドウ分割状態の復元をサポートしたと言うのに…、なんてこと
	は英語版には書けないな:-)。

【取り扱い】

	  このプログラムは、フリーソフトウェアといたします。このプログラ
	ムを使用して生じたいかなる結果に対しても作者は一切の責任を負わな
	いものといたしますが、コメントやバグレポートは大いに歓迎いたしま
	す。お気軽にご連絡下さい。連絡は以下のアドレスまでお願いいたしま
	す(2012/8現在)。
							yuuji@gentei.org
