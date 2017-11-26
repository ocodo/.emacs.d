;;; speed-type-keyboard-drills --- keyboard drills (for use with speed type)
;;; Package-Requires: ((emacs "24.0") (speed-type "0.1") (hydra "0.1"))
;;; Commentary:
;;; Code:
(require 'speed-type)

(defvar speed-type-keyboard-drill-start-lessons-error
  "No active lesson / exercise for Speed Type Keyboard Drill, use M-x speed-type-keyboard-drill instead"
  "Error message used when no lessons have been attempted.")

(defvar speed-type-keyboard-drill-message-prefix "Speed Type - Keyboard Drill"
  "Standard message prefix.")

(defvar speed-type-keyboard-drill-last-lesson-attempted nil
  "The last keyboard drill lesson attempted.")

(defvar speed-type-keyboard-drill-last-exercise-attempted nil
  "The last keyboard drill exercise attempted.")

(defvar speed-type-keyboard-drill-lessons-list
  ;; A collection of QWERTY keyboard drills
  '((:lesson
     "Home Row"
     :exercises
     ("aaa ;;; sss lll ddd kkk fff jjj"
      "aa ss dd ff aa ss dd ff"
      ";; ll kk jj ;; ll kk jj"
      "ad ad as as ask ask ad ad as as ask ask"
      ";;; lll kkk jjj ;;; lll kkk jjj"
      "asdf ;lkj asdf ;lkj asdf ;lkj"
      ";;aallsskkddjjff ffjjddkkssllaa;;"
      "add add fad fad jak jak sad sad fall fall jak jak"
      "add fad; add fad; add jak; add jak; sad fall; sad fall;"
      "a a as as fad fad dad dad ;; ;; ja ja ka ka la la"
      "jas jas kas kas las las jas jas kas kas las las jas jas"
      "jf kd ls ;a jf kd ls ;a jf kd ls ;a jf kd ls ;a"
      "fall fall sad sad all all all sad falls; all sad falls;"
      "lad lad asks asks sas sas kass kass"
      "lad asks sas kass; lad asks sas kass;"
      "a a sad sad dad dad fall fall; a sad dad fall;"
      "dad dad sad sad kad kad lad lad"
      "dad dad; sad sad; kad kad; lad lad;"
      "lad; dad; sad; lass; lad; dad; sad; lass;"
      "fad fad; ads ads; all all; fad ads all lads;"
      "ask a lad; a fall ad; ask a dad;"
      "as a lad; as a dad; as a sad lass;"))

    (:lesson
     "Malabar Beach Drills"
     :exercises (".z nv mc ,x jf nv ie ow pq ;a a; qp wo ei vn fj x, cm vn z."
                 "wp wp ri ri fe ol se ol fe ri ri wp wp"
                 "nv mc ,x .z z. x, cm vn"
                 "pqwerytuiop pqwerytuiop pqwerytuiop pqweruiop pqweruiop pqweruiop pqwerytuiop pqwerytuiop pqwerytuiop"
                 "bbvvvcccxxxzzz mmmnnn nnnmmm bbvvcxz bvvvcxz bvvvcxz zxcvvvb zxcvvvb zxcvvbb mmmnnn nnnmmm zzzxxxcccvvvbb"))
    (:lesson
     "H and E"
     :exercises
     ("a;sldkfj a;sldkfj aa ;; ss ll dd kk ff jj"
      "as ad all jak fad fall lass as ad all jak fad fall lass"
      "a lad; a dad; ask a dad; ask a lass; a fall fad;"
      "jjj hhh jjj hhh jhj jhj jhj jhj aaa hhh aaa hhh ah ah ha ha"
      "jhj hjh jhj hjh aha hah aha hah had had has as ash ash"
      "ah ah ha ha; had had has ash; has had a hall; as had a hall;"
      "ddd eee ddd eee ded ded ded ded el el led led eel eel eke eke"
      "ede ede ede ede lee lee lee lee fed fed fed fed eke eke eke eke"
      "a lake; a lake; a leek; a leek; a jade; a jade; a desk a desk;"
      "he he she she shed shed heed heed held held he she shed heed held"
      "he held a lash; she held a jade; he she held sash;"
      "he has fled; he has a sale; she has a sale; he as ash;"
      "ask ask has has lad lad all all fall falls"
      "a sash; had all; a fall jak; a lad sash;"
      "he he she she led led held held she she fell fell"
      "he he led led; she she had had; she she fell fell;"
      "a jade shelf; a jade desk shelf;; she had a shed;"
      "he sells desks; she sells desks; he sells jade;"
      "he led; she led; he as jade; she has jade;"
      "she asked a lad; he asked a lass; she fell; he fell;"))

    (:lesson
     "I and R"
     :exercises
     ("a;sldkfj fjdksla; a;sldkfj fjdksla; dad lad fad ask"
      "jh jh jh jh hj hj hj hj hah hah has has had had sash sash"
      "ed ed led led fled fled sled sled ed led fled sled"
      "k k k k ik ik ik ik is is is is if if if if did did did did"
      "aid aid aid aid kid kid kid kid hail hail hail hail"
      "if if if if is is is is kid kid kid kid his his his his"
      "a kid; a lie; he did aide his lie; if a kid lied;"
      "fff rrr fff rrr fff rrr fff rrr frf frf frf frf rfr rfr rfr rfr"
      "jar jar red red her her far far red jar her fare"
      "rake lake lark jar hear her dark red air"
      "rake rake lake lake lark lark jar jar hear hear her her"
      "dark dark red red air air sir sir fir fir fire fire"
      "he is; she is; if her; if his; her aide; his aide;"
      "he he; if if; is is; ed ed; as as;"
      "she she; her her; hah hah; red red; air air; hair hair;"
      "shed shed; rake rake; hear hear; fare fare; lark lark;"
      "he if is ed as; she her hah red air;"
      "hair shed; rake hear; fare lark;"
      "if he is did fir jak all fall jak did he is if"
      "a red jak; he ask her; she ask her aide; if she fell;"
      "she did; he did; he led her; she is her aide;"
      "she had a red jar; she had a fire sale;"))

    (:lesson
     "Review 1"
     :exercises
     ("he he has has fir fir; she she had had jak jak;"
      "he had a jade jar; she had a jade jar;"
      "she had a leek sale; she had a leek sale;"
      "if is ha lie rid die did sir fir lie led jar"
      "ad has lad kid rah jak led her ask lid had"
      "fall jeer hah all keel fire hall add iris"
      "if he is she if as is if is she if is he"
      "she did; a jak; he led; she has; he is; if he;"
      "a lid jar; all her ads; a red fish; a jade ad;"
      "if he;"
      "as if she;"
      "he had a desk;"
      "she had a red jar;"
      "he had a lead all fall;"
      "she asked if he reads ads;"
      "she sees all adds she reads;"
      "her dad had a fall sales ad in red;"
      "is is if if ah ah he he el el ad ad ha ha"
      "if if fir fir did did eel eel jak jak are are"
      "eel eel jak jak ire ire are are aid aid red red"))

    (:lesson
     "O and T"
     :exercises
     ("had a fall; as a fall add; a sad dad fall; a dad dad;"
      "he sees her aid; he irks her; a red fish; a red aid;"
      "he sells red desks; she had a jar fall; he sells desks;"
      "lll ooo lll ooo ooo lll ooo lll lol lol lol lol"
      "ol ol ol ol of of of of or or or or for for for for"
      "or or or or for for for for oak oak oak oak off off off off"
      "do so; do so; of old; of old; of oak of oak; of old red oak;"
      "fff ttt fff ttt ttt fff ttt fff ftf ftf ftf ftf"
      "it it it it at at at at tie tie tie tie the the the the fit fit fit fit"
      "tf tf tf tf fit fit fit fit sit sit hit hit kit kit tie tie"
      "too too toe toe hot hot lot lot too too toe toe hot hot lot lot"
      "too hot; odd fort; odd lot; jot a lot;"
      "to rot; the lot; for this; dot it; for the lot;"
      "sit kid jak kit old hit led is kid dot jak sit"
      "he led she led had see held fled has jade leads"
      "he asked her to led the fled fit tie sit kit"
      "it fit tie sit kit its fits hits kits lit hit tie it"
      "a fit is kit sits it fits a dit a tie sits hits fits"
      "ooo rrr ooo rrr or or or or for for for for ore ore ore ore"
      "he rode; a rod; a door; a rose; a rod; a rod door; a rose door;"
      "or it do to of odd too for she the too"
      "of all she is or the to do if he do so it is it if"
      "if she asked a lad; the lad off to the lake;"
      "he or she; off the old jet; she left the jet; a red salad;"))

    (:lesson
     "N and G"
     :exercises
     ("has a lad; a fall lad fad; has a red salad;"
      "do it; do it; a tot; a tot; do a lot; do a lot; it is hot; it is hot;"
      "to do; do to; a tot; tot a; do a lot; lot a do; dot it; it dot;"
      "her skis; her kid; his aide; it is far; is a kid;"
      "jjj nnn jjj nnn jjj nnn jjj nnn jnj jnj jnj jnj"
      "nj nj nj nj jn jn jn jn an an an an and and and and"
      "end end ant ant land land hand hand fan fan"
      "fff ggg fff ggg fff ggg fff ggg fgf fgf fgf fgf"
      "fg fg fg fg gf gf gf gf nj nj nj nj jn jn jn jn"
      "he got; to the fog; he got to jog; he got to golf;"
      "no nag ago gone long go on; a nag; no gain; long ago;"
      "log in so soon; a fine sing; led a hand; no gain;"
      "he got an old dog and she is gone;"
      "she jogs in the fog;"
      "she and he jog in the dense fog;"
      "she and he go to golf in dense fog;"
      "she and he golf a nine in the north area"
      "he has an oar; he jogs; do a log for her; she left a red jar;"
      "an an go go in in dig dig end end and and go go to to"
      "he did it for her; he is to take the old jet;"))

    (:lesson
     "Left Shift and Period"
     :exercises
     ("aa ;; ss ll dd kk ff jj gg hh ff jj dd kk ss ll aa ;;"
      "hhh eee iii rrr ooo ttt nnn ggg hh ee ii rr oo tt nn gg"
      "is he; an if; do or; go to; an oak; all of;"
      "J J J J Ja Ja Ja Ja K K K K Ka Ka Ka Ka L L L L La La La La"
      "Ha Ha Ha Ha Hal Hal Hal Hal Ja Ja Ja Ja Jan Jan Jan Jan"
      "Kal did it; Hans is red; Jan at the fig;"
      "I I I I Ia Ia Ia Ia Ian Ian Ian Ian"
      "lll … lll … lll … lll … l.l l.l l.l l.l fl. Ed. Ft. hr. in."
      "I do too. Ian is not. Ola did it. Jan does this. Kent is gone."
      "Hal did it. I shall get on a train and do this."
      "J. L. Hanes skis on the lake. Larry also does it."
      "I said ft. is for feet and rd. is for road and fl. is for floor."
      "Lt. John let L. K. take the old gong to the lake."
      "Larry asked the old store kid for a kit."
      "I like Ike and John."
      "Ike said to take the old road to get to the lake."
      "if so it is to do it do to is he go do if it is to so if"
      "Lars sent the log to the ski lodge."
      "do is the got if the for the ask the end"
      "O. J. lost one of the logs off the train."
      "Ja Ja Ka Ka La La Ia Ia Oa Oa Ja Ja Ka Ka La Ia Ia Oa Oa"))

    (:lesson
     "Review 2"
     :exercises
     ("J. L. O. I. ik rf ol ed nj gf hj tf .l ft. i. e. e.g."
      "is or to a go is he and got the for led kit"
      "I got the ski. Jan led Nan. Hal is gone."
      "hj hj hj hj jh jh jh jh ed ed ed ed de de de de she she led led"
      "Heidi had a lead the end of the set one."
      "sir sir ire ire ore ore his his risk risk fire fire ride ride"
      "Kate is at high risk if he rides the horse."
      "so if of too of hot old the toe so if of too of hot old toe"
      "Nan took the list to the food store."
      "nj nj nj nj hj hj hj hj go go go go an an an an got got got got"
      "Lane sings nine songs."
      "J. K. is going to Idaho to sing the songs."
      "Nan took the train at nine."
      "Janet asked to take the ski to the lake."
      "Karl and Janet said he left the lake at noon."
      "Jan needs the data sheet too."
      "I felt ill just as the ski lift started up the hill."))

    (:lesson
     "U and C"
     :exercises
     ("fjdksla; fjdksla; a;sldkfj a;sldkfj he he he he ir ir ir ir"
      "ot ot o tot ng ng ng ng L. L. L. L. he ir ot ng L"
      "do so to go fan hen log tan son not sign and fan tan"
      "Olga and Jena has the first skate slot."
      "jjj uuu jjj uuu jjj uuu jjj uuu uj uj uj uj ju ju ju ju us us us us"
      "jug jug jug jug sue sue sue sue lug lug lug lug use use use use"
      "due for us; the red fur; use it again; is it hers;"
      "ddd ccc ddd ccc ddd ccc ddd ccc dcd dcd dcd dcd cod cod cod cod"
      "cog cog cog tic tic tic can can can cot cot cot hot hot hot"
      "ice ice can can doc doc tic tic dock dock cue cue cut cut"
      "cues cues duck duck clue clue coat coat cut cut cake cake"
      "I found a fur coat that had cake on it."))

    (:lesson
     "W and Right Shift"
     :exercises
     ("aaa ;;; sss lll ddd kkk fff jjj he ir ot ng l. uc he ir ot ng l."
      "us us cut cut sue sue cot cot nut nut lug lug ice ice can can"
      "Janet took the lead in the race for the record."
      "sss www sss www sss www sws sws sws sow sow sow sow"
      "wow wow wow lows lows lows lows shows how shows how"
      "cows cows now now row row own own tows tows"
      "how is; so low; to sow; to own; row now; go now;"
      "aaa A A A aaa A A A ccc C C C ccc C C C fff F F F ddd D D D"
      "Dan Del Dennis Fran Frank Francis Sue Stan Scott"
      "Rio Rip Run Willa Will Wanda Alan Ali Ariel Cat Chris Curtis"
      "Wil left the show which he won the award"
      "Frank will go to Rio next fall."))

    (:lesson
     "B and Y"
     :exercises
     ("fff bbb fff bbb fff bbb fff bbb fib rib rob but big but bad fibs"
      "bbb fff bbb fff bbb fff bbb fff bid bid bud bud rob rob lob lob"
      "rib fib rub orb rob but bid a rib; to fib; it rubs; or rob; she bid;"
      "jjj yyy jjj yyy jjj yyy jjj yyy jyj jyj jyj jyj jy jy jy jy yj yj yj yj"
      "jay jay lay lay day day say say hay hay stay stay"
      "yj yj yj yj jay jay eye eye dye dye yes yes yet yet"
      "to say; jay eye; dye the eye; a dyed jay eye;"
      "by and by; buy it; a byte; the boy, the buoy;"
      "Jay and Janet went to the store to buy bread."
      "hhh eee iii rrr ooo ttt nnn ggg L J K … uuu ccc www R F W bbb yyy"
      "he he it it rot rot now now go go us us wow wow by by"
      "no in bow any cut ran bin cow deck any rain wow sub"
      "See the baby cub that Cody went to buy."
      "Buy the big baby a sub sandwich."
      "for the hand such did cut now held furl eight ask now"
      "to do to buy can go for all did ask her to buy bow wow"
      "if she had to work and such for this she held the goal"
      "The auburn hair flew in the light wind."
      "Bobby left the show and went to the city."))

    (:lesson
     "Review 3"
     :exercises
     ("tf bf cd uj .l nj gf tf ol rf ik ed hj tf bf cd uj .l nj fg tf ol"
      "by by jay jay lay lay hay hay say say buy buy boy boy yet yet"
      "Fran and Jan knew about the red and gold truck."
      "clay when they than won sly den win den send tend won one"
      "in a way | she was fine | to buy a hen | they will win"
      "John can bid on the old clock he saw at the station."
      "I know she will be here soon."
      "Lt. John Sorensen and Nan went to Sol for the day."
      "Jana and Rod are in Sweden; Jane and Bo go in June."
      "Cody and Janet went to the lake to see the boat."
      "Work for us in Los Angeles then again in San Francisco."
      "Jana, Aida and Ross went off to the club to golf."
      "she owns dug buys cog job for yet ask led head red"
      "of if | the all of | and do |cut it | he go | to do it | do it too"
      "Lake Tahoe had a big ice show today."
      "Jack said to leave this job and find a new one."
      "Cindy left for work one hour ago."
      "Rudy and Judy have good jobs in town."
      "Allan and Allen and Jon and John and Peg and Peggy"
      "John and Jan are going to work then to the sale."
      "Row the boat to the dock."
      "Carl has an old kayak and a canoe."
      "Stan told us to set goals and go for it."
      "She said for us to get going on our goals."
      "It is our job to go as high as we can go."
      "The big signs close to the area will go to Jake"
      "The big boat dock has a lot of birds on it."
      "All left the club as the news ended."
      "Jeff left the club as the news started."
      "The goal was to lower cost and the task force did it."
      "Larry likes new stuff so this was not too rough."
      "I hike each day on the road side near the school."))

    (:lesson
     "M and X"
     :exercises
     ("ow bf if rf us by ik tf ik nj nj yj ik tf hj cd uj gf by us"
      "bet you but bit dye fib yes by bye boy buy dye yes yet"
      "I can win the gold but I must set a higher goal."
      "jjj mmm jjj mmm jmj jmj jmj jmj am am am am me me me me"
      "mj mj mj mj jm jm jm jm me me me me may may may may"
      "yam yam yam yam men men men men jam jam jam jam"
      "sss xxx sss xxx sss xxx sss xxx sxs sxs sxs sxs sxs sx sx sx"
      "ox ox ox ox fox fox fox fox fix fix fix fix nix nix nix nix"
      "a fox; an ox; nix it; fix it; hit an ax; by six; by fixing it’"
      "me ox am ax jam ham hem men lax fox mix lox"
      "to lax; fix it; mix it; six men to fix; six men to hex;"
      "Mix a ham salad for six; Max can fix tea; Max can fix ham for six;"))

    (:lesson
     "P and V"
     :exercises
     ("no cad him bet no in we my be on ad on my ax bet him"
      "just is | of work | to sign | of lace | to flex | a form | sign it"
      "He won a medal at the show for sixth."
      ";;; ppp ;;; ppp ;;; ppp ;;; ppp ;p; ;p; ;p; ;p; pen pen lap lap"
      "pa pa pa pa pan pan pan pan nap nap nap nap paw paw paw paw"
      "a pen and cap; pay then pick it up; plan to keep a promise;"
      "fff vvv fff vvv fff vvv have have have have five five five five"
      "vf vf vf vf vie vie vie vie van van van van view view view view"
      "dive dive dive dive go via; vie for; has vim; a view; to live;"
      "cup up vie pen van cap vim rap have keep plan live life"
      "gave it up; pave it; save it; very apartment; give me a cup;"
      "Vic plans to have the van pick us up at five or six."))

    (:lesson
     "Q and Comma"
     :exercises
     ("ask jam for own buy dig via fix all do dig"
      "a map; an apt.; her plans; have five;"
      "Sixty pints of jam will be sent to her."
      "aaa qqq aaa qqq aaa qqq aaa qqq qt. qt. qt. qt."
      "quad quad quad quad quit quit quit quit aqua aqua aqua aqua"
      "quite quite quite quite squad squad squad squad"
      "kkk ,,, kkk ,,, kkk ,,, kkk ,,, kit, kit, kit, kit, kite, kite, kite, kite"
      "a kit, a kite, a bike, a hike, a kit, a kite, a bike, a hike"
      "I see that Pam and Ike are here but Stan will be late."
      "Enter the words quote, quite, quo, aqua, and quit."
      "I think I will quit the squad along with Raquel."
      "We were quite quick to quit the movie."))

    (:lesson
     "Paragraph 1"
     :exercises
     ("When you strike the enter key at the end of a line it is
called a hard return. If you just continue to key without
striking the enter key the computer will automatically go to the
next line. This is called a soft return."))

    (:lesson
     "Paragraph 2"
     :exercises
     ("There is another phrase for soft return. It is called
wordwrap. It is much easier to use and saves you time and
effort. Strike the enter key at the end of each paragraph."))

    (:lesson
     "Paragraph 3"
     :exercises
     ("Always use good form when keying. It will help build speed
and accuracy. Always look at what you are keying instead of the
keyboard. Keep your wrists flat and fingers curved over the home
keys."))

    (:lesson
     "Paragraph 4"
     :exercises
     ("Bouncing hands will cause more errors and slows the typist
down. Stay in control and be firm when reaching for your keys."))

    (:lesson
     "Review 4"
     :exercises
     ("Virgil and Jack found the mosque at six."
      "Pam, Quinn, Carl, Van and Hope will be here at five."
      "Vic and Vim will aid the girl with her sign work."
      "I will fix the sign and charge them for it."
      "Jay, Jim, Julie, Janice will sit in the blue auto."
      "Todd will fish the docks for a big fish."
      "I want a big bowl of salad and a cup of chili."
      "Cal and Carl just won a big prize at the fair."
      "so so an an if if is is us us am am by by or or ox ox"
      "is she in | pay in advance | if he may | in a firm"
      "I will keep pens by my desk in a tan and yellow tray."
      "I will give the new toy to the little boy down the street."
      "J. V. M. S. Dr. or Mrs., Ph.D. or Ed.D., July, August, September"
      "Mrs. Mr. Miss Mrs. Marnie Fowers, Dr. Mark V. Jensen, Mr. T. Ott"
      "B. J. Smith has a birthday in March. Go to the party with her."
      "Mary has a Ph.D. from N.Y.C.; Dolly will get a Ed.D."
      "jam for pay got the lap ox cut run ran jam ham pan"
      "make them move when both then their there that the"
      "to sit | by me | by six | old oak | did go |for the air"
      "to tie | I own | pay him | cut ties | for they"
      "I may have six students who will do the job for free."
      "Vicky and John will pack a box lunch for the fair."
      "I plan to bike for five days with John."
      "You will find the best price for the tent at Big K."
      "qa qa qa qa bf bf bf bf by by by by qt. qt. qt. qt. ft. ft. ft. ft."
      "John is the brainy one and Jess is the brawny one."
      "sx sx sx sx sw sw sw sw dc dc dc dc de de de de fv fv fv fv fr fr fr fr"
      "I keyed ox, mix, fox, fix, nix, six, and tricks."
      ";p ;p ;p ;p lo lo lo lo ki ki ki ki ju ju ju ju"
      "jn jn jn jn jm jm jm jm k, k, k, k, l. l. l. l."
      "jy jy jy jy jh jh jh jh fg fg fg fg ft ft ft ft fb fb fb fb"
      "fv fv fv fr fr fr de de de dc dc dc sw sw sw sx sx sx aq aq aq" ))

    (:lesson
     "Z and Colon (:)"
     :exercises
     ("John won a diving trophy for six events at our state meet."
      "to busy | down town | by the city | with us"
      "I have to go into town to sign a document with my mom."
      "aaa zzz aaa zzz aaa zzz aaa zzz aza aza aza aza zap zap zap zap"
      "zoo zoo zoo zoo zip zip zip zip zag zag zag zag oz. oz. oz. oz."
      "zoo zoo zoo zoo zip zip zip zip maze maze maze maze"
      "eight oz. twelve oz. twenty oz. fifteen oz. nineteen oz."
      ";;; ::: ;;; ::: ;;; ::: To: File: Shift: Reply to: Dear Alan:"
      "Leave two spaces after typing a colon."
      "Shift in opposition when keying capital letters at the beginning of names."
      "John, Jim, Larry, Ted, Janice, Steve, Paul, Tawna"
      "Zoe, Zen, Liza, Quigg, Paul, Olga, Ivan, Yoda"
      "You must shift with the left hand to get the colon to work."
      "hazy quad quit zone quay zeal quote zap qt. zoo Zen Zelda"
      "Joey amazed us all when he won the state math contest."
      "apt six fix flex flax next harp rip open drop the pen"
      "Lex and Lars are twin boys who like to fix apple pie for six friends."
      "mime mime mime mime mama mama move move move move"
      "vamp vamp vamp vamp dive dive dive dive five five five five"
      "Glena made the dog work extra hard before putting him in the kennel."
      "Six of the firms had to pay a large fee to the state."
      "See the quick red fox jump over the lazy tan dog."
      "Give me the six big tips to help me with my history quiz."
      "fix it mix it key it tie it hit it bonk it drop it"
      "Key the following: Oxen, exit, axle, sixty, and sixth."
      "qa qa qa qa aq aq aq aq ki ki ki ki ik ik ik ik qt. qt. qt. qt."
      "Key the following: Quit, aqua, equal, quiet, and quick."
      "Lazy Lucy liked to sleep during her first class of the day."
      "p: qa ;p aq zap zap zip zip size size lazy lazy"
      "Put hot peppers in the zany zesty salsa and put it on his pizza."
      "Magic Marvin and marvelous Mavis vowed to move faster and with more vim and vigor.")))

  "Collection of QWERTY keyboard drills.")

(defun speed-type-keyboard-drill-close-buffers ()
  "Close any speed-type buffers."
  (interactive)
  (--each
      (--filter
       (string-match-p "^speed-type\\(<[0-9]+>\\)?$" (buffer-name it))
       (buffer-list))
    (kill-buffer it)))

(defun speed-type-keyboard-drill-get-exercise-index (lesson exercise)
  "Get the index for the given LESSON EXERCISE."
  (--find-index
   (string= exercise it)
   (speed-type-keyboard-drill-get-exercises-for lesson)))

(defun speed-type-keyboard-drill-get-lesson-index (lesson)
  "Get the index for the given LESSON."
  (--find-index
   (string= lesson it)
   (speed-type-keyboard-drill-get-lessons)))

(defun speed-type-keyboard-drill-get-exercises-for (lesson)
  "Return the exercises for a LESSON."
  (cl-loop for lesson-set in speed-type-keyboard-drill-lessons-list
           if (string= lesson (plist-get lesson-set :lesson))
           return (plist-get lesson-set :exercises)))

(defun speed-type-keyboard-drill-get-lessons ()
  "Return the lesson titles."
  (mapcar
   (lambda (lesson)
     (plist-get lesson :lesson))
   speed-type-keyboard-drill-lessons-list))

(defun speed-type-keyboard-drill-number-of-lessons ()
  "Return the number of drill lessons."
  (length (speed-type-keyboard-drill-get-lessons)))

(defun speed-type-keyboard-drill-number-of-exercises-for (lesson)
  "Return the number of drill exercises for LESSON."
  (length (speed-type-keyboard-drill-get-exercises-for lesson)))

(defun speed-type-keyboard-drill-has-last-attempted-p ()
  "True if the lesson and exercise attempted variables are non-nil."
  (and speed-type-keyboard-drill-last-lesson-attempted
       speed-type-keyboard-drill-last-exercise-attempted))

(defun speed-type-keyboard-drill-has-next-lesson-p ()
  "True if there's a drill lesson available after the last attempted one."
  (let ((lesson-index (speed-type-keyboard-drill-get-lesson-index speed-type-keyboard-drill-last-lesson-attempted))
        (lesson-total (speed-type-keyboard-drill-number-of-lessons)))
    (> (1- lesson-total) lesson-index)))

(defun speed-type-keyboard-drill-has-next-exercise-p ()
  "True if there's a drill exercise available after the last attempted one."
  (let ((exercise-total (speed-type-keyboard-drill-number-of-exercises-for speed-type-keyboard-drill-last-lesson-attempted))
        (exercise-index (speed-type-keyboard-drill-get-exercise-index speed-type-keyboard-drill-last-lesson-attempted speed-type-keyboard-drill-last-exercise-attempted)))
    (> (1- exercise-total) exercise-index)))

(defun speed-type-keyboard-drill-get-next-exercise ()
  "Get the next exercise and return t or nil."
  (when (speed-type-keyboard-drill-has-next-exercise-p)
    (nth (1+ (speed-type-keyboard-drill-get-exercise-index speed-type-keyboard-drill-last-lesson-attempted speed-type-keyboard-drill-last-exercise-attempted))
         (speed-type-keyboard-drill-get-exercises-for speed-type-keyboard-drill-last-lesson-attempted))))

(defun speed-type-keyboard-drill-get-next-lesson ()
  "Get the next lesson and return t or nil."
  (when (speed-type-keyboard-drill-has-next-lesson-p)
    (nth (1+ (speed-type-keyboard-drill-get-lesson-index speed-type-keyboard-drill-last-lesson-attempted))
         (speed-type-keyboard-drill-get-lessons))))

(defun speed-type-keyboard-drill-next-lesson ()
  "Speed type the next lesson starting at the first exercise."
  (interactive)
  (if (speed-type-keyboard-drill-has-last-attempted-p)
      (if (speed-type-keyboard-drill-has-next-lesson-p)
          (let* ((lesson (speed-type-keyboard-drill-get-next-lesson))
                 (exercise (nth 0 (speed-type-keyboard-drill-get-exercises-for lesson))))
            (setq speed-type-keyboard-drill-last-lesson-attempted lesson
                  speed-type-keyboard-drill-last-exercise-attempted exercise)
            (speed-type-keyboard-drill-close-buffers)
            (speed-type--setup exercise))
        (message "%s - All Lessons finished." speed-type-keyboard-drill-message-prefix))
    (error speed-type-keyboard-drill-start-lessons-error)))

(defun speed-type-keyboard-drill-next-exercise ()
  "Speed type the next exercise for the current lesson."
  (interactive)
  (if (speed-type-keyboard-drill-has-last-attempted-p)
      (if (speed-type-keyboard-drill-has-next-exercise-p)
          (let* ((exercise (speed-type-keyboard-drill-get-next-exercise)))
            (setq speed-type-keyboard-drill-last-exercise-attempted exercise)
            (speed-type-keyboard-drill-close-buffers)
            (speed-type--setup exercise))
        (message "%s - Lesson [%s] finished" speed-type-keyboard-drill-message-prefix speed-type-keyboard-drill-last-lesson-attempted))
    (error speed-type-keyboard-drill-start-lessons-error)))

(defun speed-type-keyboard-drill-repeat-exercise ()
  "Repeat the current speed-type exercise."
  (interactive)
  (if (speed-type-keyboard-drill-has-last-attempted-p)
      (let ((exercise speed-type-keyboard-drill-last-exercise-attempted))
        (speed-type-keyboard-drill-close-buffers)
        (speed-type--setup exercise))
    (error speed-type-keyboard-drill-start-lessons-error)))

(defun speed-type-keyboard-drill-quit ()
  "Quit Speed type, and close all it's buffers."
  (interactive)
  (speed-type-keyboard-drill-close-buffers))

;;;###autoload
(defun speed-type-keyboard-drill-select ()
  "Speed type a keyboard drill from the collection."
  (interactive)
  (speed-type-keyboard-drill-close-buffers)
  (let* ((lesson (completing-read
                  "Select a keyboard lesson -> "
                  (speed-type-keyboard-drill-get-lessons)))
         (exercise (completing-read
                    "Select an exercise -> "
                    (speed-type-keyboard-drill-get-exercises-for lesson))))
    (setq
     speed-type-keyboard-drill-last-lesson-attempted lesson
     speed-type-keyboard-drill-last-exercise-attempted exercise)
    (speed-type--setup exercise)))

;;; Need to define oxford-dictionary-api-headers:
;;; (setq oxford-dictionary-api-headers
;;;       '(("app_key" . "key string")
;;;         ("app_id" . "id string")))

(defun speed-type-oxford-dictionary-definition (word-to-define)
  "Launch speed-type using a WORD-TO-DEFINE definition from the oxford-dictionary api."
  (interactive "SWord: ")
  (unless oxford-dictionary-api-headers (message "oxford-dictionary-api-headers must be defined"))
  (when oxford-dictionary-api-headers
    (request
     (format
      "https://od-api.oxforddictionaries.com/api/v1/entries/en/%s" word-to-define)
     :headers oxford-dictionary-api-headers
     :parser 'json-read
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (let* ((buf (generate-new-buffer "*definitions*"))
                        (word (json-pointer-get
                               data
                               "/results/0/id"))
                        (entries
                         (json-pointer-get
                          data
                          "/results/0/lexicalEntries/0/entries"))
                        (definitions (cl-map 'vector (lambda (entry)
                                                        (json-pointer-get
                                                         entry
                                                         "/senses/0/definitions/0"))
                                              entries)))
                   (with-current-buffer buf
                     (auto-fill-mode 1)
                     (insert word "\n\n")
                     (insert (string-join definitions "\n"))
                     (speed-type-buffer buf))))))))

(defun speed-type-keyboard-drill-repeat-exercise ()
  "Repeat the current exercise."
  (interactive)
  (if (speed-type-keyboard-drill-has-last-attempted-p)
      (progn
        (speed-type-keyboard-drill-close-buffers)
        (speed-type--setup speed-type-keyboard-drill-last-exercise-attempted))
    (error speed-type-keyboard-drill-start-lessons-error)))

(defun speed-type-keyboard-drill-quit ()
  "Quit speed-type."
  (interactive)
  (speed-type-keyboard-drill-close-buffers))

(require 'hydra)

(defhydra speed-type-keyboard-drill-menu-hydra (:color amaranth)
  "Select an option from the menu."
  ("r" speed-type-keyboard-drill-repeat-exercise "Repeat this exercise" :color blue)
  ("n" speed-type-keyboard-drill-next-exercise "Next exercise" :color blue)
  ("l" speed-type-keyboard-drill-next-lesson "Next lesson" :color blue)
  ("s" speed-type-keyboard-drill-select "Select a new lesson" :color blue)
  ("d" speed-type-oxford-dictionary-definition "Speed-type a word definition from the oxford dictionary api" :color blue)
  ("q" speed-type-keyboard-drill-quit "Quit" :color blue))

(defadvice speed-type--handle-complete (after speed-type-keyboard-drill-show-hydra activate)
  "Show the menu hyda when a speed-type exercise is complete."
  (message "Speed Type exercise complete...")
  (speed-type-keyboard-drill-menu-hydra/body))

;; Local Variables:
;; nameless-current-name: "speed-type-keyboard-drill"
;; End:

(provide 'speed-type-keyboard-drills)
;;; speed-type-keyboard-drills.el ends here
