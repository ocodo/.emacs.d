;;; doctor.el --- psychological help for frustrated users

;; Copyright (C) 1985, 1987, 1994, 1996, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: games

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The single entry point `doctor', simulates a Rogerian analyst using
;; phrase-production techniques similar to the classic ELIZA demonstration
;; of pseudo-AI.

;;; Code:

(defvar **mad**)        (defvar *debug*)      (defvar *print-space*)
(defvar *print-upcase*) (defvar abuselst)     (defvar abusewords)
(defvar account)        (defvar afraidof)     (defvar arerelated)
(defvar areyou)         (defvar bak)          (defvar beclst)
(defvar bother)         (defvar bye)          (defvar canyou)
(defvar chatlst)        (defvar continue)     (defvar deathlst)
(defvar describe)       (defvar drnk)         (defvar drugs)
(defvar eliza-flag)     (defvar elizalst)     (defvar famlst)
(defvar feared)         (defvar fears)        (defvar feelings-about)
(defvar foullst)        (defvar found)        (defvar hello)
(defvar history)        (defvar howareyoulst) (defvar howdyflag)
(defvar huhlst)         (defvar ibelieve)     (defvar improve)
(defvar inter)          (defvar isee)         (defvar isrelated)
(defvar lincount)       (defvar longhuhlst)   (defvar lover)
(defvar machlst)        (defvar mathlst)      (defvar maybe)
(defvar moods)          (defvar neglst)       (defvar obj)
(defvar object)         (defvar owner)        (defvar please)
(defvar problems)       (defvar qlist)        (defvar random-adjective)
(defvar relation)       (defvar remlst)       (defvar repetitive-shortness)
(defvar replist)        (defvar rms-flag)     (defvar schoollst)
(defvar sent)           (defvar sexlst)       (defvar shortbeclst)
(defvar shortlst)       (defvar something)    (defvar sportslst)
(defvar stallmanlst)    (defvar states)       (defvar subj)
(defvar suicide-flag)   (defvar sure)         (defvar thing)
(defvar things)         (defvar thlst)        (defvar toklst)
(defvar typos)          (defvar verb)         (defvar want)
(defvar whatwhen)       (defvar whereoutp)    (defvar whysay)
(defvar whywant)        (defvar zippy-flag)   (defvar zippylst)

(defun doc// (x) x)

(defmacro doc$ (what)
  "quoted arg form of doctor-sock-$"
  (list 'doctor-sock-$ (list 'quote what)))

(defun doctor-sock-$ (what)
  "Return the car of a list, rotating the list each time"
  (let* ((vv (symbol-value what))
	(first (car vv))
	(ww (append (cdr vv) (list first))))
    (set what ww)
    first))

(defvar doctor-sock-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\n" 'doctor-sock-read-print)
    (define-key map "\r" 'doctor-sock-ret-or-read)
    map))

(define-derived-mode doctor-sock-mode text-mode "Doctor-Sock"
  "Major mode for running the Doctor (Eliza) program.
Like Text mode with Auto Fill mode
except that RET when point is after a newline, or LFD at any time,
reads the sentence before point, and prints the Doctor's answer."
  (make-doctor-sock-variables)
  (turn-on-auto-fill)
  (doctor-sock-type '(i am the psychotherapist \.
		 (doc$ please) (doc$ describe) your (doc$ problems) \.
		 each time you are finished talking, type \R\E\T twice \.))
  (insert "\n"))

(defun make-doctor-sock-variables ()
  (make-local-variable 'typos)
  (setq typos
	(mapcar (function (lambda (x)
			    (put (car x) 'doctor-sock-correction  (cadr x))
			    (put (cadr x) 'doctor-sock-expansion (car (cddr x)))
			    (car x)))
		'((theyll they\'ll (they will))
		  (theyre they\'re (they are))
		  (hes he\'s (he is))
		  (he7s he\'s (he is))
		  (im i\'m (you are))
		  (i7m i\'m (you are))
		  (isa is\ a (is a))
		  (thier their (their))
		  (dont don\'t (do not))
		  (don7t don\'t (do not))
		  (you7re you\'re (i am))
		  (you7ve you\'ve (i have))
		  (you7ll you\'ll (i will)))))
  (make-local-variable 'found)
  (setq found nil)
  (make-local-variable 'owner)
  (setq owner nil)
  (make-local-variable 'history)
  (setq history nil)
  (make-local-variable '*debug*)
  (setq *debug* nil)
  (make-local-variable 'inter)
  (setq inter
	'((well\,)
	  (hmmm \.\.\.\ so\,)
	  (so)
	  (\.\.\.and)
	  (then)))
  (make-local-variable 'continue)
  (setq continue
	'((continue)
	  (proceed)
	  (go on)
	  (keep going) ))
  (make-local-variable 'relation)
  (setq relation
	'((your relationship with)
	  (something you remember about)
	  (your feelings toward)
	  (some experiences you have had with)
	  (how you feel about)))
  (make-local-variable 'fears)
  (setq fears '( ((doc$ whysay) you are (doc$ afraidof) (doc// feared) \?)
		 (you seem terrified by (doc// feared) \.)
		 (when did you first feel (doc$ afraidof) (doc// feared) \?) ))
  (make-local-variable 'sure)
  (setq sure '((sure)(positive)(certain)(absolutely sure)))
  (make-local-variable 'afraidof)
  (setq afraidof '( (afraid of) (frightened by) (scared of) ))
  (make-local-variable 'areyou)
  (setq areyou '( (are you)(have you been)(have you been) ))
  (make-local-variable 'isrelated)
  (setq isrelated '( (has something to do with)(is related to)
		     (could be the reason for) (is caused by)(is because of)))
  (make-local-variable 'arerelated)
  (setq arerelated '((have something to do with)(are related to)
		     (could have caused)(could be the reason for) (are caused by)
		     (are because of)))
  (make-local-variable 'moods)
  (setq moods '( ((doc$ areyou)(doc// found) often \?)
		 (what causes you to be (doc// found) \?)
		 ((doc$ whysay) you are (doc// found) \?) ))
  (make-local-variable 'maybe)
  (setq maybe
	'((maybe)
	  (perhaps)
	  (possibly)))
  (make-local-variable 'whatwhen)
  (setq whatwhen
	'((what happened when)
	  (what would happen if)))
  (make-local-variable 'hello)
  (setq hello
	'((how do you do \?) (hello \.) (howdy!) (hello \.) (hi \.) (hi there \.)))
  (make-local-variable 'drnk)
  (setq drnk
	'((do you drink a lot of (doc// found) \?)
	  (do you get drunk often \?)
	  ((doc$ describe) your drinking habits \.) ))
  (make-local-variable 'drugs)
  (setq drugs '( (do you use (doc// found) often \?)((doc$ areyou)
						 addicted to (doc// found) \?)(do you realize that drugs can
						 be very harmful \?)((doc$ maybe) you should try to quit using (doc// found)
						 \.)))
  (make-local-variable 'whywant)
  (setq whywant '( ((doc$ whysay) (doc// subj) might (doc$ want) (doc// obj) \?)
		   (how does it feel to want \?)
		   (why should (doc// subj) get (doc// obj) \?)
		   (when did (doc// subj) first (doc$ want) (doc// obj) \?)
		   ((doc$ areyou) obsessed with (doc// obj) \?)
		   (why should i give (doc// obj) to (doc// subj) \?)
		   (have you ever gotten (doc// obj) \?) ))
  (make-local-variable 'canyou)
  (setq canyou '((of course i can \.)
		 (why should i \?)
		 (what makes you think i would even want to \?)
		 (i am the doctor\, i can do anything i damn please \.)
		 (not really\, it\'s not up to me \.)
		 (depends\, how important is it \?)
		 (i could\, but i don\'t think it would be a wise thing to do \.)
		 (can you \?)
		 (maybe i can\, maybe i can\'t \.\.\.)
		 (i don\'t think i should do that \.)))
  (make-local-variable 'want)
  (setq want '( (want) (desire) (wish) (want) (hope) ))
  (make-local-variable 'shortlst)
  (setq shortlst
	'((can you elaborate on that \?)
	  ((doc$ please) continue \.)
	  (go on\, don\'t be afraid \.)
	  (i need a little more detail please \.)
	  (you\'re being a bit brief\, (doc$ please) go into detail \.)
	  (can you be more explicit \?)
	  (and \?)
	  ((doc$ please) go into more detail \?)
	  (you aren\'t being very talkative today\!)
	  (is that all there is to it \?)
	  (why must you respond so briefly \?)))

  (make-local-variable 'famlst)
  (setq famlst
	'((tell me (doc$ something) about (doc// owner) family \.)
	  (you seem to dwell on (doc// owner) family \.)
	  ((doc$ areyou) hung up on (doc// owner) family \?)))
  (make-local-variable 'huhlst)
  (setq huhlst
	'(((doc$ whysay)(doc// sent) \?)
	  (is it because of (doc$ things) that you say (doc// sent) \?) ))
  (make-local-variable 'longhuhlst)
  (setq longhuhlst
	'(((doc$ whysay) that \?)
	  (i don\'t understand \.)
	  ((doc$ thlst))
	  ((doc$ areyou) (doc$ afraidof) that \?)))
  (make-local-variable 'feelings-about)
  (setq feelings-about
	'((feelings about)
	  (apprehensions toward)
	  (thoughts on)
	  (emotions toward)))
  (make-local-variable 'random-adjective)
  (setq random-adjective
	'((vivid)
	  (emotionally stimulating)
	  (exciting)
	  (boring)
	  (interesting)
	  (recent)
	  (random)   ;How can we omit this?
	  (unusual)
	  (shocking)
	  (embarrassing)))
  (make-local-variable 'whysay)
  (setq whysay
	'((why do you say)
	  (what makes you believe)
	  (are you sure that)
	  (do you really think)
	  (what makes you think) ))
  (make-local-variable 'isee)
  (setq isee
	'((i see \.\.\.)
	  (yes\,)
	  (i understand \.)
	  (oh \.) ))
  (make-local-variable 'please)
  (setq please
	'((please\,)
	  (i would appreciate it if you would)
	  (perhaps you could)
	  (please\,)
	  (would you please)
	  (why don\'t you)
	  (could you)))
  (make-local-variable 'bye)
  (setq bye
	'((my secretary will send you a bill \.)
	  (bye bye \.)
	  (see ya \.)
	  (ok\, talk to you some other time \.)
	  (talk to you later \.)
	  (ok\, have fun \.)
	  (ciao \.)))
  (make-local-variable 'something)
  (setq something
	'((something)
	  (more)
	  (how you feel)))
  (make-local-variable 'thing)
  (setq thing
	'((your life)
	  (your sex life)))
  (make-local-variable 'things)
  (setq things
	'((your plans)
	  (the people you hang around with)
	  (problems at school)
	  (any hobbies you have)
	  (hangups you have)
	  (your inhibitions)
	  (some problems in your childhood)
	  (some problems at home)))
  (make-local-variable 'describe)
  (setq describe
	'((describe)
	  (tell me about)
	  (talk about)
	  (discuss)
	  (tell me more about)
	  (elaborate on)))
  (make-local-variable 'ibelieve)
  (setq ibelieve
	'((i believe) (i think) (i have a feeling) (it seems to me that)
	  (it looks like)))
  (make-local-variable 'problems)
  (setq problems '( (problems)
		    (inhibitions)
		    (hangups)
		    (difficulties)
		    (anxieties)
		    (frustrations) ))
  (make-local-variable 'bother)
  (setq bother
	'((does it bother you that)
	  (are you annoyed that)
	  (did you ever regret)
	  (are you sorry)
	  (are you satisfied with the fact that)))
  (make-local-variable 'machlst)
  (setq machlst
	'((you have your mind on (doc// found) \, it seems \.)
	  (you think too much about  (doc// found) \.)
	  (you should try taking your mind off of (doc// found)\.)
	  (are you a computer hacker \?)))
  (make-local-variable 'qlist)
  (setq qlist
	'((what do you think \?)
	  (i\'ll ask the questions\, if you don\'t mind!)
	  (i could ask the same thing myself \.)
	  ((doc$ please) allow me to do the questioning \.)
	  (i have asked myself that question many times \.)
	  ((doc$ please) try to answer that question yourself \.)))
  (make-local-variable 'foullst)
  (setq foullst
	'(((doc$ please) watch your tongue!)
	  ((doc$ please) avoid such unwholesome thoughts \.)
	  ((doc$ please) get your mind out of the gutter \.)
	  (such lewdness is not appreciated \.)))
  (make-local-variable 'deathlst)
  (setq deathlst
	'((this is not a healthy way of thinking \.)
	  ((doc$ bother) you\, too\, may die someday \?)
	  (i am worried by your obsession with this topic!)
	  (did you watch a lot of crime and violence on television as a child \?))
	)
  (make-local-variable 'sexlst)
  (setq sexlst
	'(((doc$ areyou) (doc$ afraidof) sex \?)
	  ((doc$ describe)(doc$ something) about your sexual history \.)
	  ((doc$ please)(doc$ describe) your sex life \.\.\.)
	  ((doc$ describe) your (doc$ feelings-about) your sexual partner \.)
	  ((doc$ describe) your most (doc$ random-adjective) sexual experience \.)
	  ((doc$ areyou) satisfied with (doc// lover) \.\.\. \?)))
  (make-local-variable 'neglst)
  (setq neglst
	'((why not \?)
	  ((doc$ bother) i ask that \?)
	  (why not \?)
	  (why not \?)
	  (how come \?)
	  ((doc$ bother) i ask that \?)))
  (make-local-variable 'beclst)
  (setq beclst '(
		 (is it because (doc// sent) that you came to me \?)
		 ((doc$ bother)(doc// sent) \?)
		 (when did you first know that (doc// sent) \?)
		 (is the fact that (doc// sent) the real reason \?)
		 (does the fact that (doc// sent) explain anything else \?)
		 ((doc$ areyou)(doc$ sure)(doc// sent) \? ) ))
  (make-local-variable 'shortbeclst)
  (setq shortbeclst '(
		      ((doc$ bother) i ask you that \?)
		      (that\'s not much of an answer!)
		      ((doc$ inter) why won\'t you talk about it \?)
		      (speak up!)
		      ((doc$ areyou) (doc$ afraidof) talking about it \?)
		      (don\'t be (doc$ afraidof) elaborating \.)
		      ((doc$ please) go into more detail \.)))
  (make-local-variable 'thlst)
  (setq thlst '(
		((doc$ maybe)(doc$ thing)(doc$ isrelated) this \.)
		((doc$ maybe)(doc$ things)(doc$ arerelated) this \.)
		(is it because of (doc$ things) that you are going through all this \?)
		(how do you reconcile (doc$ things) \? )
		((doc$ maybe) this (doc$ isrelated)(doc$ things) \?) ))
  (make-local-variable 'remlst)
  (setq remlst '( (earlier you said (doc$ history) \?)
		  (you mentioned that (doc$ history) \?)
		  ((doc$ whysay)(doc$ history) \? ) ))
  (make-local-variable 'toklst)
  (setq toklst
	'((is this how you relax \?)
	  (how long have you been smoking	grass \?)
	  ((doc$ areyou) (doc$ afraidof) of being drawn to using harder stuff \?)))
  (make-local-variable 'states)
  (setq states
	'((do you get (doc// found) often \?)
	  (do you enjoy being (doc// found) \?)
	  (what makes you (doc// found) \?)
	  (how often (doc$ areyou)(doc// found) \?)
	  (when were you last (doc// found) \?)))
  (make-local-variable 'replist)
  (setq replist
	'((i . (you))
	  (my . (your))
	  (me . (you))
	  (you . (me))
	  (your . (my))
	  (mine . (yours))
	  (yours . (mine))
	  (our . (your))
	  (ours . (yours))
	  (we . (you))
	  (dunno . (do not know))
;;	  (yes . ())
	  (no\, . ())
	  (yes\, . ())
	  (ya . (i))
	  (aint . (am not))
	  (wanna . (want to))
	  (gimme . (give me))
	  (gotta . (have to))
	  (gonna . (going to))
	  (never . (not ever))
	  (doesn\'t . (does not))
	  (don\'t . (do not))
	  (aren\'t . (are not))
	  (isn\'t . (is not))
	  (won\'t . (will not))
	  (can\'t . (cannot))
	  (haven\'t . (have not))
	  (i\'m . (you are))
	  (ourselves . (yourselves))
	  (myself . (yourself))
	  (yourself . (myself))
	  (you\'re . (i am))
	  (you\'ve . (i have))
	  (i\'ve . (you have))
	  (i\'ll . (you will))
	  (you\'ll . (i shall))
	  (i\'d . (you would))
	  (you\'d . (i would))
	  (here . (there))
	  (please . ())
	  (eh\, . ())
	  (eh . ())
	  (oh\, . ())
	  (oh . ())
	  (shouldn\'t . (should not))
	  (wouldn\'t . (would not))
	  (won\'t . (will not))
	  (hasn\'t . (has not))))
  (make-local-variable 'stallmanlst)
  (setq stallmanlst '(
		      ((doc$ describe) your (doc$ feelings-about) him \.)
		      ((doc$ areyou) a friend of Stallman \?)
		      ((doc$ bother) Stallman is (doc$ random-adjective) \?)
		      ((doc$ ibelieve) you are (doc$ afraidof) him \.)))
  (make-local-variable 'schoollst)
  (setq schoollst '(
		    ((doc$ describe) your (doc// found) \.)
		    ((doc$ bother) your grades could (doc$ improve) \?)
		    ((doc$ areyou) (doc$ afraidof) (doc// found) \?)
		    ((doc$ maybe) this (doc$ isrelated) to your attitude \.)
		    ((doc$ areyou) absent often \?)
		    ((doc$ maybe) you should study (doc$ something) \.)))
  (make-local-variable 'improve)
  (setq improve '((improve) (be better) (be improved) (be higher)))
  (make-local-variable 'elizalst)
  (setq elizalst '(
		   ((doc$ areyou) (doc$ sure) \?)
		   ((doc$ ibelieve) you have (doc$ problems) with (doc// found) \.)
		   ((doc$ whysay) (doc// sent) \?)))
  (make-local-variable 'sportslst)
  (setq sportslst '(
		    (tell me (doc$ something) about (doc// found) \.)
		    ((doc$ describe) (doc$ relation) (doc// found) \.)
		    (do you find (doc// found) (doc$ random-adjective) \?)))
  (make-local-variable 'mathlst)
  (setq mathlst '(
		  ((doc$ describe) (doc$ something) about math \.)
		  ((doc$ maybe) your (doc$ problems) (doc$ arerelated) (doc// found) \.)
		  (i don\'t know much (doc// found) \, but (doc$ continue)
		     anyway \.)))
  (make-local-variable 'zippylst)
  (setq zippylst '(
		   ((doc$ areyou) Zippy \?)
		   ((doc$ ibelieve) you have some serious (doc$ problems) \.)
		   ((doc$ bother) you are a pinhead \?)))
  (make-local-variable 'chatlst)
  (setq chatlst '(
		  ((doc$ maybe) we could chat \.)
		  ((doc$ please) (doc$ describe) (doc$ something) about chat mode \.)
		  ((doc$ bother) our discussion is so (doc$ random-adjective) \?)))
  (make-local-variable 'abuselst)
  (setq abuselst '(
		   ((doc$ please) try to be less abusive \.)
		   ((doc$ describe) why you call me (doc// found) \.)
		   (i\'ve had enough of you!)))
  (make-local-variable 'abusewords)
  (setq abusewords '(boring bozo clown clumsy cretin dumb dummy
			    fool foolish gnerd gnurd idiot jerk
			    lose loser louse lousy luse luser
			    moron nerd nurd oaf oafish reek
			    stink stupid tool toolish twit))
  (make-local-variable 'howareyoulst)
  (setq howareyoulst  '((how are you) (hows it going) (hows it going eh)
			(how\'s it going) (how\'s it going eh) (how goes it)
			(whats up) (whats new) (what\'s up) (what\'s new)
			(howre you) (how\'re you) (how\'s everything)
			(how is everything) (how do you do)
			(how\'s it hanging) (que pasa)
			(how are you doing) (what do you say)))
  (make-local-variable 'whereoutp)
  (setq whereoutp '( huh remem rthing ) )
  (make-local-variable 'subj)
  (setq subj nil)
  (make-local-variable 'verb)
  (setq verb nil)
  (make-local-variable 'obj)
  (setq obj nil)
  (make-local-variable 'feared)
  (setq feared nil)
  (make-local-variable 'repetitive-shortness)
  (setq repetitive-shortness '(0 . 0))
  (make-local-variable '**mad**)
  (setq **mad** nil)
  (make-local-variable 'rms-flag)
  (setq rms-flag nil)
  (make-local-variable 'eliza-flag)
  (setq eliza-flag nil)
  (make-local-variable 'zippy-flag)
  (setq zippy-flag nil)
  (make-local-variable 'suicide-flag)
  (setq suicide-flag nil)
  (make-local-variable 'lover)
  (setq lover '(your partner))
  (make-local-variable 'bak)
  (setq bak nil)
  (make-local-variable 'lincount)
  (setq lincount 0)
  (make-local-variable '*print-upcase*)
  (setq *print-upcase* nil)
  (make-local-variable '*print-space*)
  (setq *print-space* nil)
  (make-local-variable 'howdyflag)
  (setq howdyflag nil)
  (make-local-variable 'object)
  (setq object nil))

;; Define equivalence classes of words that get treated alike.

(defun doctor-sock-meaning (x) (get x 'doctor-sock-meaning))

(defmacro doctor-sock-put-meaning (symb val)
    "Store the base meaning of a word on the property list."
    (list 'put (list 'quote symb) ''doctor-sock-meaning val))

(doctor-sock-put-meaning howdy 'howdy)
(doctor-sock-put-meaning hi 'howdy)
(doctor-sock-put-meaning greetings 'howdy)
(doctor-sock-put-meaning hello 'howdy)
(doctor-sock-put-meaning tops20 'mach)
(doctor-sock-put-meaning tops-20 'mach)
(doctor-sock-put-meaning tops 'mach)
(doctor-sock-put-meaning pdp11 'mach)
(doctor-sock-put-meaning computer 'mach)
(doctor-sock-put-meaning unix 'mach)
(doctor-sock-put-meaning machine 'mach)
(doctor-sock-put-meaning computers 'mach)
(doctor-sock-put-meaning machines 'mach)
(doctor-sock-put-meaning pdp11s 'mach)
(doctor-sock-put-meaning foo 'mach)
(doctor-sock-put-meaning foobar 'mach)
(doctor-sock-put-meaning multics 'mach)
(doctor-sock-put-meaning macsyma 'mach)
(doctor-sock-put-meaning teletype 'mach)
(doctor-sock-put-meaning la36 'mach)
(doctor-sock-put-meaning vt52 'mach)
(doctor-sock-put-meaning zork 'mach)
(doctor-sock-put-meaning trek 'mach)
(doctor-sock-put-meaning startrek 'mach)
(doctor-sock-put-meaning advent 'mach)
(doctor-sock-put-meaning pdp 'mach)
(doctor-sock-put-meaning dec 'mach)
(doctor-sock-put-meaning commodore 'mach)
(doctor-sock-put-meaning vic 'mach)
(doctor-sock-put-meaning bbs 'mach)
(doctor-sock-put-meaning modem 'mach)
(doctor-sock-put-meaning baud 'mach)
(doctor-sock-put-meaning macintosh 'mach)
(doctor-sock-put-meaning vax 'mach)
(doctor-sock-put-meaning vms 'mach)
(doctor-sock-put-meaning ibm 'mach)
(doctor-sock-put-meaning pc 'mach)
(doctor-sock-put-meaning bitching 'foul)
(doctor-sock-put-meaning shit 'foul)
(doctor-sock-put-meaning bastard 'foul)
(doctor-sock-put-meaning damn 'foul)
(doctor-sock-put-meaning damned 'foul)
(doctor-sock-put-meaning hell 'foul)
(doctor-sock-put-meaning suck 'foul)
(doctor-sock-put-meaning sucking 'foul)
(doctor-sock-put-meaning sux 'foul)
(doctor-sock-put-meaning ass 'foul)
(doctor-sock-put-meaning whore 'foul)
(doctor-sock-put-meaning bitch 'foul)
(doctor-sock-put-meaning asshole 'foul)
(doctor-sock-put-meaning shrink 'foul)
(doctor-sock-put-meaning pot 'toke)
(doctor-sock-put-meaning grass 'toke)
(doctor-sock-put-meaning weed 'toke)
(doctor-sock-put-meaning marijuana 'toke)
(doctor-sock-put-meaning acapulco 'toke)
(doctor-sock-put-meaning columbian 'toke)
(doctor-sock-put-meaning tokin 'toke)
(doctor-sock-put-meaning joint 'toke)
(doctor-sock-put-meaning toke 'toke)
(doctor-sock-put-meaning toking 'toke)
(doctor-sock-put-meaning tokin\' 'toke)
(doctor-sock-put-meaning toked 'toke)
(doctor-sock-put-meaning roach 'toke)
(doctor-sock-put-meaning pills 'drug)
(doctor-sock-put-meaning dope 'drug)
(doctor-sock-put-meaning acid 'drug)
(doctor-sock-put-meaning lsd 'drug)
(doctor-sock-put-meaning speed 'drug)
(doctor-sock-put-meaning heroin 'drug)
(doctor-sock-put-meaning hash 'drug)
(doctor-sock-put-meaning cocaine 'drug)
(doctor-sock-put-meaning meth 'drug)
(doctor-sock-put-meaning uppers 'drug)
(doctor-sock-put-meaning downers 'drug)
(doctor-sock-put-meaning loves 'loves)
(doctor-sock-put-meaning love 'love)
(doctor-sock-put-meaning loved 'love)
(doctor-sock-put-meaning hates 'hates)
(doctor-sock-put-meaning dislikes 'hates)
(doctor-sock-put-meaning hate 'hate)
(doctor-sock-put-meaning hated 'hate)
(doctor-sock-put-meaning dislike 'hate)
(doctor-sock-put-meaning stoned 'state)
(doctor-sock-put-meaning drunk 'state)
(doctor-sock-put-meaning drunken 'state)
(doctor-sock-put-meaning high 'state)
(doctor-sock-put-meaning horny 'state)
(doctor-sock-put-meaning blasted 'state)
(doctor-sock-put-meaning happy 'state)
(doctor-sock-put-meaning paranoid 'state)
(doctor-sock-put-meaning wish 'desire)
(doctor-sock-put-meaning wishes 'desire)
(doctor-sock-put-meaning want 'desire)
(doctor-sock-put-meaning desire 'desire)
(doctor-sock-put-meaning like 'desire)
(doctor-sock-put-meaning hope 'desire)
(doctor-sock-put-meaning hopes 'desire)
(doctor-sock-put-meaning desires 'desire)
(doctor-sock-put-meaning wants 'desire)
(doctor-sock-put-meaning desires 'desire)
(doctor-sock-put-meaning likes 'desire)
(doctor-sock-put-meaning needs 'desire)
(doctor-sock-put-meaning need 'desire)
(doctor-sock-put-meaning frustrated 'mood)
(doctor-sock-put-meaning depressed 'mood)
(doctor-sock-put-meaning annoyed 'mood)
(doctor-sock-put-meaning upset 'mood)
(doctor-sock-put-meaning unhappy 'mood)
(doctor-sock-put-meaning excited 'mood)
(doctor-sock-put-meaning worried 'mood)
(doctor-sock-put-meaning lonely 'mood)
(doctor-sock-put-meaning angry 'mood)
(doctor-sock-put-meaning mad 'mood)
(doctor-sock-put-meaning pissed 'mood)
(doctor-sock-put-meaning jealous 'mood)
(doctor-sock-put-meaning afraid 'fear)
(doctor-sock-put-meaning terrified 'fear)
(doctor-sock-put-meaning fear 'fear)
(doctor-sock-put-meaning scared 'fear)
(doctor-sock-put-meaning frightened 'fear)
(doctor-sock-put-meaning virginity 'sexnoun)
(doctor-sock-put-meaning virgins 'sexnoun)
(doctor-sock-put-meaning virgin 'sexnoun)
(doctor-sock-put-meaning cock 'sexnoun)
(doctor-sock-put-meaning cocks 'sexnoun)
(doctor-sock-put-meaning dick 'sexnoun)
(doctor-sock-put-meaning dicks 'sexnoun)
(doctor-sock-put-meaning cunt 'sexnoun)
(doctor-sock-put-meaning cunts 'sexnoun)
(doctor-sock-put-meaning prostitute 'sexnoun)
(doctor-sock-put-meaning condom 'sexnoun)
(doctor-sock-put-meaning sex 'sexnoun)
(doctor-sock-put-meaning rapes 'sexnoun)
(doctor-sock-put-meaning wife 'family)
(doctor-sock-put-meaning family 'family)
(doctor-sock-put-meaning brothers 'family)
(doctor-sock-put-meaning sisters 'family)
(doctor-sock-put-meaning parent 'family)
(doctor-sock-put-meaning parents 'family)
(doctor-sock-put-meaning brother 'family)
(doctor-sock-put-meaning sister 'family)
(doctor-sock-put-meaning father 'family)
(doctor-sock-put-meaning mother 'family)
(doctor-sock-put-meaning husband 'family)
(doctor-sock-put-meaning siblings 'family)
(doctor-sock-put-meaning grandmother 'family)
(doctor-sock-put-meaning grandfather 'family)
(doctor-sock-put-meaning maternal 'family)
(doctor-sock-put-meaning paternal 'family)
(doctor-sock-put-meaning stab 'death)
(doctor-sock-put-meaning murder 'death)
(doctor-sock-put-meaning murders 'death)
(doctor-sock-put-meaning suicide 'death)
(doctor-sock-put-meaning suicides 'death)
(doctor-sock-put-meaning kill 'death)
(doctor-sock-put-meaning kills 'death)
(doctor-sock-put-meaning killing 'death)
(doctor-sock-put-meaning die 'death)
(doctor-sock-put-meaning dies 'death)
(doctor-sock-put-meaning died 'death)
(doctor-sock-put-meaning dead 'death)
(doctor-sock-put-meaning death 'death)
(doctor-sock-put-meaning deaths 'death)
(doctor-sock-put-meaning pain 'symptoms)
(doctor-sock-put-meaning ache 'symptoms)
(doctor-sock-put-meaning fever 'symptoms)
(doctor-sock-put-meaning sore 'symptoms)
(doctor-sock-put-meaning aching 'symptoms)
(doctor-sock-put-meaning stomachache 'symptoms)
(doctor-sock-put-meaning headache 'symptoms)
(doctor-sock-put-meaning hurts 'symptoms)
(doctor-sock-put-meaning disease 'symptoms)
(doctor-sock-put-meaning virus 'symptoms)
(doctor-sock-put-meaning vomit 'symptoms)
(doctor-sock-put-meaning vomiting 'symptoms)
(doctor-sock-put-meaning barf 'symptoms)
(doctor-sock-put-meaning toothache 'symptoms)
(doctor-sock-put-meaning hurt 'symptoms)
(doctor-sock-put-meaning rum 'alcohol)
(doctor-sock-put-meaning gin 'alcohol)
(doctor-sock-put-meaning vodka 'alcohol)
(doctor-sock-put-meaning alcohol 'alcohol)
(doctor-sock-put-meaning bourbon 'alcohol)
(doctor-sock-put-meaning beer 'alcohol)
(doctor-sock-put-meaning wine 'alcohol)
(doctor-sock-put-meaning whiskey 'alcohol)
(doctor-sock-put-meaning scotch 'alcohol)
(doctor-sock-put-meaning fuck 'sexverb)
(doctor-sock-put-meaning fucked 'sexverb)
(doctor-sock-put-meaning screw 'sexverb)
(doctor-sock-put-meaning screwing 'sexverb)
(doctor-sock-put-meaning fucking 'sexverb)
(doctor-sock-put-meaning rape 'sexverb)
(doctor-sock-put-meaning raped 'sexverb)
(doctor-sock-put-meaning kiss 'sexverb)
(doctor-sock-put-meaning kissing 'sexverb)
(doctor-sock-put-meaning kisses 'sexverb)
(doctor-sock-put-meaning screws 'sexverb)
(doctor-sock-put-meaning fucks 'sexverb)
(doctor-sock-put-meaning because 'conj)
(doctor-sock-put-meaning but 'conj)
(doctor-sock-put-meaning however 'conj)
(doctor-sock-put-meaning besides 'conj)
(doctor-sock-put-meaning anyway 'conj)
(doctor-sock-put-meaning that 'conj)
(doctor-sock-put-meaning except 'conj)
(doctor-sock-put-meaning why 'conj)
(doctor-sock-put-meaning how 'conj)
(doctor-sock-put-meaning until 'when)
(doctor-sock-put-meaning when 'when)
(doctor-sock-put-meaning whenever 'when)
(doctor-sock-put-meaning while 'when)
(doctor-sock-put-meaning since 'when)
(doctor-sock-put-meaning rms 'rms)
(doctor-sock-put-meaning stallman 'rms)
(doctor-sock-put-meaning school 'school)
(doctor-sock-put-meaning schools 'school)
(doctor-sock-put-meaning skool 'school)
(doctor-sock-put-meaning grade 'school)
(doctor-sock-put-meaning grades 'school)
(doctor-sock-put-meaning teacher 'school)
(doctor-sock-put-meaning teachers 'school)
(doctor-sock-put-meaning classes 'school)
(doctor-sock-put-meaning professor 'school)
(doctor-sock-put-meaning prof 'school)
(doctor-sock-put-meaning profs 'school)
(doctor-sock-put-meaning professors 'school)
(doctor-sock-put-meaning mit 'school)
(doctor-sock-put-meaning emacs 'eliza)
(doctor-sock-put-meaning eliza 'eliza)
(doctor-sock-put-meaning liza 'eliza)
(doctor-sock-put-meaning elisa 'eliza)
(doctor-sock-put-meaning weizenbaum 'eliza)
(doctor-sock-put-meaning doktor 'eliza)
(doctor-sock-put-meaning athletics 'sports)
(doctor-sock-put-meaning baseball 'sports)
(doctor-sock-put-meaning basketball 'sports)
(doctor-sock-put-meaning football 'sports)
(doctor-sock-put-meaning frisbee 'sports)
(doctor-sock-put-meaning gym 'sports)
(doctor-sock-put-meaning gymnastics 'sports)
(doctor-sock-put-meaning hockey 'sports)
(doctor-sock-put-meaning lacrosse 'sports)
(doctor-sock-put-meaning soccer 'sports)
(doctor-sock-put-meaning softball 'sports)
(doctor-sock-put-meaning sports 'sports)
(doctor-sock-put-meaning swimming 'sports)
(doctor-sock-put-meaning swim 'sports)
(doctor-sock-put-meaning tennis 'sports)
(doctor-sock-put-meaning volleyball 'sports)
(doctor-sock-put-meaning math 'math)
(doctor-sock-put-meaning mathematics 'math)
(doctor-sock-put-meaning mathematical 'math)
(doctor-sock-put-meaning theorem 'math)
(doctor-sock-put-meaning axiom 'math)
(doctor-sock-put-meaning lemma 'math)
(doctor-sock-put-meaning algebra 'math)
(doctor-sock-put-meaning algebraic 'math)
(doctor-sock-put-meaning trig 'math)
(doctor-sock-put-meaning trigonometry 'math)
(doctor-sock-put-meaning trigonometric 'math)
(doctor-sock-put-meaning geometry 'math)
(doctor-sock-put-meaning geometric 'math)
(doctor-sock-put-meaning calculus 'math)
(doctor-sock-put-meaning arithmetic 'math)
(doctor-sock-put-meaning zippy 'zippy)
(doctor-sock-put-meaning zippy 'zippy)
(doctor-sock-put-meaning pinhead 'zippy)
(doctor-sock-put-meaning chat 'chat)

;;;###autoload
(defun doctor ()
  "Switch to *doctor* buffer and start giving psychotherapy."
  (interactive)
  (switch-to-buffer "*doctor*")
  (doctor-sock-mode))

(defun doctor-sock-ret-or-read (arg)
  "Insert a newline if preceding character is not a newline.
Otherwise call the Doctor to parse preceding sentence."
  (interactive "*p")
  (if (= (preceding-char) ?\n)
      (doctor-sock-read-print)
    (newline arg)))

(defun doctor-sock-read-print nil
  "top level loop"
  (interactive)
  (let ((sent (doctor-sock-readin)))
    (insert "\n")
    (setq lincount (1+ lincount))
    (doctor-sock-doc sent)
    (insert "\n")
    (setq bak sent)))

(defun doctor-sock-readin nil
  "Read a sentence.  Return it as a list of words."
  (let (sentence)
    (backward-sentence 1)
    (while (not (eobp))
      (setq sentence (append sentence (list (doctor-sock-read-token)))))
    sentence))

(defun doctor-sock-read-token ()
  "read one word from buffer"
  (prog1 (intern (downcase (buffer-substring (point)
					     (progn
					       (forward-word 1)
					       (point)))))
    (re-search-forward "\\Sw*")))

;; Main processing function for sentences that have been read.

(defun doctor-sock-doc (sent)
  (cond
   ((equal sent '(foo))
    (doctor-sock-type '(bar! (doc$ please)(doc$ continue) \.)))
   ((member sent howareyoulst)
    (doctor-sock-type '(i\'m ok \.  (doc$ describe) yourself \.)))
   ((or (member sent '((good bye) (see you later) (i quit) (so long)
		       (go away) (get lost)))
	(memq (car sent)
	      '(bye halt break quit done exit goodbye
		    bye\, stop pause goodbye\, stop pause)))
    (doctor-sock-type (doc$ bye)))
   ((and (eq (car sent) 'you)
	 (memq (cadr sent) abusewords))
    (setq found (cadr sent))
    (doctor-sock-type (doc$ abuselst)))
   ((eq (car sent) 'whatmeans)
    (doctor-sock-def (cadr sent)))
   ((equal sent '(parse))
    (doctor-sock-type (list  'subj '= subj ",  "
			'verb '= verb "\n"
			'object 'phrase '= obj ","
			'noun 'form '=  object "\n"
			'current 'keyword 'is found
			", "
			'most 'recent 'possessive
			'is owner "\n"
			'sentence 'used 'was
			"..."
			'(doc// bak))))
   ((memq (car sent) '(are is do has have how when where who why))
    (doctor-sock-type (doc$ qlist)))
   ;;   ((eq (car sent) 'forget)
   ;;    (set (cadr sent) nil)
   ;;    (doctor-sock-type '((doc$ isee)(doc$ please)
   ;;     (doc$ continue)\.)))
   (t
    (if (doctor-sock-defq sent) (doctor-sock-define sent found))
    (if (> (length sent) 12)(setq sent (doctor-sock-shorten sent)))
    (setq sent (doctor-sock-correct-spelling (doctor-sock-replace sent replist)))
    (cond ((and (not (memq 'me sent))(not (memq 'i sent))
		(memq 'am sent))
	   (setq sent (doctor-sock-replace sent '((am . (are)))))))
    (cond ((equal (car sent) 'yow) (doctor-sock-zippy))
	  ((< (length sent) 2)
	   (cond ((eq (doctor-sock-meaning (car sent)) 'howdy)
		  (doctor-sock-howdy))
		 (t (doctor-sock-short))))
	  (t
	   (if (memq 'am sent)
	       (setq sent (doctor-sock-replace sent '((me . (i))))))
	   (setq sent (doctor-sock-fixup sent))
	   (if (and (eq (car sent) 'do) (eq (cadr sent) 'not))
	       (cond ((zerop (random 3))
		      (doctor-sock-type '(are you (doc$ afraidof) that \?)))
		     ((zerop (random 2))
		      (doctor-sock-type '(don\'t tell me what to do \. i am the
					    doctor here!))
		      (doctor-sock-rthing))
		     (t
		      (doctor-sock-type '((doc$ whysay) that i shouldn\'t
				     (cddr sent)
				     \?))))
	     (doctor-sock-go (doctor-sock-wherego sent))))))))

;; Things done to process sentences once read.

(defun doctor-sock-correct-spelling (sent)
  "Correct the spelling and expand each word in sentence."
  (if sent
      (apply 'append (mapcar (lambda (word)
				(if (memq word typos)
				    (get (get word 'doctor-sock-correction) 'doctor-sock-expansion)
				  (list word)))
			     sent))))

(defun doctor-sock-shorten (sent)
  "Make a sentence manageably short using a few hacks."
  (let (foo
	(retval sent)
	(temp '(because but however besides anyway until
		    while that except why how)))
    (while temp
	   (setq foo (memq (car temp) sent))
	   (if (and foo
		    (> (length foo) 3))
	       (setq retval (doctor-sock-fixup foo)
		     temp nil)
	       (setq temp (cdr temp))))
    retval))

(defun doctor-sock-define (sent found)
  (doctor-sock-svo sent found 1 nil)
  (and
   (doctor-sock-nounp subj)
   (not (doctor-sock-pronounp subj))
   subj
   (doctor-sock-meaning object)
   (put subj 'doctor-sock-meaning (doctor-sock-meaning object))
   t))

(defun doctor-sock-defq (sent)
  "Set global var FOUND to first keyword found in sentence SENT."
  (setq found nil)
  (let ((temp '(means applies mean refers refer related
		      similar defined associated linked like same)))
    (while temp
	   (if (memq (car temp) sent)
	       (setq found (car temp)
		     temp nil)
	       (setq temp (cdr temp)))))
  found)

(defun doctor-sock-def (x)
  (progn
   (doctor-sock-type (list 'the 'word x 'means (doctor-sock-meaning x) 'to 'me))
   nil))

(defun doctor-sock-forget ()
  "Delete the last element of the history list."
  (setq history (reverse (cdr (reverse history)))))

(defun doctor-sock-query (x)
  "Prompt for a line of input from the minibuffer until a noun or verb is seen.
Put dialogue in buffer."
  (let (a
	(prompt (concat (doctor-sock-make-string x)
			" what \?  "))
	retval)
    (while (not retval)
	   (while (not a)
	     (insert ?\n
		     prompt
		     (read-string prompt)
		     ?\n)
	     (setq a (doctor-sock-readin)))
	   (while (and a (not retval))
		  (cond ((doctor-sock-nounp (car a))
			 (setq retval (car a)))
			((doctor-sock-verbp (car a))
			 (setq retval (doctor-sock-build
				       (doctor-sock-build x " ")
				       (car a))))
			((setq a (cdr a))))))
    retval))

(defun doctor-sock-subjsearch (sent key type)
  "Search for the subject of a sentence SENT, looking for the noun closest
to and preceding KEY by at least TYPE words.  Set global variable subj to
the subject noun, and return the portion of the sentence following it."
  (let ((i (- (length sent) (length (memq key sent)) type)))
    (while (and (> i -1) (not (doctor-sock-nounp (nth i sent))))
      (setq i (1- i)))
    (cond ((> i -1)
	   (setq subj (nth i sent))
	   (nthcdr (1+ i) sent))
	  (t
	   (setq subj 'you)
	   nil))))

(defun doctor-sock-nounp (x)
  "Returns t if the symbol argument is a noun."
	(or (doctor-sock-pronounp x)
	    (not (or (doctor-sock-verbp x)
		     (equal x 'not)
		     (doctor-sock-prepp x)
		     (doctor-sock-modifierp x) )) ))

(defun doctor-sock-pronounp (x)
  "Returns t if the symbol argument is a pronoun."
  (memq x '(
	i me mine myself
	we us ours ourselves ourself
	you yours yourself yourselves
	he him himself she hers herself
	it that those this these things thing
	they them themselves theirs
	anybody everybody somebody
	anyone everyone someone
	anything something everything)))

(dolist (x
         '(abort aborted aborts ask asked asks am
           applied applies apply are associate
           associated ate
           be became become becomes becoming
           been being believe believed believes
           bit bite bites bore bored bores boring bought buy buys buying
           call called calling calls came can caught catch come
           contract contracted contracts control controlled controls
           could croak croaks croaked cut cuts
           dare dared define defines dial dialed dials did die died dies
           dislike disliked
           dislikes do does drank drink drinks drinking
           drive drives driving drove dying
           eat eating eats expand expanded expands
           expect expected expects expel expels expelled
           explain explained explains
           fart farts feel feels felt fight fights find finds finding
           forget forgets forgot fought found
           fuck fucked fucking fucks
           gave get gets getting give gives go goes going gone got gotten
           had harm harms has hate hated hates have having
           hear heard hears hearing help helped helping helps
           hit hits hope hoped hopes hurt hurts
           implies imply is
           join joined joins jump jumped jumps
           keep keeping keeps kept
           kill killed killing kills kiss kissed kisses kissing
           knew know knows
           laid lay lays let lets lie lied lies like liked likes
           liking listen listens
           login look looked looking looks
           lose losing lost
           love loved loves loving
           luse lusing lust lusts
           made make makes making may mean means meant might
           move moved moves moving must
           need needed needs
           order ordered orders ought
           paid pay pays pick picked picking picks
           placed placing prefer prefers put puts
           ran rape raped rapes
           read reading reads recall receive received receives
           refer refered referred refers
           relate related relates remember remembered remembers
           romp romped romps run running runs
           said sang sat saw say says
           screw screwed screwing screws scrod see sees seem seemed
           seems seen sell selling sells
           send sendind sends sent shall shoot shot should
           sing sings sit sits sitting sold studied study
           take takes taking talk talked talking talks tell tells telling
           think thinks
           thought told took tooled touch touched touches touching
           transfer transferred transfers transmit transmits transmitted
           type types types typing
           walk walked walking walks want wanted wants was watch
           watched watching went were will wish would work worked works
           write writes writing wrote use used uses using))
  (put x 'doctor-sock-sentence-type 'verb))

(defun doctor-sock-verbp (x) (if (symbolp x)
			    (eq (get x 'doctor-sock-sentence-type) 'verb)))

(defun doctor-sock-plural (x)
  "Form the plural of the word argument."
  (let ((foo (doctor-sock-make-string x)))
    (cond ((string-equal (substring foo -1) "s")
	   (cond ((string-equal (substring foo -2 -1) "s")
		  (intern (concat foo "es")))
		 (t x)))
	   ((string-equal (substring foo -1) "y")
	    (intern (concat (substring foo 0 -1)
			    "ies")))
	   (t (intern (concat foo "s"))))))

(defun doctor-sock-setprep (sent key)
  (let ((val)
	(foo (memq key sent)))
    (cond ((doctor-sock-prepp (cadr foo))
	   (setq val (doctor-sock-getnoun (cddr foo)))
	   (cond (val val)
		 (t 'something)))
	  ((doctor-sock-articlep (cadr foo))
	   (setq val (doctor-sock-getnoun (cddr foo)))
	   (cond (val (doctor-sock-build (doctor-sock-build (cadr foo) " ") val))
		 (t 'something)))
	  (t 'something))))

(defun doctor-sock-getnoun (x)
  (cond ((null x)(setq object 'something))
	((atom x)(setq object x))
	((eq (length x) 1)
	 (setq object (cond
		       ((doctor-sock-nounp (setq object (car x))) object)
		       (t (doctor-sock-query object)))))
	((eq (car x) 'to)
	 (doctor-sock-build 'to\  (doctor-sock-getnoun (cdr x))))
	((doctor-sock-prepp (car x))
	 (doctor-sock-getnoun (cdr x)))
	((not (doctor-sock-nounp (car x)))
	 (doctor-sock-build (doctor-sock-build (cdr (assq (car x)
						(append
						 '((a . this)
						   (some . this)
						   (one . that))
						 (list
						  (cons
						   (car x) (car x))))))
				     " ")
		       (doctor-sock-getnoun (cdr x))))
	(t (setq object (car x))
	   (doctor-sock-build (doctor-sock-build (car x) " ") (doctor-sock-getnoun (cdr x))))
	))

(defun doctor-sock-modifierp (x)
  (or (doctor-sock-adjectivep x)
      (doctor-sock-adverbp x)
      (doctor-sock-othermodifierp x)))

(defun doctor-sock-adjectivep (x)
  (or (numberp x)
      (doctor-sock-nmbrp x)
      (doctor-sock-articlep x)
      (doctor-sock-colorp x)
      (doctor-sock-sizep x)
      (doctor-sock-possessivepronounp x)))

(defun doctor-sock-adverbp (xx)
  (let ((xxstr (doctor-sock-make-string xx)))
    (and (>= (length xxstr) 2)
	 (string-equal (substring (doctor-sock-make-string xx) -2) "ly")
	 (not (memq xx '(family fly jelly rally))))))

(defun doctor-sock-articlep (x)
  (memq x '(the a an)))

(defun doctor-sock-nmbrp (x)
  (memq x '(one two three four five six seven eight nine ten
		eleven twelve thirteen fourteen fifteen
		sixteen seventeen eighteen nineteen
		twenty thirty forty fifty sixty seventy eighty ninety
		hundred thousand million billion
		half quarter
		first second third fourth fifth
		sixth seventh eighth ninth tenth)))

(defun doctor-sock-colorp (x)
  (memq x '(beige black blue brown crimson
		  gray grey green
		  orange pink purple red tan tawny
		  violet white yellow)))

(defun doctor-sock-sizep (x)
  (memq x '(big large tall fat wide thick
		small petite short thin skinny)))

(defun doctor-sock-possessivepronounp (x)
  (memq x '(my your his her our their)))

(defun doctor-sock-othermodifierp (x)
  (memq x '(all also always amusing any anyway associated awesome
		bad beautiful best better but certain clear
		ever every fantastic fun funny
		good great grody gross however if ignorant
		less linked losing lusing many more much
		never nice obnoxious often poor pretty real related rich
		similar some stupid super superb
		terrible terrific too total tubular ugly very)))

(defun doctor-sock-prepp (x)
  (memq x '(about above after around as at
		  before beneath behind beside between by
		  for from in inside into
		  like near next of on onto over
		  same through thru to toward towards
		  under underneath with without)))

(defun doctor-sock-remember (thing)
  (cond ((null history)
	 (setq history (list thing)))
	(t (setq history (append history (list thing))))))

(defun doctor-sock-type (x)
  (setq x (doctor-sock-fix-2 x))
  (doctor-sock-txtype (doctor-sock-assm x)))

(defun doctor-sock-fixup (sent)
  (setq sent (append
	      (cdr
	       (assq (car sent)
		     (append
		      '((me  i)
			(him  he)
			(her  she)
			(them  they)
			(okay)
			(well)
			(sigh)
			(hmm)
			(hmmm)
			(hmmmm)
			(hmmmmm)
			(gee)
			(sure)
			(great)
			(oh)
			(fine)
			(ok)
			(no))
		      (list (list (car sent)
				  (car sent))))))
	      (cdr sent)))
  (doctor-sock-fix-2 sent))

(defun doctor-sock-fix-2 (sent)
  (let ((foo sent))
    (while foo
      (if (and (eq (car foo) 'me)
	       (doctor-sock-verbp (cadr foo)))
	  (rplaca foo 'i)
	(cond ((eq (car foo) 'you)
	       (cond ((memq (cadr foo) '(am be been is))
		      (rplaca (cdr foo) 'are))
		     ((memq (cadr foo) '(has))
		      (rplaca (cdr foo) 'have))
		     ((memq (cadr foo) '(was))
		      (rplaca (cdr foo) 'were))))
	      ((equal (car foo) 'i)
	       (cond ((memq (cadr foo) '(are is be been))
		      (rplaca (cdr foo) 'am))
		     ((memq (cadr foo) '(were))
		      (rplaca (cdr foo) 'was))
		     ((memq (cadr foo) '(has))
		      (rplaca (cdr foo) 'have))))
	      ((and (doctor-sock-verbp (car foo))
		    (eq (cadr foo) 'i)
		    (not (doctor-sock-verbp (car (cddr foo)))))
	       (rplaca (cdr foo) 'me))
	      ((and (eq (car foo) 'a)
		    (doctor-sock-vowelp (string-to-char
				    (doctor-sock-make-string (cadr foo)))))
	       (rplaca foo 'an))
	      ((and (eq (car foo) 'an)
		    (not (doctor-sock-vowelp (string-to-char
					 (doctor-sock-make-string (cadr foo))))))
	       (rplaca foo 'a)))
	(setq foo (cdr foo))))
    sent))

(defun doctor-sock-vowelp (x)
  (memq x '(?a ?e ?i ?o ?u)))

(defun doctor-sock-replace (sent rlist)
  "Replace any element of SENT that is the car of a replacement
element pair in RLIST."
  (apply 'append
	 (mapcar
	  (function
	   (lambda (x)
	     (cdr (or (assq x rlist)   ; either find a replacement
		      (list x x)))))   ; or fake an identity mapping
	  sent)))

(defun doctor-sock-wherego (sent)
  (cond ((null sent)(doc$ whereoutp))
	((null (doctor-sock-meaning (car sent)))
	 (doctor-sock-wherego (cond ((zerop (random 2))
				(reverse (cdr sent)))
			       (t (cdr sent)))))
	(t
	 (setq found (car sent))
	 (doctor-sock-meaning (car sent)))))

(defun doctor-sock-svo (sent key type mem)
  "Find subject, verb and object in sentence SENT with focus on word KEY.
TYPE is number of words preceding KEY to start looking for subject.
MEM is t if results are to be put on Doctor's memory stack.
Return in the global variables SUBJ, VERB and OBJECT."
  (let ((foo (doctor-sock-subjsearch sent key type)))
    (or foo
	(setq foo sent
	      mem nil))
    (while (and (null (doctor-sock-verbp (car foo))) (cdr foo))
      (setq foo (cdr foo)))
    (setq verb (car foo))
    (setq obj (doctor-sock-getnoun (cdr foo)))
    (cond ((eq object 'i)(setq object 'me))
	  ((eq subj 'me)(setq subj 'i)))
    (cond (mem (doctor-sock-remember (list subj verb obj))))))

(defun doctor-sock-possess (sent key)
  "Set possessive in SENT for keyword KEY.
Hack on previous word, setting global variable OWNER to correct result."
  (let* ((i (- (length sent) (length (memq key sent)) 1))
	 (prev (if (< i 0) 'your
		 (nth i sent))))
    (setq owner (if (or (doctor-sock-possessivepronounp prev)
			(string-equal "s"
				      (substring (doctor-sock-make-string prev)
						 -1)))
		    prev
		  'your))))

;; Output of replies.

(defun doctor-sock-txtype (ans)
  "Output to buffer a list of symbols or strings as a sentence."
  (setq *print-upcase* t *print-space* nil)
  (mapc 'doctor-sock-type-symbol ans)
  (insert "\n"))

(defun doctor-sock-type-symbol (word)
  "Output a symbol to the buffer with some fancy case and spacing hacks."
  (setq word (doctor-sock-make-string word))
  (if (string-equal word "i") (setq word "I"))
  (if *print-upcase*
      (progn
	(setq word (capitalize word))
	(if *print-space*
	    (insert " "))))
  (cond ((or (string-match "^[.,;:?! ]" word)
	     (not *print-space*))
	 (insert word))
	(t (insert ?\s word)))
  (and auto-fill-function
       (> (current-column) fill-column)
       (apply auto-fill-function nil))
  (setq *print-upcase* (string-match "[.?!]$" word)
	*print-space* t))

(defun doctor-sock-build (str1 str2)
  "Make a symbol out of the concatenation of the two non-list arguments."
  (cond ((null str1) str2)
	((null str2) str1)
	((and (atom str1)
	      (atom str2))
	 (intern (concat (doctor-sock-make-string str1)
			 (doctor-sock-make-string str2))))
	(t nil)))

(defun doctor-sock-make-string (obj)
  (cond ((stringp obj) obj)
	((symbolp obj) (symbol-name obj))
	((numberp obj) (int-to-string obj))
	(t "")))

(defun doctor-sock-concat (x y)
  "Like append, but force atomic arguments to be lists."
  (append
   (if (and x (atom x)) (list x) x)
   (if (and y (atom y)) (list y) y)))

(defun doctor-sock-assm (proto)
  (cond ((null proto) nil)
	((atom proto) (list proto))
	((atom (car proto))
	 (cons (car proto) (doctor-sock-assm (cdr proto))))
	(t (doctor-sock-concat (doctor-sock-assm (eval (car proto))) (doctor-sock-assm (cdr proto))))))

;; Functions that handle specific words or meanings when found.

(defun doctor-sock-go (destination)
  "Call a `doctor-sock-*' function."
  (funcall (intern (concat "doctor-sock-" (doctor-sock-make-string destination)))))

(defun doctor-sock-desire1 ()
  (doctor-sock-go (doc$ whereoutp)))

(defun doctor-sock-huh ()
  (cond ((< (length sent) 9) (doctor-sock-type (doc$ huhlst)))
	(t (doctor-sock-type (doc$ longhuhlst)))))

(defun doctor-sock-rthing () (doctor-sock-type (doc$ thlst)))

(defun doctor-sock-remem () (cond ((null history)(doctor-sock-huh))
			     ((doctor-sock-type (doc$ remlst)))))

(defun doctor-sock-howdy ()
  (cond ((not howdyflag)
	 (doctor-sock-type '((doc$ hello) what brings you to see me \?))
	 (setq howdyflag t))
	(t
	 (doctor-sock-type '((doc$ ibelieve) we\'ve introduced ourselves already \.))
	 (doctor-sock-type '((doc$ please) (doc$ describe) (doc$ things) \.)))))

(defun doctor-sock-when ()
  (cond ((< (length (memq found sent)) 3)(doctor-sock-short))
	(t
	 (setq sent (cdr (memq found sent)))
	 (setq sent (doctor-sock-fixup sent))
	 (doctor-sock-type '((doc$ whatwhen)(doc// sent) \?)))))

(defun doctor-sock-conj ()
  (cond ((< (length (memq found sent)) 4)(doctor-sock-short))
	(t
	 (setq sent (cdr (memq found sent)))
	 (setq sent (doctor-sock-fixup sent))
	 (cond ((eq (car sent) 'of)
		(doctor-sock-type '(are you (doc$ sure) that is the real reason \?))
		(setq things (cons (cdr sent) things)))
	       (t
		(doctor-sock-remember sent)
		(doctor-sock-type (doc$ beclst)))))))

(defun doctor-sock-short ()
  (cond ((= (car repetitive-shortness) (1- lincount))
	 (rplacd repetitive-shortness
		 (1+ (cdr repetitive-shortness))))
	(t
	 (rplacd repetitive-shortness 1)))
  (rplaca repetitive-shortness lincount)
  (cond ((> (cdr repetitive-shortness) 6)
	 (cond ((not **mad**)
		(doctor-sock-type '((doc$ areyou)
			       just trying to see what kind of things
			       i have in my vocabulary \? please try to
			       carry on a reasonable conversation!))
		(setq **mad** t))
	       (t
		(doctor-sock-type '(i give up \. you need a lesson in creative
				 writing \.\.\.))
		)))
	(t
	 (cond ((equal sent (doctor-sock-assm '(yes)))
		(doctor-sock-type '((doc$ isee) (doc$ inter) (doc$ whysay) this is so \?)))
	       ((equal sent (doctor-sock-assm '(because)))
		(doctor-sock-type (doc$ shortbeclst)))
	       ((equal sent (doctor-sock-assm '(no)))
		(doctor-sock-type (doc$ neglst)))
	       (t (doctor-sock-type (doc$ shortlst)))))))

(defun doctor-sock-alcohol () (doctor-sock-type (doc$ drnk)))

(defun doctor-sock-desire ()
  (let ((foo (memq found sent)))
    (cond ((< (length foo) 2)
	   (doctor-sock-go (doctor-sock-build (doctor-sock-meaning found) 1)))
	  ((memq (cadr foo) '(a an))
	   (rplacd foo (append '(to have) (cdr foo)))
	   (doctor-sock-svo sent found 1 nil)
	   (doctor-sock-remember (list subj 'would 'like obj))
	   (doctor-sock-type (doc$ whywant)))
	  ((not (eq (cadr foo) 'to))
	   (doctor-sock-go (doctor-sock-build (doctor-sock-meaning found) 1)))
	  (t
	   (doctor-sock-svo sent found 1 nil)
	   (doctor-sock-remember (list subj 'would 'like obj))
	   (doctor-sock-type (doc$ whywant))))))

(defun doctor-sock-drug ()
  (doctor-sock-type (doc$ drugs))
  (doctor-sock-remember (list 'you 'used found)))

(defun doctor-sock-toke ()
  (doctor-sock-type (doc$ toklst)))

(defun doctor-sock-state ()
  (doctor-sock-type (doc$ states))(doctor-sock-remember (list 'you 'were found)))

(defun doctor-sock-mood ()
  (doctor-sock-type (doc$ moods))(doctor-sock-remember (list 'you 'felt found)))

(defun doctor-sock-fear ()
  (setq feared (doctor-sock-setprep sent found))
  (doctor-sock-type (doc$ fears))
  (doctor-sock-remember (list 'you 'were 'afraid 'of feared)))

(defun doctor-sock-hate ()
  (doctor-sock-svo sent found 1 t)
  (cond ((memq 'not sent) (doctor-sock-forget) (doctor-sock-huh))
	((equal subj 'you)
	 (doctor-sock-type '(why do you (doc// verb)(doc// obj) \?)))
	(t (doctor-sock-type '((doc$ whysay)(list subj verb obj))))))

(defun doctor-sock-symptoms ()
  (doctor-sock-type '((doc$ maybe) you should consult a medical doctor\;
		 i am a psychotherapist. \.)))

(defun doctor-sock-hates ()
  (doctor-sock-svo sent found 1 t)
  (doctor-sock-hates1))

(defun doctor-sock-hates1 ()
  (doctor-sock-type '((doc$ whysay)(list subj verb obj) \?)))

(defun doctor-sock-loves ()
  (doctor-sock-svo sent found 1 t)
  (doctor-sock-qloves))

(defun doctor-sock-qloves ()
  (doctor-sock-type '((doc$ bother)(list subj verb obj) \?)))

(defun doctor-sock-love ()
  (doctor-sock-svo sent found 1 t)
  (cond ((memq 'not sent) (doctor-sock-forget) (doctor-sock-huh))
	((memq 'to sent) (doctor-sock-hates1))
	(t
	 (cond ((equal object 'something)
		(setq object '(this person you love))))
	 (cond ((equal subj 'you)
		(setq lover obj)
		(cond ((equal lover '(this person you love))
		       (setq lover '(your partner))
		       (doctor-sock-forget)
		       (doctor-sock-type '(with whom are you in love \?)))
		      ((doctor-sock-type '((doc$ please)
				      (doc$ describe)
				      (doc$ relation)
				      (doc// lover)
				      \.)))))
	       ((equal subj 'i)
		(doctor-sock-txtype '(we were discussing you!)))
	       (t (doctor-sock-forget)
		  (setq obj 'someone)
		  (setq verb (doctor-sock-build verb 's))
		  (doctor-sock-qloves))))))

(defun doctor-sock-mach ()
  (setq found (doctor-sock-plural found))
  (doctor-sock-type (doc$ machlst)))

(defun doctor-sock-sexnoun () (doctor-sock-sexverb))

(defun doctor-sock-sexverb ()
  (if (or (memq 'me sent)(memq 'myself sent)(memq 'i sent))
      (doctor-sock-foul)
    (doctor-sock-type (doc$ sexlst))))

(defun doctor-sock-death ()
  (cond (suicide-flag (doctor-sock-type (doc$ deathlst)))
	((or (equal found 'suicide)
             (and (or (equal found 'kill)
                      (equal found 'killing))
                  (memq 'yourself sent)))
	 (setq suicide-flag t)
	 (doctor-sock-type '(If you are really suicidal, you might
			   want to contact the Samaritans via
			   E-mail: jo@samaritans.org or, at your option,
			   anonymous E-mail: samaritans@anon.twwells.com\ \.
                           or find a Befrienders crisis center at
			   http://www.befrienders.org/\ \.
			   (doc$ please) (doc$ continue) \.)))
	(t (doctor-sock-type (doc$ deathlst)))))

(defun doctor-sock-foul ()
  (doctor-sock-type (doc$ foullst)))

(defun doctor-sock-family ()
  (doctor-sock-possess sent found)
  (doctor-sock-type (doc$ famlst)))

;; I did not add this -- rms.
;; But he might have removed it.  I put it back.  --roland
(defun doctor-sock-rms ()
  (cond (rms-flag (doctor-sock-type (doc$ stallmanlst)))
	(t (setq rms-flag t) (doctor-sock-type '(do you know Stallman \?)))))

(defun doctor-sock-school nil (doctor-sock-type (doc$ schoollst)))

(defun doctor-sock-eliza ()
  (cond (eliza-flag (doctor-sock-type (doc$ elizalst)))
	(t (setq eliza-flag t)
	   (doctor-sock-type '((doc// found) \? hah !
			  (doc$ please) (doc$ continue) \.)))))

(defun doctor-sock-sports ()  (doctor-sock-type (doc$ sportslst)))

(defun doctor-sock-math () (doctor-sock-type (doc$ mathlst)))

(defun doctor-sock-zippy ()
  (cond (zippy-flag (doctor-sock-type (doc$ zippylst)))
	(t (setq zippy-flag t)
	   (doctor-sock-type '(yow! are we interactive yet \?)))))


(defun doctor-sock-chat () (doctor-sock-type (doc$ chatlst)))

(random t)

(provide 'doctor-sock-sock)

