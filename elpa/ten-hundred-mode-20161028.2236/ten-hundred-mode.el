;;; ten-hundred-mode.el --- use only the ten hundred most usual words

;; Package-Version: 20160408.001
;; Package-Requires: ((cl-lib "0.5"))
;; Copyright 2016 Aaron Miller <me@aaron-miller.me>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Lately there has been some interest in writing with only the ten
;; hundred most used English words.  Even the news has talked about one
;; thing that makes your computer take away words that aren't in the
;; ten hundred most used.

;; (Mostly the news talked about that thing because the person who
;; wrote it did so in part to make a point about someone called Donald
;; Trump, not because the thing was interesting in its own right.  But
;; that's okay, because the people who write the news need something
;; to do all day just like everyone else.)

;; Someone on Hacker News asked if there was a thing like that for
;; Emacs.  There is, sort of, but it's old and not easy to find, and it
;; doesn't work very well.  So I thought I'd write a new one that
;; everyone can find and use.  This is that thing.

;; It knows the ten hundred most used words, and when you type a word,
;; it checks to see if your word is one of them.  If not, the computer
;; takes away the word you typed, and suggests some words like the one
;; it took away but which are okay to use.  (This last part can be a
;; little slow, and it isn't always very helpful, so you can turn it
;; off if you want.) Words that start with a big letter, like names,
;; don't get taken away, even if they would be normally.

;; This thing doesn't know very much.  If you aren't typing normally,
;; it probably will let you get away with using words you aren't
;; supposed to use.  That's not nice, though, so don't do that.

;;; Contributing:

;; If you think of a way this thing can do a better job of taking away
;; words that aren't okay to use, let me know, or (even better) add it
;; to the thing and then let me know.

;; Here's where this thing lives:
;; https://github.com/aaron-em/ten-hundred-mode.el

;; I hope you like it!

;;; Code:

(require 'cl-lib)
(require 'thingatpt)

;; stem-english.el and levenshtein.el are not available on MELPA
;; stable, which is the only flavor I use. If you use MELPA edge and
;; already have these installed, then the installed versions will be
;; used; otherwise, we'll load our own copies.
(unless (featurep 'stem-english)
  (progn
    (load-file (concat
                (file-name-directory (or load-file-name
                                         buffer-file-name))
                "/lib/stem-english.el"))
    (require 'stem-english)))

(unless (featurep 'levenshtein)
  (progn
    (load-file (concat
                (file-name-directory (or load-file-name
                                         buffer-file-name))
                "/lib/levenshtein.el"))
    (require 'levenshtein)))


(defgroup ten-hundred-mode nil
  "Customization options for `ten-hundred-mode'.")

(defcustom ten-hundred-mode-offer-suggestions t
  "Whether `ten-hundred-mode' should suggest alternatives when removing a word.

  These suggestions are based entirely on the Levenshtein
distance between the removed word and members of
`ten-hundred-mode-words', so they may not be all that
applicable."
  :group 'ten-hundred-mode
  :type 'boolean)

(defvar ten-hundred-mode--whitespace-regexp
  "[
 ]"
  "A regular expression matching whitespace.")

(defvar ten-hundred-mode--punctuation-regexp
  "[].,!?)}>]"
  "A regular expression matching end-of-word punctuation.")

(defvar ten-hundred-mode-words
  '("a" "able" "about" "above" "accept" "across" "act" "actually"
  "add" "admit" "afraid" "after" "afternoon" "again" "against" "age"
  "ago" "agree" "ah" "ahead" "air" "all" "allow" "almost" "alone"
  "along" "already" "alright" "also" "although" "always" "am" "amaze"
  "an" "and" "anger" "angry" "animal" "annoy" "another" "answer" "any"
  "anymore" "anyone" "anything" "anyway" "apartment" "apparently"
  "appear" "approach" "are" "area" "aren't" "arm" "around" "arrive"
  "as" "ask" "asleep" "ass" "at" "attack" "attempt" "attention" "aunt"
  "avoid" "away" "baby" "back" "bad" "bag" "ball" "band" "bar"
  "barely" "bathroom" "be" "beat" "beautiful" "became" "because"
  "become" "bed" "bedroom" "been" "before" "began" "begin" "behind"
  "believe" "bell" "beside" "besides" "best" "better" "between" "big"
  "bit" "bite" "black" "blink" "block" "blonde" "blood" "blue" "blush"
  "body" "book" "bore" "both" "bother" "bottle" "bottom" "box" "boy"
  "boyfriend" "brain" "break" "breakfast" "breath" "breathe" "bright"
  "bring" "broke" "broken" "brother" "brought" "brown" "brush" "build"
  "burn" "burst" "bus" "business" "busy" "but" "buy" "by" "call"
  "calm" "came" "can" "can't" "car" "card" "care" "carefully" "carry"
  "case" "cat" "catch" "caught" "cause" "cell" "chair" "chance"
  "change" "chase" "check" "cheek" "chest" "child" "children"
  "chuckle" "city" "class" "clean" "clear" "climb" "close" "clothes"
  "coffee" "cold" "college" "color" "come" "comment" "complete"
  "completely" "computer" "concern" "confuse" "consider" "continue"
  "control" "conversation" "cool" "corner" "couch" "could" "couldn't"
  "counter" "couple" "course" "cover" "crack" "crazy" "cross" "crowd"
  "cry" "cup" "cut" "cute" "dad" "damn" "dance" "dark" "date"
  "daughter" "day" "dead" "deal" "dear" "death" "decide" "deep"
  "definitely" "desk" "did" "didn't" "die" "different" "dinner"
  "direction" "disappear" "do" "doctor" "does" "doesn't" "dog" "don't"
  "done" "door" "doubt" "down" "drag" "draw" "dream" "dress" "drink"
  "drive" "drop" "drove" "dry" "during" "each" "ear" "early" "easily"
  "easy" "eat" "edge" "either" "else" "empty" "end" "enjoy" "enough"
  "enter" "entire" "escape" "especially" "even" "evening" "eventually"
  "ever" "every" "everyone" "everything" "exactly" "except" "excite"
  "exclaim" "excuse" "expect" "explain" "expression" "eye" "eyebrow"
  "face" "fact" "fall" "family" "far" "fast" "father" "fault"
  "favorite" "fear" "feel" "feet" "fell" "felt" "few" "field" "fight"
  "figure" "fill" "finally" "find" "fine" "finger" "finish" "fire"
  "first" "fit" "five" "fix" "flash" "flip" "floor" "fly" "focus"
  "follow" "food" "foot" "for" "force" "forget" "form" "forward"
  "found" "four" "free" "friend" "from" "front" "frown" "fuck" "full"
  "fun" "funny" "further" "game" "gasp" "gave" "gaze" "gently" "get"
  "giggle" "girl" "girlfriend" "give" "given" "glad" "glance" "glare"
  "glass" "go" "God" "gone" "gonna" "good" "got" "gotten" "grab"
  "great" "green" "greet" "grey" "grin" "grip" "groan" "ground"
  "group" "grow" "guard" "guess" "gun" "guy" "had" "hadn't" "hair"
  "half" "hall" "hallway" "hand" "handle" "hang" "happen" "happy"
  "hard" "has" "hate" "have" "haven't" "he" "he'd" "he's" "head"
  "hear" "heard" "heart" "heavy" "held" "hell" "hello" "help" "her"
  "here" "herself" "hey" "hi" "hide" "high" "him" "himself" "his"
  "hit" "hold" "home" "hope" "horse" "hospital" "hot" "hour" "house"
  "how" "however" "hug" "huge" "huh" "human" "hundred" "hung" "hurry"
  "hurt" "I" "I'd" "I'll" "I'm" "I've" "ice" "idea" "if" "ignore"
  "imagine" "immediately" "important" "in" "inside" "instead"
  "interest" "interrupt" "into" "is" "isn't" "it" "it's" "its"
  "jacket" "jeans" "jerk" "job" "join" "joke" "jump" "just" "keep"
  "kept" "key" "kick" "kid" "kill" "kind" "kiss" "kitchen" "knee"
  "knew" "knock" "know" "known" "lady" "land" "large" "last" "late"
  "laugh" "lay" "lead" "lean" "learn" "least" "leave" "led" "left"
  "leg" "less" "let" "letter" "lie" "life" "lift" "light" "like"
  "line" "lip" "listen" "little" "live" "lock" "locker" "long" "look"
  "lose" "lost" "lot" "loud" "love" "low" "lunch" "mad" "made" "make"
  "man" "manage" "many" "mark" "marry" "match" "matter" "may" "maybe"
  "me" "mean" "meant" "meet" "memory" "men" "mention" "met" "middle"
  "might" "mind" "mine" "minute" "mirror" "miss" "mom" "moment"
  "money" "month" "mood" "more" "morning" "most" "mother" "mouth"
  "move" "movie" "Mr." "Mrs." "much" "mum" "mumble" "music" "must"
  "mutter" "my" "myself" "name" "near" "nearly" "neck" "need"
  "nervous" "never" "new" "next" "nice" "night" "no" "nod" "noise"
  "none" "normal" "nose" "not" "note" "nothing" "notice" "now"
  "number" "obviously" "of" "off" "offer" "office" "often" "oh" "okay"
  "old" "on" "once" "one" "only" "onto" "open" "or" "order" "other"
  "our" "out" "outside" "over" "own" "pack" "pain" "paint" "pair"
  "pants" "paper" "parents" "park" "part" "party" "pass" "past"
  "pause" "pay" "people" "perfect" "perhaps" "person" "phone" "pick"
  "picture" "piece" "pink" "piss" "place" "plan" "play" "please"
  "pocket" "point" "police" "pop" "position" "possible" "power"
  "practically" "present" "press" "pretend" "pretty" "probably"
  "problem" "promise" "pull" "punch" "push" "put" "question" "quick"
  "quickly" "quiet" "quietly" "quite" "race" "rain" "raise" "ran"
  "rang" "rather" "reach" "read" "ready" "real" "realize" "really"
  "reason" "recognize" "red" "relationship" "relax" "remain"
  "remember" "remind" "repeat" "reply" "respond" "rest" "return"
  "ride" "right" "ring" "road" "rock" "roll" "room" "rose" "round"
  "rub" "run" "rush" "sad" "safe" "said" "same" "sat" "save" "saw"
  "say" "scare" "school" "scream" "search" "seat" "second" "see"
  "seem" "seen" "self" "send" "sense" "sent" "serious" "seriously"
  "set" "settle" "seven" "several" "shadow" "shake" "share" "she"
  "she'd" "she's" "shift" "shirt" "shit" "shock" "shoe" "shook" "shop"
  "short" "shot" "should" "shoulder" "shouldn't" "shout" "shove"
  "show" "shower" "shrug" "shut" "sick" "side" "sigh" "sight" "sign"
  "silence" "silent" "simply" "since" "single" "sir" "sister" "sit"
  "situation" "six" "skin" "sky" "slam" "sleep" "slightly" "slip"
  "slow" "slowly" "small" "smell" "smile" "smirk" "smoke" "snap" "so"
  "soft" "softly" "some" "somehow" "someone" "something" "sometimes"
  "somewhere" "son" "song" "soon" "sorry" "sort" "sound" "space"
  "speak" "spend" "spent" "spoke" "spot" "stair" "stand" "star"
  "stare" "start" "state" "stay" "step" "stick" "still" "stomach"
  "stood" "stop" "store" "story" "straight" "strange" "street"
  "strong" "struggle" "stuck" "student" "study" "stuff" "stupid"
  "such" "suck" "sudden" "suddenly" "suggest" "summer" "sun" "suppose"
  "sure" "surprise" "surround" "sweet" "table" "take" "taken" "talk"
  "tall" "teacher" "team" "tear" "teeth" "tell" "ten" "than" "thank"
  "that" "that's" "the" "their" "them" "themselves" "then" "there"
  "there's" "these" "they" "they'd" "they're" "thick" "thing" "think"
  "third" "this" "those" "though" "thought" "three" "threw" "throat"
  "through" "throw" "tie" "tight" "time" "tiny" "tire" "to" "today"
  "together" "told" "tomorrow" "tone" "tongue" "tonight" "too" "took"
  "top" "totally" "touch" "toward" "town" "track" "trail" "train"
  "tree" "trip" "trouble" "true" "trust" "truth" "try" "turn" "TV"
  "twenty" "two" "type" "uncle" "under" "understand" "until" "up"
  "upon" "us" "use" "usual" "usually" "very" "visit" "voice" "wait"
  "wake" "walk" "wall" "want" "warm" "warn" "was" "wasn't" "watch"
  "water" "wave" "way" "we" "we'll" "we're" "we've" "wear" "week"
  "weird" "well" "went" "were" "weren't" "wet" "what" "what's"
  "whatever" "when" "where" "whether" "which" "while" "whisper"
  "white" "who" "whole" "why" "wide" "wife" "will" "wind" "window"
  "wipe" "wish" "with" "within" "without" "woke" "woman" "women"
  "won't" "wonder" "wood" "word" "wore" "work" "world" "worry" "worse"
  "would" "wouldn't" "wow" "wrap" "write" "wrong" "yeah" "year" "yell"
  "yes" "yet" "you" "you'd" "you'll" "you're" "you've" "young" "your"
  "yourself")
  "The ten hundred most commonly used English words.")

(defun ten-hundred-mode--comma-list (words)
  "Make a grammatical English list from WORDS.

This function will produce a grammatically correct English list,
including the serial or 'Oxford' comma, from a list of any number
of words greater than zero."
  (cond
    ((= 1 (length words))
     (concat "'" (nth 0 words) "'"))
    ((= 2 (length words))
     (concat "'"
             (mapconcat #'identity words "' or '")
             "'"))
    (t (concat "'"
               (mapconcat #'identity (butlast words 2) "', '")
               "', '"
               (mapconcat #'identity (list
                                      (nth (- (length words) 2) words)
                                      (nth (- (length words) 1) words)) "', or '")
               "'"))))

(defun ten-hundred-mode--did-you-mean (target)
  "Offer suggestions for permissible alternatives to TARGET."
  (let* ((minimal-cost most-positive-fixnum)
         (cheapest-words nil))
    (dolist (word ten-hundred-mode-words)
      (let ((cost (levenshtein-distance word target)))
        (and (< cost minimal-cost)
             (progn
               (setq minimal-cost cost)
               (setq cheapest-words nil)))
        (and (= cost minimal-cost)
             (push word cheapest-words))))
    (message "Perhaps you meant %s."
             (ten-hundred-mode--comma-list cheapest-words))))

(defun ten-hundred-mode--end-of-word-p (char)
  "Test whether CHAR might be the end of a word.

'The end of a word' is here defined as either whitespace or
word-terminating punctuation.  These categories are themselves
embodied in regexps bound to
`ten-hundred-mode--whitespace-regexp' and
`ten-hundred-mode--punctuation-regexp', which see."
  (save-match-data
    (or (not (null (string-match ten-hundred-mode--whitespace-regexp char)))
        (not (null (string-match ten-hundred-mode--punctuation-regexp char))))))

(defun ten-hundred-mode--permissible-p (word)
  "Test whether WORD, or its stem, is in `ten-hundred-mode-words'.

This function uses Porter stemming, provided by the stem-english
library, to avoid spurious rejection of a permissible word's
inflected form."
  (let* ((stems
          (stem-english word))
         (permissible
          (or (string= "" word)
              (null stems)
              (not (null (member word ten-hundred-mode-words)))
              (cl-reduce #'(lambda (m n) (or (not (null m)) (not (null n))))
                         (mapcar #'(lambda (s)
                                     (member s ten-hundred-mode-words))
                                 stems)))))
    permissible))

(defun ten-hundred-mode--inspect-last-word nil
  "Examine the last typed word for permissibility.

If it fails that check, remove it, and offer alternatives if
`ten-hundred-mode-offer-suggestions' is non-nil."
  (save-excursion
    (save-match-data
      (let ((case-fold-search nil)
            last word-start word-end delete-until word)
        (if (bolp) (backward-char 1))
        (setq delete-until (point))
        (while (string-match "\\W" (setq last (buffer-substring-no-properties (point) (- (point) 1))))
          (backward-char 1))
        (setq word-end (point))
        (re-search-backward "^\\|[^_[:alnum:]]" nil t)
        (while (looking-at "\\W")
          (forward-char 1))
        (setq word-start (point))
        (and word-start word-end
             (setq word (buffer-substring-no-properties word-start word-end)))
        (if (and word
                 (not (string-match "[A-Z]"
                                    (char-to-string (elt word 0))))
                 (not (ten-hundred-mode--permissible-p word)))
            (progn
              (delete-region word-start delete-until)
              (and ten-hundred-mode-offer-suggestions
                   (ten-hundred-mode--did-you-mean word))))))))

(defun ten-hundred-mode--post-self-insert nil
  "Hook applying `ten-hundred-mode' to newly typed words."
  (save-excursion
    (let ((last (buffer-substring-no-properties (point) (- (point) 1))))
      (and (not (string= "'" last))
           (ten-hundred-mode--end-of-word-p last)
           (ten-hundred-mode--inspect-last-word)))))

(defun ten-hundred-mode--enable nil
  "Enable `ten-hundred-mode' in the current buffer."
  
  (add-hook 'post-self-insert-hook
            #'ten-hundred-mode--post-self-insert
            nil t))

(defun ten-hundred-mode--disable nil
  "Disable `ten-hundred-mode' in the current buffer."
  (remove-hook 'post-self-insert-hook
               #'ten-hundred-mode--post-self-insert
               t))

;;;###autoload
(define-minor-mode ten-hundred-mode
    "This makes you write with only the ten hundred most usual
words that everyone uses every day. If you try to use any other
words, the computer takes them away for you, so you can try
again. So you always use easy words that everyone can
understand, and no one has trouble figuring out what you want
to say."
  nil " 1k" nil
  (if ten-hundred-mode
      (ten-hundred-mode--enable)
      (ten-hundred-mode--disable)))

(provide 'ten-hundred-mode)

;;; ten-hundred-mode.el ends here
