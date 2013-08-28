(require 's)
(eval-when-compile (require 'cl)) ;; loop, return

(defvar mst--remaining-lexemes nil
  "Since `mst--parse-inner' recursively calls itself, we need a shared value to mutate.")

(defun mst--parse (lexemes)
  "Given a list LEXEMES, return a list of lexemes nested according to #tags or ^tags."
  (setq mst--remaining-lexemes lexemes)
  (mst--parse-inner))

(defun mst--parse-inner (&optional section-name)
  "Parse `mst--remaining-lexemes', and return a list of lexemes nested according to #tags or ^tags."
  (let (parsed-lexemes
        lexeme)
    (loop while mst--remaining-lexemes do
          (setq lexeme (pop mst--remaining-lexemes))
          (cond
           ((mst--open-section-p lexeme)
            ;; recurse on this nested section
            (push (cons lexeme (mst--parse-inner (mst--section-name lexeme))) parsed-lexemes))
           ((mst--close-section-p lexeme)
            ;; this is the last tag in this section
            (unless (equal section-name (mst--section-name lexeme))
              (error "Mismatched brackets: You closed a section with %s, but it wasn't open" section-name))
            (push lexeme parsed-lexemes)
            (return))
           (t
            ;; this is just a tag in the current section
            (push lexeme parsed-lexemes))))

    ;; ensure we aren't inside an unclosed section
    (when (and section-name (not (mst--close-section-p lexeme)))
      (error "Unclosed section: You haven't closed %s" section-name))

    (nreverse parsed-lexemes)))

(defun mst--open-section-p (lexeme)
  "Is LEXEME a #tag or ^tag ?"
  (destructuring-bind (type value) lexeme
    (and (equal type :tag)
         (or
          (s-starts-with-p "#" value)
          (s-starts-with-p "^" value)))))

(defun mst--close-section-p (lexeme)
  "Is LEXEME a /tag ?"
  (destructuring-bind (type value) lexeme
    (and (equal type :tag)
         (s-starts-with-p "/" value))))

(defun mst--section-name (lexeme)
  "Get the name of the section from LEXEME, a two part list returned by `mst--lex'.
The leading character (the #, ^ or /) is stripped."
  (s-chop-prefixes '("#" "^" "/") (cadr lexeme)))
