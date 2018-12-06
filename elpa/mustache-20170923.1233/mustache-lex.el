(eval-when-compile (require 'cl)) ;; first, second, destructuring-bind, loop
(require 's)

(defun mst--lex (template)
  "Iterate through TEMPLATE, splitting {{ tags }} and bare strings.
We return a list of lists: ((:text \"foo\") (:tag \"variable-name\"))"
  ;; convert {{{foo}}} to {{& foo}}
  (setq template (replace-regexp-in-string "{{{\\(.*?\\)}}}" "{{& \\1}}" template))
  
  (let ((open-delimeter "{{")
        (close-delimeter "}}")
        (lexemes nil))
    (while (not (s-equals? template ""))
      (let* ((open-index (s-index-of open-delimeter template))
             (close-index (s-index-of close-delimeter template)))
        ;; todo: check open-index < close-index
        ;; todo: error if we have an open and no close
        (if (and open-index close-index)
            ;; we have a tag
            (let ((between-delimeters
                   (substring template (+ open-index (length open-delimeter)) close-index))
                  (continue-from-index (+ close-index (length close-delimeter))))
              ;; save the string before the tag
              (when (> open-index 0)
                (push (list :text (substring template 0 open-index)) lexemes))
              
              ;; if this is a tag that changes delimeters e.g. {{=<< >>=}}
              ;; then set the new open/close delimeter string
              (if (s-matches-p "^=.+ .+=$" between-delimeters)
                (let* (;; strip leading/trailing =
                       (delimeter-spec (substring between-delimeters 1 -1))
                       (spec-parts (s-split " " delimeter-spec)))
                  (setq open-delimeter (first spec-parts))
                  (setq close-delimeter (second spec-parts)))

                ;; otherwise it's a normal tag, so save it
                (push (list :tag (s-trim between-delimeters)) lexemes))
              
              ;; iterate on the remaining template
              (setq template
                    (substring template continue-from-index)))
          ;; else only plain text left
          (progn
            (push (list :text template) lexemes)
            (setq template "")))))
    (nreverse lexemes)))

(defun mst--clean-whitespace (lexemes)
  "Given a list of LEXEMES, remove whitespace around sections and
comments if they're on their own on a line. Modifies the original
list."
  ;; iterate over all lexemes:
  (loop for i from 0 to (- (length lexemes) 3)
        collect
        ;; find sections that have plain text before and after
        (let ((first (elt lexemes i))
              (second (elt lexemes (+ i 1)))
              (third (elt lexemes (+ i 2))))
          (when (and (mst--text-p first)
                     (or
                      (mst--section-tag-p second)
                      (mst--comment-tag-p second))
                     (mst--text-p third)
                     ;; check the section is on its own line
                     (string-match-p "\n *$" (mst--tag-text first))
                     (string-match-p "^\n" (mst--tag-text third)))
            ;; then we cleanup whitespace
            (setf (elt lexemes i) (mst--no-trailing-newline first)))))
  lexemes)

(defalias 'mst--tag-text 'second
  "Returns the text context of a tag.")

(defun mst--no-trailing-newline (lexeme)
  "Replace \"\n\" or \"\n   \" at the end of a plain text lexeme."
  (list
   :text
   (replace-regexp-in-string "\n *$" "" (mst--tag-text lexeme))))

(defun mst--tag-p (lexeme)
  "Is LEXEME a tag?"
  (equal (car lexeme) :tag))

(defun mst--section-tag-p (lexeme)
  "Is LEXEME a section tag?"
  (or
   (mst--open-section-tag-p lexeme)
   (mst--inverted-section-tag-p lexeme)
   (mst--close-section-tag-p lexeme)))

(defun mst--open-section-tag-p (lexeme)
  "Is LEXEME an open section tag?
See also `mst--inverted-section-tag-p'."
  (and (mst--tag-p lexeme)
       (s-starts-with-p "#" (second lexeme))))

(defun mst--inverted-section-tag-p (lexeme)
  "Is LEXEME an inverted section tag?"
  (and (mst--tag-p lexeme)
       (s-starts-with-p "^" (second lexeme))))

(defun mst--close-section-tag-p (lexeme)
  "Is LEXEME a close section tag?"
  (and (mst--tag-p lexeme)
       (s-starts-with-p "/" (second lexeme))))

(defun mst--comment-tag-p (lexeme)
  "Is LEXEME a comment tag?"
  (and (mst--tag-p lexeme)
       (s-starts-with-p "!" (second lexeme))))

(defun mst--unescaped-tag-p (lexeme)
  "Is LEXEME an unescaped variable tag?
Note that the lexer converts {{{foo}}} to {{& foo}}."
  (and (mst--tag-p lexeme)
       (s-starts-with-p "&" (second lexeme))))

(defun mst--partial-tag-p (lexeme)
  "Is LEXEME a partial tag?"
  (and (mst--tag-p lexeme)
       (s-starts-with-p ">" (second lexeme))))

(defun mst--section-p (lexeme)
  "Is LEXEME a nested section?"
  (not (atom (car lexeme))))

(defun mst--text-p (lexeme)
  "Is LEXEME plain text?"
  (equal (car lexeme) :text))

;; fixme: assumes the delimeters haven't changed
;; fixme: mst--lex doens't preserve whitespace
(defun mst--unlex (lexemes)
  "Given a lexed (and optionally parsed) list of lexemes,
return the original input string."
  (if lexemes
      (let ((lexeme (first lexemes))
            (rest (cdr lexemes)))
        (cond
         ;; recurse on this section, then the rest
         ((mst--section-p lexeme)
          (concat (mst--unlex lexeme) (mst--unlex rest)))
         ((mst--tag-p lexeme)
          ;; restore the delimeters, then unlex the rest
          (let ((tag-name (second lexeme)))
            (concat "{{" tag-name "}}" (mst--unlex rest))))
         ;; otherwise, it's just raw text
         (t
          (let ((text (second lexeme)))
            (concat text (mst--unlex rest))))))
    ""))
