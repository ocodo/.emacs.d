;;; php-structure.el --- Functions that deal with the structure of a PHP script

;; Version: 1.0
;; Created: 10-03-2011
;; Copyright © 2011 Michael Dwyer
;; Author(s): 
;; Michael Dwyer <mdwyer@ehtech.in>

;;; *****
;;; About
;;; *****

;; php-structure.el is a part of the php+-mode suite and contains
;; convenience functions for dealing with PHP structure, such as
;; determining whether point is within a string and where the
;; beginning of the current statment resides.

;; ********************************************************************

;; ************
;; REQUIREMENTS
;; ************
(require 'cl)
(require 'php-parse)
(require 'php-utils)

;; *********
;; VARIABLES
;; *********

(defvar php-text-struct-cache nil
  "Cache used by ``php-in-text-structp''.")

;; *********
;; FUNCTIONS
;; *********

(defun syntax-inside-text-structp (&optional pos)
  "Uses ``syntax-ppss'' to determine whether point (or optionally
POS) is within a string or comment.  Returns the value of (point)
at the beginning of the text structure."
  (save-excursion
    (elt (syntax-ppss pos) 8)))

(defun php-locate-non-enclosed (reg &optional end skip-sexps)
  "This function returns a list of locations and lengths of REG
that are not inside of a PHP string or enclosure.  REG may be a
list. Stops at the end of the line or END.  If END is an integer,
it will stop at that location.  If END is a regexp it will stop
at the first non-string occurrence of END.  It can be told to
SKIP-SEXP."
  (unless reg (error "Invalid character(s)."))
  (save-excursion
    (save-match-data
      (let ((string-begin (php-in-stringp)))
        (when string-begin
          (goto-char string-begin)))
      (let* ((end (or end (save-excursion (php-skip-this-statement) (point))))
             (reg (if (listp reg) reg `(,reg)))
             (enclosure-start-re (concat "[" (when skip-sexps "[(") "\"'{]"))
             (reg-locs '())
             (regexp (concat "\\(" (mapconcat 'identity reg "\\|") "\\)")))
        (catch 'done
          (while (< (point) (point-max))
            (when (looking-at-p enclosure-start-re)
              (condition-case nil
                  (forward-sexp)
                (error (throw 'done (reverse reg-locs))))
              (backward-char))
            (when (if (integerp end) (>= (point) end) (looking-at-p end))
              (throw 'done (reverse reg-locs)))
            (if (not (looking-at regexp))
                (forward-char)
              (setf reg-locs (cons `(,(point) 
                                     ,(length (match-string-no-properties 0)))
                                   reg-locs))
              (goto-char (match-end 0)))))))))

(defun php-find-struct-position (type name &optional visibility staticp 
                                      abstractp finalp)
  "Return the best position to insert a new struct with the
provided description.  It first looks for a class at point, then
an interface at point, and then finally for the first class found
in the file, then the first interface"
  (save-excursion
    (save-match-data
      (let* ((parse (php-parse-current '(class interface script file)))
             (parse-type (rest (assoc 'type parse))))
        (when (eq 'file parse-type)
          (setq parse (first (rest (assoc 'scripts parse)))))
        (when (eq 'script parse-type)
          (setq parse (first (or (rest (assoc 'classes parse))
                                 (rest (assoc 'interfaces parse))))))
        (when parse
          (let* ((structures (sort (append (rest (assoc 'constants parse))
                                           (rest (assoc 'properties parse))
                                           (rest (assoc 'methods parse)))
                                   'php-parse<))
                 (new-struct `((begin . 0)
                               (end . 0)
                               (text . "")
                               (name . ,name)
                               (type . ,type)
                               (staticp . ,staticp)
                               (finalp . ,finalp)
                               (abstractp . ,abstractp)
                               (visibility . ,visibility)))
                 (begin (or (rest (assoc 'begin 
                                         (catch 'found
                                           (dolist (s structures)
                                             (when (php-parse< new-struct s)
                                               (throw 'found s))))))
                            (1- (rest (assoc 'end parse))))))
            (goto-char begin)
            (when (looking-at-p ws-re)
              (re-search-forward non-ws-re nil t)
              (backward-char))
            (point)))))))

(defun php-find-current-sexp-begin/end-get-args (&optional delimiter
                                                           matching-delimiter)
  "This function gathers arguments for
php-find-current-sexp-{begin,end}."
  (let* ((delimiter (if (consp current-prefix-arg)
                        (read-string "Delimiter ('('): " nil nil 
                                     "(") "("))
         (default-matching 
           (condition-case nil (matching-delimiter delimiter)
             (error nil)))
         (matching-delimiter 
          (if (consp current-prefix-arg)
              (read-string (concat "Matching ('" default-matching
                                   "'): ") nil nil 
                                   default-matching)
            ")")))
    `(,delimiter ,matching-delimiter t)))

(defun php-find-current-sexp-begin/end (which &optional delimiter 
                                              matching-delimiter move-point)
  "This function locates the beginning/ending of the current sexp
starting with DELIMITER.  It defaults to parenthesis.  A
MATCHING-DELIMITER may also be specified if there is no
reasonable default.  If point happens to be on DELIMITER, it is
considered part of its parent sexp.  This function can be told to
MOVE-POINT, unless it is not part of an sexp.  It will do so if
called interactively."
  (let* ((delimiter (or delimiter "("))
         (matching-delimiter (or matching-delimiter
                                 (matching-delimiter delimiter)))
         (delimiter (if (string= delimiter "[") "\\[" delimiter))
         (delimiter-count 0)
         (retval
          (if (and (member delimiter '("(" "{" "["))
                   (string= matching-delimiter (matching-delimiter delimiter)))
              (let (sexp-begin)
                (save-excursion
                  (catch 'found
                    (while (setf sexp-begin (elt (syntax-ppss) 1))
                      (goto-char sexp-begin)
                      (when (looking-at-p delimiter)
                        (if (eq which 'begin)
                            (throw 'found (point))
                          (forward-sexp)
                          (throw 'found (point))))))))
            (save-excursion 
              (catch 'found
                (while (or (and (eq which 'begin) 
                                (> (point) (point-min)))
                           (and (eq which 'end)
                                (< (point) (point-max))))
                  (cond ((eq which 'begin) (backward-char))
                        ((eq which 'end) (forward-char)))
                  (if (and (looking-at-p delimiter)
                           (not (php-in-text-structp)))
                      (setq delimiter-count (1+ delimiter-count))
                    (if (and (looking-at-p matching-delimiter)
                             (not (php-in-text-structp)))
                        (setq delimiter-count (1- delimiter-count))))
                  (when (or (and (eq which 'begin) (= delimiter-count 1))
                            (and (eq which 'end) (= delimiter-count -1)))
                    (throw 'found (point)))))))))
    (when (and move-point (integerp retval)) (goto-char retval)) retval))

(defun php-find-current-sexp-begin (&optional delimiter matching-delimiter 
                                              move-point)
  (interactive (php-find-current-sexp-begin/end-get-args))
  (php-find-current-sexp-begin/end 'begin delimiter matching-delimiter 
                                   move-point))

(defun php-find-current-sexp-end (&optional delimiter matching-delimiter 
                                            move-point)
  (interactive (php-find-current-sexp-begin/end-get-args))
  (php-find-current-sexp-begin/end 'end delimiter matching-delimiter 
                                   move-point))

(defun php-sexp-end-position (&optional pos)
  "Return the position of the end of sexp started by the
character at point or POS."
  (save-excursion
    (when pos (goto-char pos))
    (when parenthesis-stack
      (goto-char (first parenthesis-stack))
      (forward-sexp)
      (1- (point)))))

(defun php-get-current-sexp-level (&optional delimiter matching-delimiter)
  "Return how many sexps deep point is."
  (save-excursion
    (let ((count 0))
      (catch 'done
        (while (> (point) (point-min))
          (if (php-find-current-sexp-begin delimiter matching-delimiter t)
              (setf count (1+ count))
            (throw 'done count))))
      count)))

(defun php-goto-start-of-script/html ()
  "Move point to the beginning of the current script or HTML
block."
  (interactive)
  (let ((begin (php-in-scriptp)))
    (if (integerp begin)
        (goto-char begin)
      (let ((begin (php-in-bare-htmlp)))
        (when (integerp begin)
          (goto-char begin))))))

(defun php-goto-end-of-script/html ()
  "Move point to the beginning of the current script or HTML
block."
  (interactive)
  (or (php-skip-this-script)
      (php-skip-this-bare-html)))

(defun php-in-scriptp ()
  "If point is within a PHP script block, return (point) at the
beginning of the block."
  (let ((parse (php-parse-current 'script)))
    (when (php-parse-p parse)
      (rest (assoc 'begin parse)))))
  
(defun php-skip-this-script ()
  "If in a PHP script block, send point to the end of it."
  (let ((script (php-parse-current 'script)))
    (when script (goto-char (1+ (rest (assoc 'end script)))))))

(defun php-in-bare-htmlp ()
  "If point is not within a PHP script block, return (point) at
the beginning of the non-script section."
  (unless (php-in-scriptp)
    (let ((scripts (rest (assoc 'scripts (php-parse-current 'file))))
          (script-end (point-min)))
      (catch 'too-far
        (dolist (script scripts)
          (let ((cur-end (rest (assoc 'end script))))
            (if (< cur-end (point))
                (setf script-end cur-end)
              (throw 'too-far t)))))
      (1+ script-end))))

(defun php-skip-this-bare-html ()
  "If in bare HTML, send point to the end of it."
  (unless (php-in-scriptp)
    (let ((scripts (rest (assoc 'scripts (php-parse-current 'file))))
          (script-begin (point-max)))
      (catch 'found-it
        (dolist (script scripts)
          (let ((cur-begin (rest (assoc 'begin script))))
            (when (>= cur-begin (point))
              (setf script-begin cur-begin)
              (throw 'found-it t)))))
      (goto-char script-begin))))

(defun php-get-text-type (&optional pos)
  "Return the type of the current text struct (optionally at POS)
or nil if not in one."
  (save-excursion
    (when (and (wholenump pos)
               (>= pos (point-min))
               (<= pos (point-max)))
      (goto-char pos))
    (third (first (member-if (lambda (x) 
                               (and (>= (point) (first x))
                                    (<= (point) (second x))))
                             php-text-struct-cache)))))

(defun php-next-text-struct (&optional bound not-in-string)
  "Goto the next PHP text struct.  Optionally stop at BOUND.  If
``php-text-struct-cache'' is invalid and we are NOT-IN-STRING
tell us so that we may avoid infinite recursion.  Returns the
type."
  (save-match-data
    (let* ((bound (if (integerp bound) bound (point-max)))
           (start-point (point))
           type
           (type (catch 'done
                   (unless not-in-string
                     (when (php-in-text-structp)
                       (php-skip-this-text-struct)))
                   (while (<= (point) bound)
                     (if (not (re-search-forward 
                               "\\?>\\|<<<\\|//\\|/\\*\\|[#'\"]" nil t))
                         (throw 'done nil)
                       (goto-char (match-beginning 0))
                       (if (> (point) bound)
                           (throw 'done nil)
                         (let ((match (match-string-no-properties 0)))
                           (setf type (cond ((string= match "?>") 'bare-html)
                                            ((or (string= match "<<<")
                                                 (string= match "'")
                                                 (string= match "\""))
                                             'string)
                                            ((or (string= match "//")
                                                 (string= match "/*")
                                                 (string= match "#"))
                                             'comment)))
                           (if type
                               (throw 'done type)
                             (goto-char (match-end 0))))))))))
      (unless type
        (goto-char start-point))
      type)))

(defun php-skip-this-text-struct (&optional bound at-beginning)
  "If at the beginning of a text structure (string/comment), skip
to the end and return (point).  Optionally stop at BOUND.  If you
know that ``php-text-struct-cache'' is invalid, make sure you are
AT-BEGINNING and tell us so that we avoid infinite recursion."
  (save-match-data
    (let* ((text-begin (if at-beginning (point) (php-in-text-structp)))
           (text-type (if at-beginning
                          (cond ((looking-at-p "[\"']\\|<<<") 'string)
                                ((looking-at-p "\\?>") 'bare-html)
                                (t 'comment))
                        (php-get-text-type))))
      (when (integerp text-begin) (goto-char text-begin))
      (let ((bound (if (integerp bound) bound (point-max))))
        (when (cond ((or (= (point) (point-min))
                         (looking-at-p "\\?>")
                         (and (looking-at-p ">")
                              (looking-back-p "\\?"))
                         (looking-back-p "\\?>"))
                     (or (when (re-search-forward "<\\?\\(php\\|=\\)?" nil t)
                           (goto-char (match-beginning 0)))
                         (goto-char (point-max))))
                    ((looking-at-p "/\\*")
                     (or (re-search-forward "\\*/" nil t) (goto-char bound)))
                    ((or (looking-at-p "//") (looking-at-p "#")) 
                     (forward-char)
                     (or (when (re-search-forward "\\?>\\|^" nil t)
                           (goto-char (match-beginning 0)))
                         (goto-char bound)))
                    ((looking-at "[\"']")
                     (let* ((quote-string (match-string-no-properties 0))
                            (search-re (concat "[" quote-string "{]")))
                       (forward-char)
                       (or (catch 'done
                             (while (re-search-forward search-re bound t)
                               (if (string= (match-string-no-properties 0) "{")
                                   (when (looking-at-p 
                                          (concat "$" 
                                                  (substring 
                                                   (php-type-regexp 
                                                    'identifier) 
                                                   2) "}"))
                                     (re-search-forward "}" bound t))
                                 (unless (and (char-equal 
                                               (char-before (1- (point))) ?\\)
                                              (not (char-equal 
                                                    (char-before (- (point) 2)) 
                                                    ?\\)))
                                   (throw 'done (point))))))
                           (goto-char bound))))
                    ((looking-at-p "<<<")
                     (forward-char 3)
                     (or (when (looking-at-p non-ws-re)
                           (let ((this-quote nil))
                             (when (looking-at-p "['\"]")
                               (setq this-quote (char-after))
                               (forward-char))
                             (let ((doc-id-start (point)))
                               (re-search-forward "$" nil t)
                               (when (and (characterp this-quote)
                                          (= this-quote (char-before)))
                                 (backward-char))
                               (when (re-search-forward
                                      (concat (buffer-substring-no-properties 
                                               doc-id-start (point)) "[\n;]")
                                      nil t)
                                 (when (char-equal (char-before) ?\;)
                                   (backward-char))
                                 t))))
                         (goto-char bound))))
          (goto-char (min bound (point))))))))

(defun php-text-struct-cache-change-function (begin end old-length)
  "This hook updates ``php-text-struct-cache'' whenever the
buffer is changed."
  (put 'php-text-struct-cache 'valid nil)
  (let* ((change (- (- end begin) old-length))
         found-begin
         (affected (member-if (lambda (x)
                                (let* ((x-type (third x))
                                       (x-beg 
                                        (- (first x) 
                                           (if (eq x-type 'bare-html) 2 0)))
                                       (x-end
                                        (+ (second x)
                                           (if (eq x-type 'bare-html) 2 0))))
                                  (or (> x-beg end)
                                      (integer-range-intersectp 
                                       `(,x-beg ,x-end) `(,begin ,end)))))
                              php-text-struct-cache))
         (unaffected (butlast php-text-struct-cache (length affected))))
    (if affected
      (let ((affected-start (if affected
                                (save-excursion 
                                  (save-match-data
                                    (let ((text-struct-begin (caar affected)))
                                      (goto-char (min begin text-struct-begin))
                                      (if (or (and (looking-back-p "\\?")
                                                   (looking-at-p ">"))
                                              (and (eq 'bare-html 
                                                       (third (car affected)))
                                                   (= (point) 
                                                      text-struct-begin)))
                                          (progn 
                                            (backward-char (min (- (point) 
                                                                   (point-min))
                                                                2))
                                            (point))
                                        (-compensate-for-new-quotes)))))
                              begin)))
        (when (integerp affected-start)
          (let* ((first-affected (first affected))
                 (affected (if (equal (mapcar (lambda (x) (inc-ints x change))
                                              first-affected)
                                      (first (php-scan-for-text-structs 
                                              (+ (second first-affected) change)
                                              affected-start)))
                               (mapcar (lambda (x)
                                         (mapcar (lambda (y) 
                                                   (inc-ints y change)) x))
                                       affected)
                             (php-scan-for-text-structs nil affected-start))))
            (setf php-text-struct-cache (nconc unaffected affected)))))
      (setf php-text-struct-cache 
            (nconc unaffected (php-scan-for-text-structs end begin)))))
  (put 'php-text-struct-cache 'valid t))

(defun -compensate-for-new-quotes ()
  (save-match-data
    (if (or (and (looking-at "/")
                 (or (looking-back "/")
                     (looking-at "//")
                     (looking-at "/\\*")))
            (and (looking-at "\\*")
                 (looking-back "/"))
            (and (looking-at "<")
                 (or (looking-at "<<<")
                     (and (looking-back "<")
                          (looking-at "<<"))
                     (looking-back "<<"))))
        (match-beginning 0)
      (point))))

(defun php-text-struct-cache-initialize ()
  "Initializes ``php-text-struct-cache''."
  (set (make-local-variable 'php-text-struct-cache) (php-scan-for-text-structs))
  (put 'php-text-struct-cache 'valid t)
  (add-hook 'after-change-functions 
            'php-text-struct-cache-change-function nil t))

(defun php-text-struct-cache-deactivate ()
  (remove-hook 'after-change-functions 'php-text-struct-cache-change-function 
               t))

(defun php-text-struct-cache-clear ()
  "Clears ``php-text-struct-cache''."
  (setf php-text-struct-cache nil))

(defun php-text-struct-cache-insert (string-stats &optional at-end)
  "Inserts a STRING-STATS '(start end type) into the
``php-text-struct-cache'' in the appropriate place.  Returns the
position that STRING-STATS was inserted at.  If you know you will
be adding to the end of the cache, apecifying AT-END will improve
performance."
  (let* ((cache-len (length php-text-struct-cache)))
    (if (not at-end)
        (let* ((pos (catch 'new-pos
                      (dotimes (i cache-len)
                        (let* ((cur-string-stats (elt php-text-struct-cache i))
                               (start (first cur-string-stats))
                               (end (second cur-string-stats)))
                          (when (or (and (>= (first string-stats) start) 
                                         (<= (first string-stats) end))
                                    (and (>= (second string-stats) start) 
                                         (<= (second string-stats) end)))
                            (error (concat "String-Stats %s intersects already "
                                           "existing string-stats %s")
                                           string-stats cur-string-stats)
                            (when (> start (first string-stats)) 
                              (throw 'new-pos i)))))))
               (pos (or pos cache-len)))
          (setf php-text-struct-cache (append (butlast php-text-struct-cache
                                                       (- cache-len pos))
                                              `(,string-stats)
                                              (nthcdr pos 
                                                      php-text-struct-cache)))
          pos)
      (setf php-text-struct-cache 
            (append php-text-struct-cache `(,string-stats)))
      cache-len)))

(defun php-text-struct-cachep (pos)
  "Check to see whether POS is in a string in
``php-text-struct-map''.  Returns the bounds of the string."
  (first (member-if (lambda (string-stats)
                      (and (>= pos (first string-stats))
                           (<= pos (second string-stats)))) 
                    php-text-struct-cache)))

(defun php-in-text-structp (&optional type pos end)
  "TYPE may be a list of types or nil.  If nil, all types
  '(string comments) will be tried.  The beginning and any end
  delimiters will be considered part of the struct.  Returns
  point at the beginning of the struct or nil.  If POS is an
  integer, start there.  If END is provided, return true if any
  character from POS to END qualifies.  This function relies on
  ``php-text-struct-cache''!"
  (unless (get 'php-text-struct-cache 'valid)
    (php-text-struct-cache-initialize))
  (let* ((pos (if (integerp pos) pos (point)))
         (end (if (and (integerp end) (>= end pos)) end pos))
         (type (cond ((consp type) type)
                     ((not type) '(string comment bare-html))
                     (t `(,type)))))
    (catch 'done
      (dotimes (i (- (1+ end) pos))
        (let ((match (php-text-struct-cachep (+ pos i))))
          (when (and match (member (third match) type))
            (throw 'done (first match))))))))

(defun php-scan-for-text-structs (&optional bound start)
  "Scan the buffer for text structs, optionally up to BOUND,
returning a structure like ``php-text-struct-cache''.  If you
want parsing to begin at START rather than (point-min), you may
supply it."
  (save-excursion
    (save-restriction
      (widen)
      (let (php-text-struct-cache 
            (bound (if (wholenump bound) bound (point-max)))
            (start (if (wholenump start) start (point-min))))
        (goto-char start)
        (let* ((in-script (save-excursion
                            (save-match-data
                              (when (looking-at (concat non-ws-re "+"))
                                (goto-char (match-end 0)))
                              (when (re-search-backward "<\\?" nil t)
                                (match-beginning 0)))))
               (next-script (unless in-script 
                              (re-search-forward "<\\?" nil t))))
          (unless in-script
            (let ((struct-end (min bound 
                                   (if next-script 
                                       (1- next-script) (point-max)))))
              (setf php-text-struct-cache `((,start ,struct-end bare-html)))
              (goto-char struct-end)))
          (catch 'done
            (let ((last-point -1)
                  in-struct)
              (while (<= (point) bound)
                (when (<= (point) last-point)
                  (error (concat "Infinite loop detected in "
                                 "php-scan-for-text-structs.")))
                (setf last-point (point))
                (setq in-struct nil)
                (let ((this-type 
                       (cond ((looking-at-p "//\\|/\\*\\|#") 'comment)
                             ((looking-at-p (concat "['\"]\\|" 
                                                    doc-begin-tag-re)) 
                              'string)
                             ((looking-at-p "\\?>") 'bare-html)
                             (t (php-next-text-struct bound t)))))
                  (if (not this-type)
                      (throw 'done nil)
                    (setq in-struct 
                          (when (<= (point) bound)
                            (if (eq this-type 'bare-html) 
                                (+ (point) 2) 
                              (point))))
                    (let* ((string-end (php-skip-this-text-struct nil t))
                           (string-end (if (= (point-max) string-end) 
                                           string-end
                                         (1- string-end))))
                      (setf php-text-struct-cache 
                            (nconc php-text-struct-cache 
                                   `((,in-struct ,string-end ,this-type)))))
                    (when (or (= (point) (point-max))
                              (> (point) bound))
                      (throw 'done in-struct))))))))
        php-text-struct-cache))))
  
(defun php-inside-text-structp (&optional type pos end)
  "Return t if point is inside a text struct (optionally of TYPE
at POS), not on the delimiters (quotes, etc).  If END is
provided, return true if any character from POS to END
qualifies."
  (let* ((pos (if (integerp pos) pos (point)))
         (begin (php-in-text-structp type pos end)))
    (when begin
      (let* ((inside-begin (+ begin 
                              (save-excursion
                                (goto-char begin)
                                (save-match-data
                                  (cond ((looking-at doc-begin-tag-re)
                                         (- (match-end 0) (match-beginning 0)))
                                        ((looking-at-p "\\?>") 2)
                                        (t 1))))))
             (inside-end (save-excursion 
                           (- (php-skip-this-text-struct)
                              (save-match-data
                                (cond ((or (looking-back 
                                            (substring doc-end-tag-re 0 -8))
                                           (looking-back "<\\?\\(php\\|=\\)?"))
                                       (1+ (- (match-end 0) 
                                              (match-beginning 0))))
                                      (t 2)))))))
        (and (>= pos inside-begin)
             (<= pos inside-end))))))

(defun php-in-stringp (&optional pos end)
  "The beginning and end quotes and {here,now}doc tags will be
  considered part of the string.  Returns point at the beginning
  of the string or nil.  If END is provided, return true if any
  character from POS to END qualifies."
  (php-in-text-structp 'string pos end))

(defun php-in-commentp (&optional pos end)
  "The beginning and end delimiters will be considered part of
  the comment.  Returns point at the beginning of the comment or
  nil.  If END is provided, return true if any character from POS
  to END qualifies."
  (php-in-text-structp 'comment pos end))

(defconst php-possible-num-chars "-+.0-9xa-fA-F")

(defun php-in-numberp (&optional pos)
  "Returns point at the beginning of the number or nil."
  (save-excursion
    (save-match-data
      (when (integerp pos)
        (goto-char pos))
      (when (not (php-in-text-structp))
        (when (looking-at-p (concat "[" php-possible-num-chars "]"))
          (re-search-backward (concat "[^" php-possible-num-chars "]") nil t))
        (when (looking-at-p ws-re)
          (forward-char)
          (when (looking-at-p (php-type-regexp 'number))
            (point)))))))

(defconst php-definite-identifier-chars "][_a-zA-Z0-9-ÿ")
(defconst php-possible-identifier-chars "$<?:->!\\+[:space:]\n()\\.")

(defun php-in-identifierp (&optional pos)
  "Returns point at the beginning of the identifier or nil."
  (save-excursion
    (when (integerp pos)
      (goto-char pos))
    (let* ((start (point))
           (accessor-re (php-type-regexp 'accessor))
           (begin (catch 'done
                    (when (looking-at-p "<\\?\\(php\\|=\\)?")
                      (throw 'done (point)))
                    (while (> (point) (point-min))
                      (let ((in-string (php-in-stringp)))
                        (when in-string (goto-char (1- in-string)))
                        (if (looking-at-p (concat "[" 
                                                  php-definite-identifier-chars 
                                                  "]"))
                            (backward-char)
                          (if (looking-at-p 
                               (concat "[^" php-possible-identifier-chars "]"))
                              (throw 'done (point))
                            (cond ((looking-at-p "!")
                                   (throw 'done (point)))
                                  ((looking-at-p "\\$")
                                   (if (looking-back-p (concat "\\(->\\|::\\)"
                                                               ws-re "*"))
                                       (backward-char)
                                     (throw 'done (point))))
                                  ((looking-at "\\.")
                                   (if (php-in-numberp)
                                       (backward-char)
                                     (throw 'done (1+ (point)))))
                                  ((looking-at-p "\\+")
                                   (if (looking-back-p "\\+")
                                       (throw 'done (1- (point)))
                                     (if (looking-back-p 
                                          (concat "[" 
                                                  php-definite-identifier-chars
                                                  "]"))
                                         (throw 'done (1+ (point)))
                                       (throw 'done (point)))))
                                  ((looking-at-p ":")
                                   (if (looking-at-p "::")
                                       (backward-char)
                                     (if (looking-back-p ":")
                                         (backward-char 2)
                                       (throw 'done (1+ (point))))))
                                  ((looking-at-p "-")
                                   (if (looking-at-p "->")
                                       (backward-char)
                                     (if (looking-back-p "-")
                                         (throw 'done (1- (point)))
                                       (if (looking-back-p 
                                            php-definite-identifier-chars)
                                           (throw 'done (1+ (point)))
                                         (throw 'done (point))))))
                                  ((looking-at-p "<\\?\\(php\\|=\\)?")
                                   (throw 'done (point)))
                                  ((looking-at-p "\\?")
                                   (if (looking-back-p "<")
                                       (throw 'done (1- (point)))
                                     (backward-char)))
                                  ((looking-at-p ">")
                                   (if (or (looking-back-p "-")
                                           (looking-back-p "?"))
                                       (backward-char 2)
                                     (throw 'done (1+ (point)))))
                                  ((looking-at-p ws-re)
                                   (let ((pos (point)))
                                     (save-match-data
                                       (re-search-backward non-ws-re nil t))
                                     (forward-char)
                                     (if (and 
                                          (or 
                                           (not 
                                            (looking-at-p 
                                             (concat ws-re "*" accessor-re)))
                                           (save-excursion
                                             (backward-char)
                                             (looking-at-p 
                                              (concat 
                                               "[^"
                                                php-definite-identifier-chars
                                               (substring 
                                                php-possible-identifier-chars 9)
                                               "]"))))
                                          (not (or (looking-back-p "->")
                                                   (looking-back-p "::")
                                                   (looking-back-p "new"))))
                                         (throw 'done (1+ pos))
                                       (backward-char))))
                                  ((looking-at-p "(")
                                   (backward-char))
                                  ((looking-at-p ")")
                                   (forward-char)
                                   (backward-sexp)
                                   (backward-char))
                                  (t (throw 'done (1+ (point))))))))))))
      (when (and (integerp begin) (<= begin start))
        begin))))

(defun php-skip-this-identifier (&optional bound ignore-in-string)
  "If at the beginning of a identifier, skip to the end.
Optionally stop at BOUND or IGNORE-IN-STRING.  Returns (point) at
the end."
  (save-match-data
    (let ((start (point)))
      (if (and (not ignore-in-string) (php-in-text-structp))
          (php-skip-this-text-struct)
        (when (looking-at 
               (concat "\\(\\(\\+\\+?\\|--?\\|!\\)[(a-zA-Z_-ÿ$]\\)\\|\\$\\|)"
                       "\\|<?\\?>?"))
          (goto-char (match-end 0)))
        (let ((re (concat "\\(?99:[^]a-zA-Z0-9_-ÿ]\\)\\|"
                          "\\(?99:" doc-begin-tag-re "\\)\\|"  
                          "\\(?99:-[^>]\\)\\|\\([^:]\\(?99::\\)[^:]\\)\\|"
                          "\\(?:[^>:[:space:]\n]\\(?99:" ws-re 
                          "+[^-[:space:]\n:]\\)\\)\\|"
                          "\\(?:[^>:[:space:]\n]\\(?99:" ws-re 
                          "+-[^>]\\)\\)\\|"
                          "\\(?:[^>:[:space:]\n]\\(?99:" ws-re 
                          "+:[^:]\\)\\)\\|\\(?99:[^-]>" ws-re "+\\)\\|"
                          "\\(?99:[^:]:" ws-re "+\\)\\|"
                          "\\(?99:" ws-re "+[^-[:space:]\n:]\\)\\|"
                          "\\(?99:" ws-re "+-[^>]\\)\\|"
                          "\\(?99:" ws-re "+:[^:]\\)"))
              (current-sexp (php-get-current-sexp-level)))
          (catch 'done
            (while (re-search-forward re nil t)
              (goto-char (match-beginning 99))
              (when (looking-at-p ws-re)
                (save-match-data
                  (re-search-backward non-ws-re nil t)
                  (forward-char)))
              (save-match-data
                (when (and (looking-back-p "function")
                           (looking-at (concat ws-re "*\\(" 
                                               (php-type-regexp 'identifier)
                                               "\\)?([^)]*)" ws-re "*{")))
                  (goto-char (1- (match-end 0)))
                  (setf start (point))))
              (if (and (looking-at-p ")")
                       (> (php-get-current-sexp-level) current-sexp))
                  (forward-char)
                (catch 'parenthesis
                  (while (looking-at-p "[[({]")
                    (when (and (looking-at-p "{")
                               (not (= (point) start)))
                      (throw 'parenthesis t))
                    (condition-case nil
                        (forward-sexp)
                      (error (throw 'done (1+ (point)))))
                    (when (looking-back-p ")")
                      (throw 'parenthesis t)))))
              (if (looking-at (concat ws-re "*" "\\(->\\|::\\)"))
                  (goto-char (match-end 0))
                (throw 'done (when (> (point) start) (point)))))))))))

(defun php-in-variablep (&optional stand-alone)
  "When inside a variable, return point at the beginning of the
variable.  Optionally, the variable must STAND-ALONE, and not be
part of any function calls."
  (save-excursion
    (let ((begin (php-in-identifierp)))
      (when (integerp begin)
        (goto-char begin)
        (let ((end (save-excursion (php-skip-this-identifier))))
          (when (looking-at-p "\\$")
            (if (not stand-alone)
                begin
              (catch 'found-end
                (while (<= (point) end)
                  (forward-char)
                  (when (looking-at-p ws-re)
                    (throw 'found-end begin))
                  (unless (looking-at-p (php-type-regexp 'identifier))
                    (throw 'found-end nil)))))))))))

(defun php-in-method-call ()
  "When in a method call, return t."
  (and (> (php-get-current-sexp-level) 0)
       (not (php-function-definitionp))
       (not (php-in-control-statementp))))

(defun php-anonymous-function-definitionp ()
  "When in an anonymous function definition return point at the
beginning of the definition."
  (php-function-definitionp t))

(defun php-named-function-definitionp ()
  "When in a named function definition return point at the
beginning of the definition."
  (and (not (php-anonymous-function-definitionp))
       (php-function-definitionp)))

(defun php-class/interface-definitionp ()
  "When inside a class/interface definition return point at the
beginning of the definition."
  (let ((statement-begin (php-in-statementp)))
    (when (integerp statement-begin)
      (save-excursion
        (goto-char statement-begin)
        (looking-at-p (concat "\\(\\(final\\|abstract\\)" ws-re 
                              "\\)*class\\|interface"))))))

(defun php-function-definitionp (&optional anonymousp)
  "When inside a function definition return point at the
beginning of the definition.  Optionally, return nil unless the
definition is ANONYMOUSP."
  (save-excursion
    (save-match-data
      (let ((start (point)))
        (end-of-line)
        (when (looking-back "{[[:space:]]*")
          (goto-char (1- (match-beginning 0))))
        (let ((too-far (or (save-excursion 
                             (catch 'found
                               (beginning-of-line)
                               (while (re-search-backward "[{;]\\|\\*/" nil t)
                                 (when (not (php-in-text-structp))
                                   (throw 'found (point))))))
                           (point-min))))
          (when (wholenump too-far)
            (let ((def-begin 
                    (catch 'found
                      (while (re-search-backward-greedy 
                              (concat "\\(\\(public\\|private\\|protected"
                                      "\\|static\\|abstract\\)" ws-re 
                                      "*\\)*function\\(" ws-re "*("
                                      (unless anonymousp 
                                        (concat "\\|" ws-re "+" 
                                                (substring 
                                                 (php-type-regexp 'identifier) 
                                                 2) ws-re "*("))
                                      "\\)")
                              too-far t)
                        (unless (php-in-text-structp)
                          (throw 'found (point)))))))
              (when (and (wholenump def-begin)
                         (<= def-begin start))
                def-begin))))))))
  
(defun php-in-statementp (&optional pos)
  "Returns point at the beginning of the statement or nil."
  (save-excursion
    (save-match-data
      (when (integerp pos)
        (goto-char pos))
      (let* ((starting-point (point))
             (starting-block-level (php-get-current-sexp-level "{"))
             (in-comment (php-in-commentp))
             (in-start-tag (save-excursion
                             (cond ((looking-at-p non-ws-re)
                                    (re-search-forward ws-re nil t)
                                    (backward-char))
                                   (t (re-search-backward non-ws-re nil t)
                                      (forward-char)))
                             (when (looking-back "<\\?\\(php\\|=\\)?")
                               (match-beginning 0))))
             (in-bare-html (unless (or in-comment in-start-tag)
                             (let ((start (php-in-bare-htmlp)))
                               (cond ((integerp start)
                                      (- start 2))
                                     ((and (> (point) (point-min))
                                           (char-equal (char-before) ??)
                                           (char-equal (char-after) ?>))
                                      (1- (point)))
                                     ((looking-at-p "\\?>") (point)))))))
        (cond ((integerp in-start-tag) in-start-tag)
              ((integerp in-comment) in-comment)
              ((integerp in-bare-html) in-bare-html)
              (t (catch 'found
                   (unless (or (= (point) (point-max))
                               (looking-at-p "}"))
                     (while (re-search-backward 
                             "[:;{}#]\\|\\*/\\|//\\|<\\?\\(php\\|=\\)?" nil t)
                       
                       (unless (php-inside-text-structp)
                         (let ((match (match-string-no-properties 0)))
                           (when (and (string= match "<?")
                                      (looking-at "<\\?\\(php\\|=\\)?"))
                             (setf match (match-string-no-properties 0)))
                           (when (cond ((string= match "*/")
                                        (goto-char (+ 2 (point))))
                                       ((or (string= match "//") 
                                            (string= match "#"))
                                        (forward-line))
                                       ((or (string= match "<?php")
                                            (string= match "<?=")
                                            (string= match "<?"))
                                        (goto-char (+ (length match) (point))))
                                       ((not (php-in-text-structp))
                                        (goto-char (1+ (point)))))
                             (let 
                                 ((skip 
                                   (or 
                                    (when (looking-back-p ";")
                                      (php-in-control-statementp "for" t))
                                    (when (or (looking-back-p "}")
                                              (looking-back-p "{"))
                                      (save-excursion
                                        (cond ((looking-back-p "}") 
                                               (backward-sexp))
                                              ((looking-back-p "{") 
                                               (backward-char)))
                                        (when 
                                            (> (save-excursion
                                                 (forward-char)
                                                 (php-get-current-sexp-level "{"))
                                                 starting-block-level)
                                          (php-anonymous-function-definitionp))))
                                    (when (looking-back-p ":")
                                      (cond ((looking-back-p "::")
                                             (- (point) 2))
                                            ((or (and (looking-back-p ":") 
                                                      (looking-at-p ":"))
                                                 (save-excursion
                                                   (beginning-of-line-non-whitespace)
                                                   (not 
                                                    (or (looking-at-p 
                                                         "case ")
                                                        (looking-at-p 
                                                         "default:")))))
                                             (1- (point))))))))
                               (if skip
                                   (goto-char skip)
                                 (if (not (re-search-forward non-ws-re nil t))
                                     (throw 'found nil)
                                   (backward-char)
                                   (while (php-in-commentp)
                                     (php-skip-this-text-struct)
                                     (re-search-forward non-ws-re nil t)
                                     (backward-char))
                                   (if (< starting-point (point))
                                       (throw 'found nil)
                                     (throw 'found (point))))))))))))))))))

(defun php-skip-this-statement (&optional bound)
  "Skip to the end of the current PHP statement.  If not
currently in a statement, will skip ahead to the next statement.
Optionally stop at BOUND."
  (save-match-data
    (if (and (looking-at-p "{")
             (php-function-definitionp)
             (not (php-anonymous-function-definitionp)))
        (goto-char (1+ (point)))
      (let ((bound (if (integerp bound) bound (point-max)))
            (in-comment (php-in-commentp))
            (identifier-end (php-skip-this-identifier)))
        (if (or (integerp in-comment)
                (looking-back-p "<\\?\\(php\\|=\\)?")
                (and (looking-back-p "}") (not (php-in-statementp))))
            identifier-end
          (let (in-string 
                (statement-begin (or (php-in-statementp)
                                     (save-excursion
                                       (re-search-forward non-ws-re nil t)))))
            (if statement-begin
                (let* ((case-statement-begin 
                        (or (and (<= (+ 5 statement-begin) (point-max))
                                 (string= "case " 
                                          (buffer-substring-no-properties 
                                           statement-begin 
                                           (+ 5 statement-begin))))
                            (and (<= (+ 8 statement-begin) (point-max))
                                 (string= "default:" 
                                          (buffer-substring-no-properties 
                                           statement-begin 
                                           (+ 8 statement-begin))))))
                       (statement-end-char (if case-statement-begin ":" ";"))
                       (enclosure-open (php-find-current-sexp-begin "{"))
                       (in-enclosure (if (and (integerp enclosure-open) 
                                              (> enclosure-open 
                                                 statement-begin)) 
                                         1 0)))
                  (min bound
                       (or (catch 'done
                             (while (re-search-forward 
                                     (concat "[" statement-end-char "'\"{}]") 
                                     nil t)
                               (backward-char)
                               (if (php-in-text-structp)
                                   (php-skip-this-text-struct)
                                 (when (looking-at-p ";")
                                   (forward-char)
                                   (unless (or (> in-enclosure 0) 
                                               (php-in-control-statementp "for" 
                                                                          t))
                                     (throw 'done (point))))
                                 (when (looking-at-p "{")
                                   (if (or (php-anonymous-function-definitionp) 
                                           (not (zerop in-enclosure)))
                                       (setf in-enclosure (1+ in-enclosure))
                                     (forward-char)
                                     (throw 'done (point))))
                                 (when (looking-at-p "}")
                                   (setf in-enclosure (1- in-enclosure)))
                                 (forward-char)
                                 (when (and (looking-back-p statement-end-char)
                                            (not (looking-back-p "::"))
                                            (not (looking-at-p ":"))
                                            (<= in-enclosure 0))
                                   (throw 'done (point))))) bound))))
              identifier-end)))))))

(defun php-is-control-keyword (key)
  "Returns t if KEY is a PHP control structure keyword."
  (or (member key php-block-stmt-1-kwds)
      (member key php-block-stmt-2-kwds)))

(defun php-in-control-statementp (&optional type current-sexp)
  "Return the value of (point) at the beginning of the current
control statement, if in one.  Optionally, specify a string TYPE
or list of TYPEs or only check the CURRENT-SEXP."
  (let ((types (cond ((not type) (append php-block-stmt-1-kwds 
                                         php-block-stmt-2-kwds))
                     ((listp type) type)
                     (type `(,type)))))
    (save-excursion
      (save-match-data
        (or (looking-at-p "(") (php-find-current-sexp-begin nil nil t))
        (or current-sexp (while (php-find-current-sexp-begin nil nil t)))
        (when (looking-at-p "(")
          (backward-char))
        (when (looking-at-p non-ws-re)
          (re-search-forward (concat ws-re "\\|(") nil t)
          (backward-char))
        (catch 'found
          (dolist (type types)
            (let ((look-for (concat ws-re type ws-re "*")))
              (when (looking-back look-for)
                (throw 'found (1+ (match-beginning 0)))))))))))

(defun php-skip-this-atom (&optional bound)
  "Skip the current PHP atom.  Optionally stop at BOUND."
  (let ((bound (if (integerp bound) bound (point-max)))
        (bounds (php-atom-bounds)))
    (when bounds
      (goto-char (second bounds))
      (when (> (point) bound)
        (goto-char bound)))))

(defun php-skip-this-number (&optional bound)
  "If at the beginning of a number, skip to the end.  Optionally
stop at BOUND."
  (save-match-data
    (let ((bound (if (integerp bound) bound (point-max)))
          (type nil))
      (when (looking-at-p "[-+.0-9]")
        (re-search-forward (concat "[^" php-possible-num-chars "]") nil t)
        (backward-char 2)
        (while (looking-at-p "[^0-9]")
          (backward-char))
        (forward-char)
        (if (> (point) bound)
          (goto-char bound)
          (point))))))

(defun php-beginning-of-chain ()
  "Return the value of (point) at the beginning of the current
chain."
  (save-excursion
    (save-match-data
      (let ((begin (php-in-identifierp)))
        (catch 'done
          (while begin
            (goto-char begin)
            (backward-char 2)
            (if (not (looking-at-p "->"))
                (throw 'done begin)
              (backward-char)
              (when (looking-at-p ws-re)
                (re-search-backward non-ws-re nil t))
              (when (looking-at-p "[])]")
                (forward-char)
                (backward-sexp)
                (backward-char))
              (setf begin (php-in-identifierp))
              (when begin (goto-char begin)))))))))

(defun php-beginning-of-concat (&optional statement-begin)
  "Return the value of (point) at the beginning of the current
concatenation.  Optionally, pass in STATEMENT-BEGIN for speed
improvement."
  (let ((statement-begin (if (integerp statement-begin) 
                             statement-begin 
                           (php-in-statementp))))
    (when (integerp statement-begin)
      (save-excursion
        (save-match-data
          (or (catch 'found
                (while (>= (point) statement-begin)
                  (goto-char (1- (or (php-in-stringp) (1+ (point)))))
                  (when (< (point) statement-begin)
                    (throw 'found (1- (re-search-forward non-ws-re nil t))))
                  (if (or (looking-at-p ws-re)
                          (looking-at-p "\\."))
                      (re-search-backward non-ws-re statement-begin t)
                    (let ((identifier-begin (php-in-identifierp)))
                      (if (or (not (integerp identifier-begin))
                              (and (save-excursion
                                     (forward-char)
                                     (not (looking-at-p (concat ws-re "*\\."))))
                                   (save-excursion
                                     (goto-char identifier-begin)
                                     (not (looking-back-p (concat "\\." ws-re
                                                                  "*"))))))
                          (throw 'found (save-excursion
                                          (forward-char)
                                          (1- (re-search-forward non-ws-re nil 
                                                                 t))))
                        (goto-char (1- identifier-begin)))))))
              statement-begin))))))

(defun php-end-of-concat ()
  "Return the value of (point) at the end of the current
concatenation."
  (save-excursion
    (save-match-data
      (php-skip-this-identifier)
      (catch 'found
        (while (< (point) (point-max))
          (let (current-atom-bounds)
            (while (not current-atom-bounds)
              (if (looking-at-p "(")
                  (forward-sexp)
                (if (not (looking-at (concat ws-re "*\\." ws-re "*")))
                    (throw 'found (point))
                  (goto-char (match-end 0))
                  (setq current-atom-bounds (php-atom-bounds)))))
            (goto-char (second current-atom-bounds))))))))

(defun php-skip-this-concat ()
  "Skip over the current PHP concatenation."
  (let ((end (php-end-of-concat)))
    (when (integerp end)
      (goto-char end))))

(defun php-atom-bounds ()
  "Return the position of the beginning of the current PHP atom."
  (save-excursion
    (let (type begin end)
      (cond ((or (integerp (setq type 'text begin (php-in-text-structp)))
                 (integerp (setq type 'number begin (php-in-numberp)))
                 (integerp (setq type 'identifier begin (php-in-identifierp))))
             (setq end (cond ((eq type 'text) (php-skip-this-text-struct))
                             ((eq type 'number) (php-skip-this-number))
                             ((eq type 'identifier) 
                              (php-skip-this-identifier))))))
      (when (and (integerp begin) (integerp end)) `(,begin ,end)))))

(defun php-get-ternary-bounds ()
  "Returns the bounds of the ternary expression that point is in."
  (save-excursion
    (save-match-data
      (let (begin end q-pos c-pos)
        (catch 'done
          (while (not (and begin end))
            (cond ((looking-at-p "\\?")
                   (setf q-pos (point))
                   (save-excursion
                     (re-search-backward non-ws-re nil t)
                     (setf begin (first (php-atom-bounds))))
                   (unless end
                     (catch 'found
                       (let ((end (save-excursion 
                                    (php-skip-this-statement) 
                                    (point))))
                         (while (< (point) end)
                           (re-search-forward ":" end t)
                           (unless (php-in-stringp)
                             (backward-char)
                             (throw 'found t)))))))
                  ((looking-at-p ":")
                   (setf c-pos (point))
                   (save-excursion
                     (forward-char)
                     (re-search-forward non-ws-re nil t)
                     (backward-char)
                     (setf end (second (php-atom-bounds))))
                   (unless begin
                     (catch 'found
                       (let ((begin (php-in-statementp)))
                         (while (> (point) begin)
                           (re-search-backward "\\?" begin t)
                           (unless (php-in-stringp)
                             (throw 'found t)))))))
                  (t (let* ((bounds (php-atom-bounds))
                            (p1 (save-excursion
                                  (goto-char (first bounds))
                                  (re-search-backward non-ws-re nil t)
                                  (when (or (looking-at-p "\\?")
                                            (looking-at-p ":"))
                                    (point))))
                            (p2 (save-excursion
                                  (goto-char (second bounds))
                                  (re-search-forward non-ws-re nil t)
                                  (when (or (looking-back-p "\\?")
                                            (looking-back-p ":"))
                                    (1- (point)))))
                            (p3 (or p1 p2)))
                       (if p3 (goto-char p3) (throw 'done nil)))))))
        (when (and begin end) `(,begin ,end ,q-pos ,c-pos))))))

(defun php-structure-test-text-struct ()
  (save-excursion
    (let ((begin (php-in-text-structp)))
      (when (integerp begin)
        (goto-char begin)
        (sit-for 1)
        (php-skip-this-text-struct)
        (sit-for 1)))))

(defun php-structure-test-statement ()
  (save-excursion
    (let ((begin (php-in-statementp)))
      (when (integerp begin)
        (goto-char begin)
        (sit-for 1)
        (php-skip-this-statement)
        (sit-for 1)))))

(defun php-structure-test-identifier ()
  (save-excursion
    (let ((begin (php-in-identifierp)))
      (when (integerp begin)
        (goto-char begin)
        (sit-for 1)
        (php-skip-this-identifier)
        (sit-for 1)))))

(defun php-structure-test-atom ()
  (save-excursion
    (let ((bounds (php-atom-bounds)))
      (when bounds
        (goto-char (first bounds))
        (sit-for 1)
        (goto-char (second bounds))
        (sit-for 1)))))

(provide 'php-structure)
