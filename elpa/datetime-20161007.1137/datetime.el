;;; datetime.el --- Parsing, formatting and matching timestamps  -*- lexical-binding: t -*-

;; Copyright (C) 2016 Paul Pogonyshev

;; Author:     Paul Pogonyshev <pogonyshev@gmail.com>
;; Maintainer: Paul Pogonyshev <pogonyshev@gmail.com>
;; Version:    0.2
;; Package-Version: 20161007.1137
;; Keywords:   lisp, i18n
;; Homepage:   https://github.com/doublep/datetime
;; Package-Requires: ((emacs "24.1"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses.


;;; Commentary:

;; Library that provides support for formatting, parsing and matching
;; timestamps in certain format.


;;; Code:


;; Internally any date-time pattern is parsed to a list of value pairs
;; (type . details).  Type is a symbol, while details are either nil,
;; another symbol or a number that represents minimum number of
;; characters in formatted number (left padded with zeros).  The only
;; exception is "as-is" part: it is just a string, not a cons cell.
;;
;; Here are all currently used types with details in parentheses,
;; grouped roughly by represented date-time value.  Context/standalone
;; is meaningful for languages that involve flexing, for English they
;; are the same.
;;
;; In all cases these should be seen as internals and can be changed
;; in a future library versions without prior notice.
;;
;;   era (full | abbreviated) --- AD or BC
;;
;;   year (add-century-when-parsing | always-two-digits | NUMBER)
;;     - add-century-when-parsing: format as-is, but when parsing add
;;       century if exactly two digits;
;;   year-for-week (same as for year)
;;
;;   month (NUMBER)
;;   month-context-name (full | abbreviated)
;;   month-standalone-name (full | abbreviated)
;;
;;   week-in-year (NUMBER)
;;   week-in-month (NUMBER)
;;
;;   day-in-year (NUMBER)
;;   day-in-month (NUMBER)
;;   weekday-in-month (NUMBER)
;;       e.g. would be 2 for 2015-09-09, because it is the second
;;       Wednesday that month;
;;   weekday (NUMBER)
;;   weekday-context-name (full | abbreviated)
;;   weekday-standalone-name (full | abbreviated)
;;
;;   am/pm (full | abbreviated)
;;
;;   hour-0-23 (NUMBER)
;;   hour-1-24 (NUMBER)
;;   hour-am/pm-0-11 (NUMBER)
;;   hour-am/pm-1-12 (NUMBER)
;;
;;   minute (NUMBER)
;;   second (NUMBER)
;;   millisecond (NUMBER)
;;   second-fractional (NUMBER)
;;       this is a generalization used internally: (second-fractional . 3)
;;       means millis, (second-fractional . 6) -- micros, and so on.
;;
;;   timezone (?) -- currently not supported further than pattern parsing


(define-error 'datetime-unsupported-timezone "Timezones are currently not supported")


;; Set at the end of this file because the initializer is huge.
(defvar datetime--locale-data nil
  "Don't access directly, use `datetime-locale-field' instead.")

(defvar datetime--pattern-parsers '((parsed . (lambda (pattern options) pattern))
                                    (java   . datetime--parse-java-pattern)))

(defvar datetime--pattern-formatters '((parsed . (lambda (parts options) parts))
                                       (java   . datetime--format-java-pattern)))


(defun datetime--parse-pattern (type pattern options)
  (let ((parser (cdr (assq type datetime--pattern-parsers))))
    (if parser
        (funcall parser pattern options)
      (error "Unknown pattern type `%s'" type))))

(defun datetime--format-pattern (type parts options)
  (let ((formatter (cdr (assq type datetime--pattern-formatters))))
    (if formatter
        (funcall formatter parts options)
      (error "Unknown pattern type `%s'" type))))


;; Appending character-by-character is slow, but pretty sure it
;; doesn't matter for generally short date-time patterns.
(defmacro datetime--extend-as-is-part (parts text)
  `(let ((text ,text))
     (if (stringp (car ,parts))
         (setcar parts (concat (car ,parts) text))
       (push text ,parts))))


(defun datetime--parse-java-pattern (pattern options)
  (let ((scan   0)
        (length (length pattern))
        parts)
    (while (< scan length)
      (let ((character       (aref pattern scan))
            (num-repetitions 1))
        (setq scan (1+ scan))
        (cond ((= character ?')
               (when (= scan length)
                 (error "Unterminated quote"))
               ;; Ugly code to parse single-quoted string.
               (if (= (aref pattern scan) ?')
                   (progn
                     (datetime--extend-as-is-part parts "'")
                     (setq scan (1+ scan)))
                 (while (progn
                          (when (= scan length)
                            (error "Unterminated quote"))
                          (setq character (aref pattern scan)
                                scan      (1+ scan))
                          (if (/= character ?')
                              (datetime--extend-as-is-part parts (string character))
                            (when (and (< scan length) (= (aref pattern scan) ?'))
                              (datetime--extend-as-is-part parts (string ?'))
                              (setq scan (1+ scan))))))))
              ((or (and (<= ?A character) (<= character ?Z))  (and (<= ?a character) (<= character ?z)))
               (while (and (< scan length) (eq (aref pattern scan) character))
                 (setq scan            (1+ scan)
                       num-repetitions (1+ num-repetitions)))
               (pcase character
                 ((or ?G ?E ?a)
                  (push (cons (pcase character
                                (?G 'era)
                                (?E 'weekday-context-name)
                                (?a 'am/pm))
                              (if (>= num-repetitions 4) 'full 'abbreviated))
                        parts))
                 ((or ?y ?Y)
                  (push (cons (if (= character ?y) 'year 'year-for-week)
                              (pcase num-repetitions
                                (1 'add-century-when-parsing)
                                (2 'always-two-digits)
                                (_ num-repetitions)))
                        parts))
                 ((or ?M ?L)
                  (push (if (<= num-repetitions 2)
                            (cons 'month num-repetitions)
                          (cons (if (= character ?M) 'month-context-name 'month-standalone-name)
                                (if (>= num-repetitions 4) 'full 'abbreviated)))
                        parts))
                 (?w (push (cons 'week-in-year     num-repetitions) parts))
                 (?W (push (cons 'week-in-month    num-repetitions) parts))
                 (?D (push (cons 'day-in-year      num-repetitions) parts))
                 (?d (push (cons 'day-in-month     num-repetitions) parts))
                 (?F (push (cons 'weekday-in-month num-repetitions) parts))
                 (?H (push (cons 'hour-0-23        num-repetitions) parts))
                 (?k (push (cons 'hour-1-24        num-repetitions) parts))
                 (?K (push (cons 'hour-am/pm-0-11  num-repetitions) parts))
                 (?h (push (cons 'hour-am/pm-1-12  num-repetitions) parts))
                 (?m (push (cons 'minute           num-repetitions) parts))
                 (?s (push (cons 'second           num-repetitions) parts))
                 (?S (push (cons (if (plist-get options :second-fractional-extension) 'second-fractional 'millisecond)
                                 num-repetitions)
                           parts))
                 (?z (push (cons 'timezone         'general)        parts))
                 (?Z (push (cons 'timezone         'rfc-822)        parts))
                 (?X (push (cons 'timezone         'iso-8601)       parts))
                 (_
                  (error "Illegal pattern character `%c'" character))))
              (t
               (datetime--extend-as-is-part parts (string character))))))
    (nreverse parts)))

(defun datetime--format-java-pattern (parts options)
  (let ((case-fold-search nil)
        strings)
    (dolist (part parts)
      (if (stringp part)
          (progn
            (when (string-match "\\`'+" part)
              (push (concat (match-string-no-properties 0) (match-string-no-properties 0)) strings)
              (setq part (substring part (match-end 0))))
            (when (> (length part) 0)
              (push (if (string-match "['[:alpha:]]" part)
                        ;; TODO: Might want to prettify a bit.
                        (concat "'" (replace-regexp-in-string "'" "''" part t t) "'")
                      part)
                    strings)))
        (let* ((type    (car part))
               (details (cdr part))
               (string  (pcase type
                          (`era              "G")
                          ((or `year `year-for-week)
                           (let ((base (if (eq type 'year) ?y ?Y)))
                             (pcase details
                               (`add-century-when-parsing base)
                               (`always-two-digits        (cons base 2))
                               (_                         (cons base details)))))
                          (`month            (cons ?M details))
                          ((or `month-context-name `month-standalone-name `weekday-context-name)
                           (cons (pcase type
                                   (`month-context-name    ?M)
                                   (`month-standalone-name ?L)
                                   (`weekday-context-name  ?E))
                                 (pcase details
                                   (`abbreviated 3)
                                   (`full        4)
                                   (_            (error "Unexpected details %s" details)))))
                          (`week-in-year      (cons ?w details))
                          (`week-in-month     (cons ?W details))
                          (`day-in-year       (cons ?D details))
                          (`day-in-month      (cons ?d details))
                          (`weekday-in-month  (cons ?F details))
                          (`hour-0-23         (cons ?H details))
                          (`hour-1-24         (cons ?k details))
                          (`hour-am/pm-0-11   (cons ?K details))
                          (`hour-am/pm-1-12   (cons ?h details))
                          (`minute            (cons ?m details))
                          (`second            (cons ?s details))
                          (`millisecond       (cons ?S details))
                          (`second-fractional (if (plist-get options :second-fractional-extension)
                                                  (cons ?S details)
                                                (error "`second-fractional' extension is not enabled")))
                          (`am/pm             "a")
                          (_                  (error "Unexpected part type %s" type)))))
          (push (cond ((integerp string)
                       (string string))
                      ((consp string)
                       (unless (integerp (cdr string))
                         (error "Unexpected details %s" (cdr string)))
                       (make-string (cdr string) (car string)))
                      (t
                       string))
                strings))))
    (apply #'concat (nreverse strings))))


(defun datetime-matching-regexp (type pattern &rest options)
  "Return a regexp that matches date-time according to the PATTERN.
Argument TYPE defines how the pattern should be interpreted, see
library documentation.  Rest of the arguments must be a property
list, i.e. keywords interleaved with values.

Returned regexp contains only \"shy\" groups, so it can be
inserted into a larger one without screwing group ordering.  Note
that the returned regexp as a whole is not enclosed in a group;
when inserting you need to take that into account where
necessarily.

Note that the returned regexp will match some incorrect dates
too.  It is supposed to be used as a good and fast estimation if
a string represents date-time formatted according to PATTERN, but
it is not strict enough to be used as a validator.

OPTIONS should be any keyword arguments understood by
`datetime-recode-pattern' plus any from the list below, specific
to this function.

The function understands several keyword arguments to subtly
tweak the produced regexp.  Many of these flags can be used to
discard valid date-time strings.  They are still useful because
\"can be parsed\" is not necessarily equal to \"valid in this
context\".  Default value of keyword arguments is nil unless
specified otherwise.

  :locale

    Locale (language) used for month, weekday etc. names.  Always
    defaults to English, even if system locale is different.

  :only-4-digit-years

    Match only four consecutive digits as a year assuming the
    pattern contains a 4-digit year placeholder.  By default any
    number of digits will be accepted.  This can be seen as a
    special case of :require-leading-zeros for year field only.

  :lax-whitespace

    Match any whitespace in PATTERN against any whitespace in
    date-time string.  For this purpose \"whitespace\" is defined
    as space and tab characters only.

  :accept-leading-space

    Make variable-width numbers (e.g. day number without leading
    zero) match also if there is a leading space.

  :require-leading-zeros

    Make numbers that are formatted with leading zeros in PATTERN
    only match when there are corresponding zeros in the string.

  :forbid-unnecessary-zeros

    Don't match more leading zeros than required by the pattern.
    E.g. \"030 September\" is a valid date, but no-one writes it
    like that and with this flag such strings are not matched."
  (let* ((lax-whitespace (plist-get options :lax-whitespace))
         (locale         (or (plist-get options :locale) 'en))
         regexp-parts)
    (dolist (part (datetime--parse-pattern type pattern options))
      (if (stringp part)
          (push (if lax-whitespace
                    (replace-regexp-in-string (rx (1+ (any blank))) (rx (1+ (any blank))) (regexp-quote part) t t)
                  (regexp-quote part))
                regexp-parts)
        (let* ((type    (car part))
               (details (cdr part))
               (regexp  (pcase type
                          (`era (regexp-opt (append (datetime-locale-field locale :era) nil)))
                          ((or `year `year-for-week)
                           (cond ((and (plist-get options :only-4-digit-years) (eq details 4))
                                  (rx (= 4 (any "0-9"))))
                                 ((or (memq details '(1 add-century-when-parsing)) (not (plist-get options :require-leading-zeros)))
                                  (rx (1+ (any "0-9"))))
                                 ((memq details '(2 always-two-digits))
                                  (rx (any "0-9") (1+ (any "0-9"))))
                                 (t
                                  (format "[0-9]\\{%d\\}[0-9]+" (1- details)))))
                          (`month                12)
                          (`month-context-name
                           (regexp-opt (append (datetime-locale-field locale (if (eq details 'abbreviated)
                                                                                 :month-context-abbr
                                                                               :month-context-names))
                                               nil)))
                          (`month-standalone-name
                           (regexp-opt (append (datetime-locale-field locale (if (eq details 'abbreviated)
                                                                                 :month-standalone-abbr
                                                                               :month-standalone-names))
                                               nil)))
                          (`week-in-year     53)
                          (`week-in-month     5)
                          (`day-in-month     31)
                          (`weekday-in-month  5)
                          (`weekday           7)
                          (`weekday-context-name
                           (regexp-opt (append (datetime-locale-field locale (if (eq details 'abbreviated)
                                                                                 :weekday-context-abbr
                                                                               :weekday-context-names))
                                               nil)))
                          (`weekday-standalone-name
                           (regexp-opt (append (datetime-locale-field locale (if (eq details 'abbreviated)
                                                                                 :weekday-standalone-abbr
                                                                               :weekday-standalone-names))
                                               nil)))
                          (`am/pm
                           (regexp-opt (append (datetime-locale-field locale :am/pm) nil)))
                          (`hour-0-23        23)
                          (`hour-1-24        24)
                          (`hour-am/pm-0-11  11)
                          (`hour-am/pm-1-12  12)
                          (`minute           59)
                          (`second           59)
                          ((or `millisecond `second-fractional)
                           (apply #'concat (make-list details (rx (any "0-9")))))
                          (`timezone
                           (signal 'datetime-unsupported-timezone nil))
                          ((pred stringp)
                           (regexp-quote type))
                          (_ (error "Unexpected value %s" type)))))
          (when (integerp regexp)
            ;; REGEXP is really the maximum value of this one- or
            ;; two-digit number.
            (setq regexp (if (<= regexp 9)
                             (cond ((and (>= details 2) (plist-get options :require-leading-zeros)
                                         (format "%s[1-%d]" (make-string (- details 1) ?0) regexp)))
                                   ((plist-get options :forbid-unnecessary-zeros)
                                    (format "[1-%d]" regexp))
                                   (t
                                    (format "0*[1-%d]" regexp)))
                           (cond ((and (= details 1) (plist-get options :accept-leading-space))
                                  (format "[ 0-%d]?[0-9]" (/ regexp 10)))
                                 ((and (>= details 2) (plist-get options :require-leading-zeros)
                                       (format "%s[0-%d][0-9]" (make-string (- details 2) ?0) (/ regexp 10))))
                                 ((plist-get options :forbid-unnecessary-zeros)
                                  (format "[0-%d]?[0-9]" (/ regexp 10)))
                                 ((>= regexp 20)
                                  (format "0*[1-%d]?[0-9]" (/ regexp 10)))
                                 (t
                                  "0*1?[0-9]")))))
          (push regexp regexp-parts))))
    (apply #'concat (nreverse regexp-parts))))


(defun datetime-recode-pattern (from to pattern &rest options)
  "Recode PATTERN between two supported types.
As a special case, either of FROM and TO can be set to 'parsed.
This is useful as a speed optimization in a few cases where you
perform several transformations on the same pattern.

Options can be a list of the following keyword arguments:

  :second-fractional-extension

    In Java patterns any number of \"S\" stand for milliseconds.
    With this extension they are instead interpreted according to
    how many \"S\" there is, e.g. \"SSSSSS\" means microseconds."
  (datetime--format-pattern to (datetime--parse-pattern from pattern options) options))


;; Arguments are expected to be atoms.
(defmacro datetime--pattern-includes-p (type pattern &rest part-types)
  `(let ((parts (datetime--parse-pattern ,type ,pattern nil))
         includes)
     (while parts
       (let ((part (car parts)))
         (if (and (consp part) ,(if (= (length part-types) 1)
                                    `(eq (car part) ',(car part-types))
                                  `(memq (car part) ',part-types)))
             (setq parts    nil
                   includes t)
           (setq parts (cdr parts)))))
     includes))

(defun datetime-pattern-locale-dependent-p (type pattern)
  "Determine if PATTERN includes any locale-based parts.
In other words, return non-nil if PATTERN includes any textual
names."
  (datetime--pattern-includes-p type pattern era month-context-name month-standalone-name weekday-context-name weekday-standalone-name am/pm))

(defun datetime-pattern-includes-era-p (type pattern)
  "Determine if PATTERN includes the date era."
  (datetime--pattern-includes-p type pattern era))

(defun datetime-pattern-includes-year-p (type pattern)
  "Determine if PATTERN includes the year."
  (datetime--pattern-includes-p type pattern year year-for-week))

(defun datetime-pattern-includes-month-p (type pattern)
  "Determine if PATTERN includes the month."
  (datetime--pattern-includes-p type pattern month month-context-name month-standalone-name))

(defun datetime-pattern-includes-day-p (type pattern)
  "Determine if PATTERN includes the day."
  (datetime--pattern-includes-p type pattern day-in-year day-in-month))

(defun datetime-pattern-includes-hour-p (type pattern)
  "Determine if PATTERN includes hours."
  (datetime--pattern-includes-p type pattern hour-0-23 hour-1-24 hour-am/pm-0-11 hour-am/pm-1-12))

(defun datetime-pattern-includes-minute-p (type pattern)
  "Determine if PATTERN includes minutes."
  (datetime--pattern-includes-p type pattern minute))

(defun datetime-pattern-includes-second-p (type pattern)
  "Determine if PATTERN includes seconds."
  (datetime--pattern-includes-p type pattern second))

(defun datetime-pattern-includes-millisecond-p (type pattern)
  "Determine if PATTERN includes fractions of seconds."
  ;; Without enabled :second-fractional-extension consecutive "S" are
  ;; just always parsed to milliseconds.  Check for
  ;; `second-fractional' just in case of another pattern type.
  (datetime--pattern-includes-p type pattern millisecond second-fractional))

(defun datetime-pattern-includes-timezone-p (type pattern)
  "Determine if PATTERN includes timezone."
  (datetime--pattern-includes-p type pattern timezone))


(defun datetime-list-locales (&optional include-variants)
  "List all locales for which the library has information.
If INCLUDE-VARIANTS is nil, only include “base” locales (in
format \"xx\"), if it is t then also include “variants” in format
\"xx-YY\".

Return value is a list of symbols; it can be modified freely."
  (let (locales)
    (dolist (data datetime--locale-data)
      (when (or include-variants (<= (length (symbol-name (car data))) 2))
        (push (car data) locales)))
    (nreverse locales)))


(defsubst datetime--do-get-locale-pattern (patterns variant)
  (or (plist-get patterns variant)
      (unless (eq variant :medium) (plist-get patterns :medium))
      (when (eq variant :full) (plist-get patterns :long))))

(defun datetime-locale-date-pattern (locale &optional variant)
  "Get given date pattern for the LOCALE.
Supported variants are `:short', `:medium', `:long' and `:full'.
If no VARIANT is specified, it defaults to `:medium'.

Returned pattern is always of type \\\='java."
  (datetime--do-get-locale-pattern (datetime-locale-field locale :date-patterns) (or variant :medium)))

(defun datetime-locale-time-pattern (locale &optional variant)
  "Get given time pattern for the LOCALE.
Supported variants are `:short', `:medium', `:long' and `:full'.
If no VARIANT is specified, it defaults to `:medium'.

Returned pattern is always of type \\\='java."
  (datetime--do-get-locale-pattern (datetime-locale-field locale :time-patterns) (or variant :medium)))

(defun datetime-locale-date-time-pattern (locale &optional date-variant time-variant)
  "Get given date-time pattern for the LOCALE.
Supported variants are `:short', `:medium', `:long' and `:full'.
If DATE-VARIANT is not specified, it defaults to `:medium'.  If
TIME-VARIANT is not specified, it defaults to DATE-VARIANT (or
`:medium' it that's missing too).

Returned pattern is always of type \\\='java.

This function exists not just for completeness: while in most
cases the result is just corresponding date and time patterns
separated by a space, for a few locales it is different."
  (let ((date-time-pattern-rule (or (datetime-locale-field locale :date-time-pattern-rule) '(t . " ")))
        (date-part              (datetime-locale-date-pattern locale date-variant))
        (time-part              (datetime-locale-time-pattern locale (or time-variant date-variant))))
    (if (car date-time-pattern-rule)
        (concat date-part (cdr date-time-pattern-rule) time-part)
      (concat time-part (cdr date-time-pattern-rule) date-part))))


(defconst datetime--english-eras  ["BC" "AD"])
(defconst datetime--english-am-pm ["AM" "PM"])

(defsubst datetime--do-get-locale-field (locale-data field)
  (or (plist-get locale-data field)
      ;; See `datetime--locale-data' for description of fallbacks.
      (pcase field
        (:decimal-separator        ?.)
        (:eras                     datetime--english-eras)
        (:month-standalone-abbr    (plist-get locale-data :month-context-abbr))
        (:month-standalone-names   (plist-get locale-data :month-context-names))
        (:weekday-standalone-abbr  (plist-get locale-data :weekday-context-abbr))
        (:weekday-standalone-names (plist-get locale-data :weekday-context-names))
        (:am/pm                    datetime--english-am-pm))))

(defun datetime-locale-field (locale field)
  "Get a FIELD of data for the LOCALE.
Supported fields:

  :decimal-separator
  :eras
  :month-context-abbr
  :month-context-names
  :weekday-context-abbr
  :weekday-context-names
  :month-standalone-abbr
  :month-standalone-names
  :weekday-standalone-abbr
  :weekday-standalone-names
  :am-pm"
  ;; Additionally `:date-patterns', `:time-patterns' and
  ;; `:date-time-pattern-rule' are supported for internal use.
  (or (datetime--do-get-locale-field (cdr (assq locale datetime--locale-data)) field)
      (let ((name (symbol-name locale)))
        (when (> (length name) 3)
          (datetime--do-get-locale-field (cdr (assq (intern (substring name 0 2)) datetime--locale-data)) field)))))


;; Extracted from Java using `dev/HarvestData.java'.  All patterns are
;; obviously of java type.
;;
;; There are many fallbacks involved to reduce size of this alist:
;;   - for locale XX-YY value for any property defaults to that of
;;     locale XX;
;;   - `:decimal-separator' defaults to dot;
;;   - `:eras' and `:am-pm' default to English version;
;;   - month/dayweek standalone abbreviations or names default to
;;     the corresponding context-aware property;
;;   - date-time patterns are not stored, instead they are built from
;;     date and time parts for that locale; corresponding field is a
;;     cons with car determining what should be in the beginning (t
;;     for date, nil for time), and cdr being the separator string;
;;     the cons defaults to (t . " ");
;;   - all patterns have the following fallbacks: `:short' defaults to
;;     `:medium', `:long' defaults to `:medium', `:full' defaults to
;;     `:long'.
(setq datetime--locale-data
  '((ar
     :eras                     ["ق.م" "م"]
     :month-context-abbr       ["ينا" "فبر" "مار" "أبر" "ماي" "يون" "يول" "أغس" "سبت" "أكت" "نوف" "ديس"]
     :month-context-names      ["يناير" "فبراير" "مارس" "أبريل" "مايو" "يونيو" "يوليو" "أغسطس" "سبتمبر" "أكتوبر" "نوفمبر" "ديسمبر"]
     :weekday-context-abbr     ["ن" "ث" "ر" "خ" "ج" "س" "ح"]
     :weekday-context-names    ["الاثنين" "الثلاثاء" "الأربعاء" "الخميس" "الجمعة" "السبت" "الأحد"]
     :am-pm                    ["ص" "م"]
     :date-patterns            (:short "dd/MM/yy" :medium "dd/MM/yyyy" :long "dd MMMM, yyyy")
     :time-patterns            (:short "hh:mm a" :medium "hh:mm:ss a" :long "z hh:mm:ss a"))
    (ar-JO
     :month-context-abbr       ["كانون الثاني" "شباط" "آذار" "نيسان" "نوار" "حزيران" "تموز" "آب" "أيلول" "تشرين الأول" "تشرين الثاني" "كانون الأول"]
     :month-context-names      ["كانون الثاني" "شباط" "آذار" "نيسان" "نوار" "حزيران" "تموز" "آب" "أيلول" "تشرين الأول" "تشرين الثاني" "كانون الأول"]
     :weekday-context-abbr     ["الاثنين" "الثلاثاء" "الأربعاء" "الخميس" "الجمعة" "السبت" "الأحد"])
    (ar-LB
     :month-context-abbr       ["كانون الثاني" "شباط" "آذار" "نيسان" "نوار" "حزيران" "تموز" "آب" "أيلول" "تشرين الأول" "تشرين الثاني" "كانون الأول"]
     :month-context-names      ["كانون الثاني" "شباط" "آذار" "نيسان" "نوار" "حزيران" "تموز" "آب" "أيلول" "تشرين الأول" "تشرين الثاني" "كانون الأول"]
     :weekday-context-abbr     ["الاثنين" "الثلاثاء" "الأربعاء" "الخميس" "الجمعة" "السبت" "الأحد"])
    (ar-SY
     :month-context-abbr       ["كانون الثاني" "شباط" "آذار" "نيسان" "نوار" "حزيران" "تموز" "آب" "أيلول" "تشرين الأول" "تشرين الثاني" "كانون الأول"]
     :month-context-names      ["كانون الثاني" "شباط" "آذار" "نيسان" "نواران" "حزير" "تموز" "آب" "أيلول" "تشرين الأول" "تشرين الثاني" "كانون الأول"]
     :weekday-context-abbr     ["الاثنين" "الثلاثاء" "الأربعاء" "الخميس" "الجمعة" "السبت" "الأحد"])
    (be
     :decimal-separator        ?,
     :eras                     ["да н.е." "н.е."]
     :month-context-abbr       ["стд" "лют" "скв" "крс" "май" "чрв" "лпн" "жнв" "врс" "кст" "ліс" "снж"]
     :month-context-names      ["студзеня" "лютага" "сакавіка" "красавіка" "мая" "чрвеня" "ліпеня" "жніўня" "верасня" "кастрычніка" "лістапада" "снежня"]
     :weekday-context-abbr     ["пн" "ат" "ср" "чц" "пт" "сб" "нд"]
     :weekday-context-names    ["панядзелак" "аўторак" "серада" "чацвер" "пятніца" "субота" "нядзеля"]
     :date-patterns            (:short "d.M.yy" :medium "d.M.yyyy" :long "EEEE, d, MMMM yyyy")
     :time-patterns            (:short "H.mm" :medium "H.mm.ss" :long "H.mm.ss z"))
    (bg
     :decimal-separator        ?,
     :eras                     ["пр.н.е." "н.е."]
     :month-context-abbr       ["I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX" "X" "XI" "XII"]
     :month-context-names      ["Януари" "Февруари" "Март" "Април" "Май" "Юни" "Юли" "Август" "Септември" "Октомври" "Ноември" "Декември"]
     :weekday-context-abbr     ["Пн" "Вт" "Ср" "Чт" "Пт" "Сб" "Нд"]
     :weekday-context-names    ["Понеделник" "Вторник" "Сряда" "Четвъртък" "Петък" "Събота" "Неделя"]
     :date-patterns            (:short "dd.MM.yy" :medium "dd.MM.yyyy" :long "dd MMMM y" :full "dd MMMM y, EEEE")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z" :full "HH:mm:ss zzzz"))
    (ca
     :decimal-separator        ?,
     :month-context-abbr       ["de gen." "de febr." "de març" "d’abr." "de maig" "de juny" "de jul." "d’ag." "de set." "d’oct." "de nov." "de des."]
     :month-context-names      ["de gener" "de febrer" "de març" "d’abril" "de maig" "de juny" "de juliol" "d’agost" "de setembre" "d’octubre" "de novembre" "de desembre"]
     :weekday-context-abbr     ["dl." "dt." "dc." "dj." "dv." "ds." "dg."]
     :weekday-context-names    ["dilluns" "dimarts" "dimecres" "dijous" "divendres" "dissabte" "diumenge"]
     :month-standalone-abbr    ["gen." "feb." "març" "abr." "maig" "juny" "jul." "ag." "set." "oct." "nov." "des."]
     :month-standalone-names   ["gener" "febrer" "març" "abril" "maig" "juny" "juliol" "agost" "setembre" "octubre" "novembre" "desembre"]
     :date-patterns            (:short "dd/MM/yy" :medium "dd/MM/yyyy" :long "d' / 'MMMM' / 'yyyy" :full "EEEE, d' / 'MMMM' / 'yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z"))
    (cs
     :decimal-separator        ?,
     :eras                     ["př.Kr." "po Kr."]
     :month-context-abbr       ["Led" "Úno" "Bře" "Dub" "Kvě" "Čer" "Čvc" "Srp" "Zář" "Říj" "Lis" "Pro"]
     :month-context-names      ["ledna" "února" "března" "dubna" "května" "června" "července" "srpna" "září" "října" "listopadu" "prosince"]
     :weekday-context-abbr     ["Po" "Út" "St" "Čt" "Pá" "So" "Ne"]
     :weekday-context-names    ["Pondělí" "Úterý" "Středa" "Čtvrtek" "Pátek" "Sobota" "Neděle"]
     :month-standalone-abbr    ["I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX" "X" "XI" "XII"]
     :month-standalone-names   ["leden" "únor" "březen" "duben" "květen" "červen" "červenec" "srpen" "září" "říjen" "listopad" "prosinec"]
     :am-pm                    ["dop." "odp."]
     :date-patterns            (:short "d.M.yy" :medium "d.M.yyyy" :long "d. MMMM yyyy" :full "EEEE, d. MMMM yyyy")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss" :long "H:mm:ss z"))
    (da
     :decimal-separator        ?,
     :eras                     ["f.Kr." "e.Kr."]
     :month-context-abbr       ["jan." "feb." "mar." "apr." "maj" "jun." "jul." "aug." "sep." "okt." "nov." "dec."]
     :month-context-names      ["januar" "februar" "marts" "april" "maj" "juni" "juli" "august" "september" "oktober" "november" "december"]
     :weekday-context-abbr     ["ma" "ti" "on" "to" "fr" "lø" "sø"]
     :weekday-context-names    ["mandag" "tirsdag" "onsdag" "torsdag" "fredag" "lørdag" "søndag"]
     :month-standalone-abbr    ["jan" "feb" "mar" "apr" "maj" "jun" "jul" "aug" "sep" "okt" "nov" "dec"]
     :date-patterns            (:short "dd-MM-yy" :medium "dd-MM-yyyy" :long "d. MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z"))
    (de
     :decimal-separator        ?,
     :eras                     ["v. Chr." "n. Chr."]
     :month-context-abbr       ["Jan" "Feb" "Mär" "Apr" "Mai" "Jun" "Jul" "Aug" "Sep" "Okt" "Nov" "Dez"]
     :month-context-names      ["Januar" "Februar" "März" "April" "Mai" "Juni" "Juli" "August" "September" "Oktober" "November" "Dezember"]
     :weekday-context-abbr     ["Mo" "Di" "Mi" "Do" "Fr" "Sa" "So"]
     :weekday-context-names    ["Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag" "Samstag" "Sonntag"]
     :month-standalone-abbr    ["Jan" "Feb" "Mrz" "Apr" "Mai" "Jun" "Jul" "Aug" "Sep" "Okt" "Nov" "Dez"]
     :date-patterns            (:short "dd.MM.yy" :medium "dd.MM.yyyy" :long "d. MMMM yyyy" :full "EEEE, d. MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z" :full "HH:mm' Uhr 'z"))
    (de-AT
     :month-context-abbr       ["Jän" "Feb" "Mär" "Apr" "Mai" "Jun" "Jul" "Aug" "Sep" "Okt" "Nov" "Dez"]
     :month-context-names      ["Jänner" "Februar" "März" "April" "Mai" "Juni" "Juli" "August" "September" "Oktober" "November" "Dezember"]
     :date-patterns            (:short "dd.MM.yy" :medium "dd.MM.yyyy" :long "dd. MMMM yyyy" :full "EEEE, dd. MMMM yyyy"))
    (de-CH)
    (el
     :decimal-separator        ?,
     :month-context-abbr       ["Ιαν" "Φεβ" "Μαρ" "Απρ" "Μαϊ" "Ιουν" "Ιουλ" "Αυγ" "Σεπ" "Οκτ" "Νοε" "Δεκ"]
     :month-context-names      ["Ιανουαρίου" "Φεβρουαρίου" "Μαρτίου" "Απριλίου" "Μαΐου" "Ιουνίου" "Ιουλίου" "Αυγούστου" "Σεπτεμβρίου" "Οκτωβρίου" "Νοεμβρίου" "Δεκεμβρίου"]
     :weekday-context-abbr     ["Δευ" "Τρι" "Τετ" "Πεμ" "Παρ" "Σαβ" "Κυρ"]
     :weekday-context-names    ["Δευτέρα" "Τρίτη" "Τετάρτη" "Πέμπτη" "Παρασκευή" "Σάββατο" "Κυριακή"]
     :month-standalone-abbr    ["Ιαν" "Φεβ" "Μάρ" "Απρ" "Μάι" "Ιούν" "Ιούλ" "Αυγ" "Σεπ" "Οκτ" "Νοέ" "Δεκ"]
     :month-standalone-names   ["Ιανουάριος" "Φεβρουάριος" "Μάρτιος" "Απρίλιος" "Μάϊος" "Ιούνιος" "Ιούλιος" "Αύγουστος" "Σεπτέμβριος" "Οκτώβριος" "Νοέμβριος" "Δεκέμβριος"]
     :am-pm                    ["πμ" "μμ"]
     :date-patterns            (:short "d/M/yyyy" :medium "d MMM yyyy" :long "d MMMM yyyy" :full "EEEE, d MMMM yyyy")
     :time-patterns            (:short "h:mm a" :medium "h:mm:ss a" :long "h:mm:ss a z"))
    (el-CY
     :eras                     ["π.Χ." "μ.Χ."]
     :month-context-names      ["Ιανουάριος" "Φεβρουάριος" "Μάρτιος" "Απρίλιος" "Μάιος" "Ιούνιος" "Ιούλιος" "Αύγουστος" "Σεπτέμβριος" "Οκτώβριος" "Νοέμβριος" "Δεκέμβριος"]
     :am-pm                    ["ΠΜ" "ΜΜ"]
     :date-patterns            (:short "dd/MM/yyyy" :medium "dd MMM yyyy" :long "dd MMMM yyyy" :full "EEEE, dd MMMM yyyy"))
    (en
     :month-context-abbr       ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
     :month-context-names      ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"]
     :weekday-context-abbr     ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"]
     :weekday-context-names    ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"]
     :date-patterns            (:short "M/d/yy" :medium "MMM d, yyyy" :long "MMMM d, yyyy" :full "EEEE, MMMM d, yyyy")
     :time-patterns            (:short "h:mm a" :medium "h:mm:ss a" :long "h:mm:ss a z"))
    (en-AU
     :date-patterns            (:short "d/MM/yy" :medium "dd/MM/yyyy" :long "d MMMM yyyy" :full "EEEE, d MMMM yyyy")
     :time-patterns            (:short "h:mm a" :medium "h:mm:ss a" :full "h:mm:ss a z"))
    (en-CA
     :date-patterns            (:short "dd/MM/yy" :medium "d-MMM-yyyy" :long "MMMM d, yyyy" :full "EEEE, MMMM d, yyyy")
     :time-patterns            (:short "h:mm a" :medium "h:mm:ss a" :long "h:mm:ss z a" :full "h:mm:ss 'o''clock' a z"))
    (en-GB
     :date-patterns            (:short "dd/MM/yy" :medium "dd-MMM-yyyy" :long "dd MMMM yyyy" :full "EEEE, d MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z" :full "HH:mm:ss 'o''clock' z"))
    (en-IE
     :date-patterns            (:short "dd/MM/yy" :medium "dd-MMM-yyyy" :long "dd MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z" :full "HH:mm:ss 'o''clock' z"))
    (en-IN
     :date-patterns            (:short "d/M/yy" :medium "d MMM, yyyy" :long "d MMMM, yyyy" :full "EEEE, d MMMM, yyyy"))
    (en-MT
     :date-patterns            (:short "dd/MM/yyyy" :medium "dd MMM yyyy" :long "dd MMMM yyyy" :full "EEEE, d MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z"))
    (en-NZ
     :date-patterns            (:short "d/MM/yy" :medium "d/MM/yyyy" :long "d MMMM yyyy" :full "EEEE, d MMMM yyyy")
     :time-patterns            (:short "h:mm a" :medium "h:mm:ss a" :full "h:mm:ss a z"))
    (en-PH
     :date-patterns            (:short "M/d/yy" :medium "MM d, yy" :long "MMMM d, yyyy" :full "EEEE, MMMM d, yyyy"))
    (en-ZA
     :date-patterns            (:short "yyyy/MM/dd" :medium "dd MMM yyyy" :long "dd MMMM yyyy" :full "EEEE dd MMMM yyyy")
     :time-patterns            (:short "h:mm a" :medium "h:mm:ss a"))
    (es
     :decimal-separator        ?,
     :eras                     ["antes de Cristo" "anno Dómini"]
     :month-context-abbr       ["ene" "feb" "mar" "abr" "may" "jun" "jul" "ago" "sep" "oct" "nov" "dic"]
     :month-context-names      ["enero" "febrero" "marzo" "abril" "mayo" "junio" "julio" "agosto" "septiembre" "octubre" "noviembre" "diciembre"]
     :weekday-context-abbr     ["lun" "mar" "mié" "jue" "vie" "sáb" "dom"]
     :weekday-context-names    ["lunes" "martes" "miércoles" "jueves" "viernes" "sábado" "domingo"]
     :date-patterns            (:short "d/MM/yy" :medium "dd-MMM-yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss" :long "H:mm:ss z" :full "HH'H'mm'' z"))
    (es-AR
     :date-patterns            (:short "dd/MM/yy" :medium "dd/MM/yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "H:mm:ss z" :full "HH'h'''mm z"))
    (es-BO
     :date-patterns            (:short "dd-MM-yy" :medium "dd-MM-yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "hh:mm a" :medium "hh:mm:ss a" :long "hh:mm:ss a z"))
    (es-CL
     :date-patterns            (:short "dd-MM-yy" :medium "dd-MM-yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss" :long "H:mm:ss z" :full "HH:mm:ss zzzz"))
    (es-CO
     :date-patterns            (:short "d/MM/yy" :medium "d/MM/yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "hh:mm a" :medium "hh:mm:ss a" :long "hh:mm:ss a z"))
    (es-CR
     :date-patterns            (:short "dd/MM/yy" :medium "dd/MM/yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "hh:mm a" :medium "hh:mm:ss a" :long "hh:mm:ss a z"))
    (es-DO
     :date-patterns            (:short "dd/MM/yy" :medium "dd/MM/yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "hh:mm a" :medium "hh:mm:ss a" :long "hh:mm:ss a z"))
    (es-EC
     :date-patterns            (:short "dd/MM/yy" :medium "dd/MM/yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss" :long "H:mm:ss z" :full "HH:mm:ss zzzz"))
    (es-GT
     :date-patterns            (:short "d/MM/yy" :medium "d/MM/yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "hh:mm a" :medium "hh:mm:ss a" :long "hh:mm:ss a z"))
    (es-HN
     :date-patterns            (:short "MM-dd-yy" :medium "MM-dd-yyyy" :long "dd' de 'MMMM' de 'yyyy" :full "EEEE dd' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "hh:mm a" :medium "hh:mm:ss a" :long "hh:mm:ss a z"))
    (es-MX
     :date-patterns            (:short "d/MM/yy" :medium "d/MM/yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "hh:mm a" :medium "hh:mm:ss a" :long "hh:mm:ss a z"))
    (es-NI
     :date-patterns            (:short "MM-dd-yy" :medium "MM-dd-yyyy" :long "dd' de 'MMMM' de 'yyyy" :full "EEEE dd' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "hh:mm a" :medium "hh:mm:ss a" :long "hh:mm:ss a z"))
    (es-PA
     :date-patterns            (:short "MM/dd/yy" :medium "MM/dd/yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "hh:mm a" :medium "hh:mm:ss a" :long "hh:mm:ss a z"))
    (es-PE
     :date-patterns            (:short "dd/MM/yy" :medium "dd/MM/yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "hh:mm a" :medium "hh:mm:ss a" :long "hh:mm:ss a z"))
    (es-PR
     :date-patterns            (:short "MM-dd-yy" :medium "MM-dd-yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "hh:mm a" :medium "hh:mm:ss a" :long "hh:mm:ss a z"))
    (es-PY
     :date-patterns            (:short "dd/MM/yy" :medium "dd/MM/yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "hh:mm a" :medium "hh:mm:ss a" :long "hh:mm:ss a z"))
    (es-SV
     :date-patterns            (:short "MM-dd-yy" :medium "MM-dd-yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "hh:mm a" :medium "hh:mm:ss a" :long "hh:mm:ss a z"))
    (es-US
     :eras                     ["a.C." "d.C."]
     :am-pm                    ["a.m." "p.m."]
     :date-patterns            (:short "M/d/yy" :medium "MMM d, yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "h:mm a" :medium "h:mm:ss a" :long "h:mm:ss a z"))
    (es-UY
     :date-patterns            (:short "dd/MM/yy" :medium "dd/MM/yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "hh:mm a" :medium "hh:mm:ss a" :long "hh:mm:ss a z"))
    (es-VE
     :date-patterns            (:short "dd/MM/yy" :medium "dd/MM/yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "hh:mm a" :medium "hh:mm:ss a" :long "hh:mm:ss a z"))
    (et
     :decimal-separator        ?,
     :eras                     ["e.m.a." "m.a.j."]
     :month-context-abbr       ["jaan" "veebr" "märts" "apr" "mai" "juuni" "juuli" "aug" "sept" "okt" "nov" "dets"]
     :month-context-names      ["jaanuar" "veebruar" "märts" "aprill" "mai" "juuni" "juuli" "august" "september" "oktoober" "november" "detsember"]
     :weekday-context-abbr     ["E" "T" "K" "N" "R" "L" "P"]
     :weekday-context-names    ["esmaspäev" "teisipäev" "kolmapäev" "neljapäev" "reede" "laupäev" "pühapäev"]
     :date-patterns            (:short "d.MM.yy" :medium "d.MM.yyyy" :long "EEEE, d. MMMM yyyy. 'a'" :full "EEEE, d. MMMM yyyy")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss" :long "H:mm:ss z"))
    (fi
     :decimal-separator        ?,
     :eras                     ["eKr." "jKr."]
     :month-context-abbr       ["tammikuuta" "helmikuuta" "maaliskuuta" "huhtikuuta" "toukokuuta" "kesäkuuta" "heinäkuuta" "elokuuta" "syyskuuta" "lokakuuta" "marraskuuta" "joulukuuta"]
     :month-context-names      ["tammikuuta" "helmikuuta" "maaliskuuta" "huhtikuuta" "toukokuuta" "kesäkuuta" "heinäkuuta" "elokuuta" "syyskuuta" "lokakuuta" "marraskuuta" "joulukuuta"]
     :weekday-context-abbr     ["ma" "ti" "ke" "to" "pe" "la" "su"]
     :weekday-context-names    ["maanantai" "tiistai" "keskiviikko" "torstai" "perjantai" "lauantai" "sunnuntai"]
     :month-standalone-abbr    ["tammi" "helmi" "maalis" "huhti" "touko" "kesä" "heinä" "elo" "syys" "loka" "marras" "joulu"]
     :month-standalone-names   ["tammikuu" "helmikuu" "maaliskuu" "huhtikuu" "toukokuu" "kesäkuu" "heinäkuu" "elokuu" "syyskuu" "lokakuu" "marraskuu" "joulukuu"]
     :am-pm                    ["ap." "ip."]
     :date-patterns            (:medium "d.M.yyyy" :long "d. MMMM'ta 'yyyy")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss" :long "'klo 'H.mm.ss" :full "H.mm.ss z"))
    (fr
     :decimal-separator        ?,
     :eras                     ["BC" "ap. J.-C."]
     :month-context-abbr       ["janv." "févr." "mars" "avr." "mai" "juin" "juil." "août" "sept." "oct." "nov." "déc."]
     :month-context-names      ["janvier" "février" "mars" "avril" "mai" "juin" "juillet" "août" "septembre" "octobre" "novembre" "décembre"]
     :weekday-context-abbr     ["lun." "mar." "mer." "jeu." "ven." "sam." "dim."]
     :weekday-context-names    ["lundi" "mardi" "mercredi" "jeudi" "vendredi" "samedi" "dimanche"]
     :date-patterns            (:short "dd/MM/yy" :medium "d MMM yyyy" :long "d MMMM yyyy" :full "EEEE d MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z" :full "HH' h 'mm z"))
    (fr-BE
     :date-patterns            (:short "d/MM/yy" :medium "dd-MMM-yyyy" :long "d MMMM yyyy" :full "EEEE d MMMM yyyy")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss" :long "H:mm:ss z" :full "H' h 'mm' min 'ss' s 'z"))
    (fr-CA
     :date-patterns            (:short "yy-MM-dd" :medium "yyyy-MM-dd" :long "d MMMM yyyy" :full "EEEE d MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z" :full "H' h 'mm z"))
    (fr-CH
     :date-patterns            (:short "dd.MM.yy" :medium "d MMM yyyy" :long "d. MMMM yyyy" :full "EEEE, d. MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z" :full "HH.mm.' h' z"))
    (ga
     :eras                     ["RC" "AD"]
     :month-context-abbr       ["Ean" "Feabh" "Márta" "Aib" "Beal" "Meith" "Iúil" "Lún" "MFómh" "DFómh" "Samh" "Noll"]
     :month-context-names      ["Eanáir" "Feabhra" "Márta" "Aibreán" "Bealtaine" "Meitheamh" "Iúil" "Lúnasa" "Meán Fómhair" "Deireadh Fómhair" "Samhain" "Nollaig"]
     :weekday-context-abbr     ["Luan" "Máirt" "Céad" "Déar" "Aoine" "Sath" "Domh"]
     :weekday-context-names    ["Dé Luain" "Dé Máirt" "Dé Céadaoin" "Déardaoin" "Dé hAoine" "Dé Sathairn" "Dé Domhnaigh"]
     :am-pm                    ["a.m." "p.m."]
     :date-patterns            (:short "yy/MM/dd" :medium "yyyy MMM d" :long "yyyy MMMM d" :full "EEEE, yyyy MMMM dd")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z"))
    (ga-IE
     :date-patterns            (:short "dd/MM/yyyy" :medium "d MMM yyyy" :long "d MMMM yyyy" :full "EEEE d MMMM yyyy"))
    (he
     :eras                     ["לסה"נ" "לפסה"נ"]
     :month-context-abbr       ["ינו" "פבר" "מרץ" "אפר" "מאי" "יונ" "יול" "אוג" "ספט" "אוק" "נוב" "דצמ"]
     :month-context-names      ["ינואר" "פברואר" "מרץ" "אפריל" "מאי" "יוני" "יולי" "אוגוסט" "ספטמבר" "אוקטובר" "נובמבר" "דצמבר"]
     :weekday-context-abbr     ["ב" "ג" "ד" "ה" "ו" "ש" "א"]
     :weekday-context-names    ["יום שני" "יום שלישי" "יום רביעי" "יום חמישי" "יום שישי" "שבת" "יום ראשון"]
     :month-standalone-abbr    ["ינו׳" "פבר׳" "מרץ" "אפר׳" "מאי" "יונ׳" "יול׳" "אוג׳" "ספט׳" "אוק׳" "נוב׳" "דצמ׳"]
     :date-patterns            (:short "dd/MM/yy" :medium "dd/MM/yyyy" :long "d MMMM yyyy" :full "EEEE d MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z")
     :date-time-pattern-rule   (nil . " "))
    (hi
     :month-context-abbr       ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
     :month-context-names      ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"]
     :weekday-context-abbr     ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"]
     :weekday-context-names    ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"]
     :date-patterns            (:short "M/d/yy" :medium "MMM d, yyyy" :long "MMMM d, yyyy" :full "EEEE, MMMM d, yyyy")
     :time-patterns            (:short "h:mm a" :medium "h:mm:ss a" :long "h:mm:ss a z"))
    (hi-IN
     :eras                     ["ईसापूर्व" "सन"]
     :month-context-abbr       ["जनवरी" "फ़रवरी" "मार्च" "अप्रैल" "मई" "जून" "जुलाई" "अगस्त" "सितंबर" "अक्‍तूबर" "नवंबर" "दिसंबर"]
     :month-context-names      ["जनवरी" "फ़रवरी" "मार्च" "अप्रैल" "मई" "जून" "जुलाई" "अगस्त" "सितंबर" "अक्‍तूबर" "नवंबर" "दिसंबर"]
     :weekday-context-abbr     ["सोम" "मंगल" "बुध" "गुरु" "शुक्र" "शनि" "रवि"]
     :weekday-context-names    ["सोमवार" "मंगलवार" "बुधवार" "गुरुवार" "शुक्रवार" "शनिवार" "रविवार"]
     :am-pm                    ["पूर्वाह्न" "अपराह्न"]
     :date-patterns            (:short "d/M/yy" :medium "d MMM, yyyy" :long "d MMMM, yyyy" :full "EEEE, d MMMM, yyyy"))
    (hr
     :decimal-separator        ?,
     :eras                     ["Prije Krista" "Poslije Krista"]
     :month-context-abbr       ["sij" "velj" "ožu" "tra" "svi" "lip" "srp" "kol" "ruj" "lis" "stu" "pro"]
     :month-context-names      ["siječnja" "veljače" "ožujka" "travnja" "svibnja" "lipnja" "srpnja" "kolovoza" "rujna" "listopada" "studenoga" "prosinca"]
     :weekday-context-abbr     ["pon" "uto" "sri" "čet" "pet" "sub" "ned"]
     :weekday-context-names    ["ponedjeljak" "utorak" "srijeda" "četvrtak" "petak" "subota" "nedjelja"]
     :month-standalone-abbr    ["sij" "vel" "ožu" "tra" "svi" "lip" "srp" "kol" "ruj" "lis" "stu" "pro"]
     :month-standalone-names   ["siječanj" "veljača" "ožujak" "travanj" "svibanj" "lipanj" "srpanj" "kolovoz" "rujan" "listopad" "studeni" "prosinac"]
     :date-patterns            (:medium "yyyy.MM.dd" :long "yyyy. MMMM dd")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z"))
    (hr-HR
     :date-patterns            (:short "dd.MM.yy." :medium "dd.MM.yyyy." :long "yyyy. MMMM dd"))
    (hu
     :decimal-separator        ?,
     :eras                     ["i.e." "i.u."]
     :month-context-abbr       ["jan." "febr." "márc." "ápr." "máj." "jún." "júl." "aug." "szept." "okt." "nov." "dec."]
     :month-context-names      ["január" "február" "március" "április" "május" "június" "július" "augusztus" "szeptember" "október" "november" "december"]
     :weekday-context-abbr     ["H" "K" "Sze" "Cs" "P" "Szo" "V"]
     :weekday-context-names    ["hétfő" "kedd" "szerda" "csütörtök" "péntek" "szombat" "vasárnap"]
     :am-pm                    ["DE" "DU"]
     :date-patterns            (:medium "yyyy.MM.dd." :long "yyyy. MMMM d.")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss" :long "H:mm:ss z"))
    (id
     :decimal-separator        ?,
     :eras                     ["BCE" "CE"]
     :month-context-abbr       ["Jan" "Feb" "Mar" "Apr" "Mei" "Jun" "Jul" "Agu" "Sep" "Okt" "Nov" "Des"]
     :month-context-names      ["Januari" "Februari" "Maret" "April" "Mei" "Juni" "Juli" "Agustus" "September" "Oktober" "November" "Desember"]
     :weekday-context-abbr     ["Sen" "Sel" "Rab" "Kam" "Jum" "Sab" "Min"]
     :weekday-context-names    ["Senin" "Selasa" "Rabu" "Kamis" "Jumat" "Sabtu" "Minggu"]
     :date-patterns            (:short "yy/MM/dd" :medium "yyyy MMM d" :long "yyyy MMMM d" :full "EEEE, yyyy MMMM dd")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z"))
    (id-ID
     :date-patterns            (:short "dd/MM/yy" :medium "dd MMM yy" :long "dd MMMM yyyy" :full "EEEE dd MMMM yyyy")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss"))
    (is
     :decimal-separator        ?,
     :month-context-abbr       ["jan." "feb." "mar." "apr." "maí" "jún." "júl." "ágú." "sep." "okt." "nóv." "des."]
     :month-context-names      ["janúar" "febrúar" "mars" "apríl" "maí" "júní" "júlí" "ágúst" "september" "október" "nóvember" "desember"]
     :weekday-context-abbr     ["mán." "þri." "mið." "fim." "fös." "lau." "sun."]
     :weekday-context-names    ["mánudagur" "þriðjudagur" "miðvikudagur" "fimmtudagur" "föstudagur" "laugardagur" "sunnudagur"]
     :date-patterns            (:medium "d.M.yyyy" :long "d. MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z"))
    (it
     :decimal-separator        ?,
     :eras                     ["BC" "dopo Cristo"]
     :month-context-abbr       ["gen" "feb" "mar" "apr" "mag" "giu" "lug" "ago" "set" "ott" "nov" "dic"]
     :month-context-names      ["gennaio" "febbraio" "marzo" "aprile" "maggio" "giugno" "luglio" "agosto" "settembre" "ottobre" "novembre" "dicembre"]
     :weekday-context-abbr     ["lun" "mar" "mer" "gio" "ven" "sab" "dom"]
     :weekday-context-names    ["lunedì" "martedì" "mercoledì" "giovedì" "venerdì" "sabato" "domenica"]
     :month-standalone-names   ["Gennaio" "Febbraio" "Marzo" "Aprile" "Maggio" "Giugno" "Luglio" "Agosto" "Settembre" "Ottobre" "Novembre" "Dicembre"]
     :date-patterns            (:short "dd/MM/yy" :medium "d-MMM-yyyy" :long "d MMMM yyyy" :full "EEEE d MMMM yyyy")
     :time-patterns            (:short "H.mm" :medium "H.mm.ss" :long "H.mm.ss z"))
    (it-CH
     :date-patterns            (:short "dd.MM.yy" :medium "d-MMM-yyyy" :long "d. MMMM yyyy" :full "EEEE, d. MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z" :full "H.mm' h' z"))
    (ja
     :eras                     ["紀元前" "西暦"]
     :month-context-abbr       ["1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12"]
     :month-context-names      ["1月" "2月" "3月" "4月" "5月" "6月" "7月" "8月" "9月" "10月" "11月" "12月"]
     :weekday-context-abbr     ["月" "火" "水" "木" "金" "土" "日"]
     :weekday-context-names    ["月曜日" "火曜日" "水曜日" "木曜日" "金曜日" "土曜日" "日曜日"]
     :am-pm                    ["午前" "午後"]
     :date-patterns            (:short "yy/MM/dd" :medium "yyyy/MM/dd" :full "yyyy'年'M'月'd'日'")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss" :long "H:mm:ss z" :full "H'時'mm'分'ss'秒' z"))
    (ja-JP-u-ca-japanese-x-lvariant-JP
     :date-patterns            (:medium "Gy.MM.dd" :full "GGGGyyyy'年'M'月'd'日'"))
    (ko
     :eras                     ["기원전" "서기"]
     :month-context-abbr       ["1월" "2월" "3월" "4월" "5월" "6월" "7월" "8월" "9월" "10월" "11월" "12월"]
     :month-context-names      ["1월" "2월" "3월" "4월" "5월" "6월" "7월" "8월" "9월" "10월" "11월" "12월"]
     :weekday-context-abbr     ["월" "화" "수" "목" "금" "토" "일"]
     :weekday-context-names    ["월요일" "화요일" "수요일" "목요일" "금요일" "토요일" "일요일"]
     :am-pm                    ["오전" "오후"]
     :date-patterns            (:short "yy. M. d" :medium "yyyy. M. d" :long "yyyy'년' M'월' d'일' '('EE')'" :full "yyyy'년' M'월' d'일' EEEE")
     :time-patterns            (:short "a h:mm" :medium "a h:mm:ss" :long "a h'시' mm'분' ss'초'" :full "a h'시' mm'분' ss'초' z"))
    (lt
     :decimal-separator        ?,
     :eras                     ["pr.Kr." "po.Kr."]
     :month-context-abbr       ["Sau" "Vas" "Kov" "Bal" "Geg" "Bir" "Lie" "Rgp" "Rgs" "Spa" "Lap" "Grd"]
     :month-context-names      ["sausio" "vasaris" "kovas" "balandis" "gegužė" "birželis" "liepa" "rugpjūtis" "rugsėjis" "spalis" "lapkritis" "gruodis"]
     :weekday-context-abbr     ["Pr" "An" "Tr" "Kt" "Pn" "Št" "Sk"]
     :weekday-context-names    ["Pirmadienis" "Antradienis" "Trečiadienis" "Ketvirtadienis" "Penktadienis" "Šeštadienis" "Sekmadienis"]
     :month-standalone-abbr    ["Saus." "Vas." "Kov." "Bal." "Geg." "Bir." "Liep." "Rugp." "Rugs." "Spal." "Lapkr." "Gruod."]
     :month-standalone-names   ["Sausio" "Vasario" "Kovo" "Balandžio" "Gegužės" "Birželio" "Liepos" "Rugpjūčio" "Rugsėjo" "Spalio" "Lapkričio" "Gruodžio"]
     :date-patterns            (:short "yy.M.d" :medium "yyyy-MM-dd" :long "EEEE, yyyy, MMMM d")
     :time-patterns            (:short "HH.mm" :medium "HH.mm.ss" :long "HH.mm.ss z"))
    (lv
     :decimal-separator        ?,
     :eras                     ["pmē" "mē"]
     :month-context-abbr       ["janv." "febr." "marts" "apr." "maijs" "jūn." "jūl." "aug." "sept." "okt." "nov." "dec."]
     :month-context-names      ["janvāris" "februāris" "marts" "aprīlis" "maijs" "jūnijs" "jūlijs" "augusts" "septembris" "oktobris" "novembris" "decembris"]
     :weekday-context-abbr     ["P" "O" "T" "C" "Pk" "S" "Sv"]
     :weekday-context-names    ["pirmdiena" "otrdiena" "trešdiena" "ceturtdiena" "piektdiena" "sestdiena" "svētdiena"]
     :month-standalone-abbr    ["Jan" "Feb" "Mar" "Apr" "Maijs" "Jūn" "Jūl" "Aug" "Sep" "Okt" "Nov" "Dec"]
     :date-patterns            (:short "yy.d.M" :medium "yyyy.d.M" :long "EEEE, yyyy, d MMMM")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z"))
    (mk
     :decimal-separator        ?,
     :eras                     ["пр.н.е." "ае."]
     :month-context-abbr       ["јан." "фев." "мар." "апр." "мај." "јун." "јул." "авг." "септ." "окт." "ноем." "декем."]
     :month-context-names      ["јануари" "февруари" "март" "април" "мај" "јуни" "јули" "август" "септември" "октомври" "ноември" "декември"]
     :weekday-context-abbr     ["пон." "вт." "сре." "чет." "пет." "саб." "нед."]
     :weekday-context-names    ["понеделник" "вторник" "среда" "четврток" "петок" "сабота" "недела"]
     :date-patterns            (:short "d.M.yy" :medium "d.M.yyyy" :long "d, MMMM yyyy" :full "EEEE, d, MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:" :long "HH:mm:ss z"))
    (ms
     :eras                     ["BCE" "CE"]
     :month-context-abbr       ["Jan" "Feb" "Mac" "Apr" "Mei" "Jun" "Jul" "Ogos" "Sep" "Okt" "Nov" "Dis"]
     :month-context-names      ["Januari" "Februari" "Mac" "April" "Mei" "Jun" "Julai" "Ogos" "September" "Oktober" "November" "Disember"]
     :weekday-context-abbr     ["Isn" "Sel" "Rab" "Kha" "Jum" "Sab" "Ahd"]
     :weekday-context-names    ["Isnin" "Selasa" "Rabu" "Khamis" "Jumaat" "Sabtu" "Ahad"]
     :date-patterns            (:short "yy/MM/dd" :medium "yyyy MMM d" :long "yyyy MMMM d" :full "EEEE, yyyy MMMM dd")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z"))
    (ms-MY
     :date-patterns            (:short "dd/MM/yyyy" :medium "dd MMMM yyyy" :full "EEEE dd MMM yyyy")
     :time-patterns            (:short "h:mm" :medium "h:mm:ss a" :long "h:mm:ss a z"))
    (mt
     :eras                     ["QK" "WK"]
     :month-context-abbr       ["Jan" "Fra" "Mar" "Apr" "Mej" "Ġun" "Lul" "Aww" "Set" "Ott" "Nov" "Diċ"]
     :month-context-names      ["Jannar" "Frar" "Marzu" "April" "Mejju" "Ġunju" "Lulju" "Awwissu" "Settembru" "Ottubru" "Novembru" "Diċembru"]
     :weekday-context-abbr     ["Tne" "Tli" "Erb" "Ħam" "Ġim" "Sib" "Ħad"]
     :weekday-context-names    ["It-Tnejn" "It-Tlieta" "L-Erbgħa" "Il-Ħamis" "Il-Ġimgħa" "Is-Sibt" "Il-Ħadd"]
     :am-pm                    ["QN" "WN"]
     :date-patterns            (:short "dd/MM/yyyy" :medium "dd MMM yyyy" :long "d 'ta’' MMMM yyyy" :full "EEEE, d 'ta’' MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z"))
    (nl
     :decimal-separator        ?,
     :eras                     ["v. Chr." "n. Chr."]
     :month-context-abbr       ["jan" "feb" "mrt" "apr" "mei" "jun" "jul" "aug" "sep" "okt" "nov" "dec"]
     :month-context-names      ["januari" "februari" "maart" "april" "mei" "juni" "juli" "augustus" "september" "oktober" "november" "december"]
     :weekday-context-abbr     ["ma" "di" "wo" "do" "vr" "za" "zo"]
     :weekday-context-names    ["maandag" "dinsdag" "woensdag" "donderdag" "vrijdag" "zaterdag" "zondag"]
     :date-patterns            (:short "d-M-yy" :medium "d-MMM-yyyy" :long "d MMMM yyyy" :full "EEEE d MMMM yyyy")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss" :long "H:mm:ss z" :full "H:mm:ss' uur' z"))
    (nl-BE
     :date-patterns            (:short "d/MM/yy" :medium "d-MMM-yyyy" :long "d MMMM yyyy" :full "EEEE d MMMM yyyy")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss" :long "H:mm:ss z" :full "H.mm' u. 'z"))
    (nn-NO
     :weekday-context-abbr     ["må" "ty" "on" "to" "fr" "lau" "su"]
     :weekday-context-names    ["måndag" "tysdag" "onsdag" "torsdag" "fredag" "laurdag" "sundag"])
    (no
     :decimal-separator        ?,
     :month-context-abbr       ["jan" "feb" "mar" "apr" "mai" "jun" "jul" "aug" "sep" "okt" "nov" "des"]
     :month-context-names      ["januar" "februar" "mars" "april" "mai" "juni" "juli" "august" "september" "oktober" "november" "desember"]
     :weekday-context-abbr     ["ma" "ti" "on" "to" "fr" "lø" "sø"]
     :weekday-context-names    ["mandag" "tirsdag" "onsdag" "torsdag" "fredag" "lørdag" "søndag"]
     :date-patterns            (:short "dd.MM.yy" :medium "dd.MMM.yyyy" :long "d. MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z" :full "'kl 'HH.mm z"))
    (pl
     :decimal-separator        ?,
     :eras                     ["p.n.e." "n.e."]
     :month-context-abbr       ["sty" "lut" "mar" "kwi" "maj" "cze" "lip" "sie" "wrz" "paź" "lis" "gru"]
     :month-context-names      ["stycznia" "lutego" "marca" "kwietnia" "maja" "czerwca" "lipca" "sierpnia" "września" "października" "listopada" "grudnia"]
     :weekday-context-abbr     ["Pn" "Wt" "Śr" "Cz" "Pt" "So" "N"]
     :weekday-context-names    ["poniedziałek" "wtorek" "środa" "czwartek" "piątek" "sobota" "niedziela"]
     :month-standalone-names   ["styczeń" "luty" "marzec" "kwiecień" "maj" "czerwiec" "lipiec" "sierpień" "wrzesień" "październik" "listopad" "grudzień"]
     :date-patterns            (:short "yy-MM-dd" :medium "yyyy-MM-dd" :long "d MMMM yyyy" :full "EEEE, d MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z"))
    (pl-PL
     :date-patterns            (:short "dd.MM.yy" :medium "yyyy-MM-dd" :long "d MMMM yyyy" :full "EEEE, d MMMM yyyy"))
    (pt
     :decimal-separator        ?,
     :eras                     ["a.C." "d.C."]
     :month-context-abbr       ["jan" "fev" "mar" "abr" "mai" "jun" "jul" "ago" "set" "out" "nov" "dez"]
     :month-context-names      ["Janeiro" "Fevereiro" "Março" "Abril" "Maio" "Junho" "Julho" "Agosto" "Setembro" "Outubro" "Novembro" "Dezembro"]
     :weekday-context-abbr     ["Seg" "Ter" "Qua" "Qui" "Sex" "Sáb" "Dom"]
     :weekday-context-names    ["Segunda-feira" "Terça-feira" "Quarta-feira" "Quinta-feira" "Sexta-feira" "Sábado" "Domingo"]
     :date-patterns            (:short "dd-MM-yyyy" :medium "d/MMM/yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE, d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss" :long "H:mm:ss z" :full "HH'H'mm'm' z"))
    (pt-BR
     :date-patterns            (:short "dd/MM/yy" :medium "dd/MM/yyyy" :long "d' de 'MMMM' de 'yyyy" :full "EEEE, d' de 'MMMM' de 'yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "H'h'm'min's's' z" :full "HH'h'mm'min'ss's' z"))
    (ro
     :decimal-separator        ?,
     :eras                     ["d.C." "î.d.C."]
     :month-context-abbr       ["Ian" "Feb" "Mar" "Apr" "Mai" "Iun" "Iul" "Aug" "Sep" "Oct" "Nov" "Dec"]
     :month-context-names      ["ianuarie" "februarie" "martie" "aprilie" "mai" "iunie" "iulie" "august" "septembrie" "octombrie" "noiembrie" "decembrie"]
     :weekday-context-abbr     ["L" "Ma" "Mi" "J" "V" "S" "D"]
     :weekday-context-names    ["luni" "marţi" "miercuri" "joi" "vineri" "sâmbătă" "duminică"]
     :month-standalone-abbr    ["ian." "feb." "mar." "apr." "mai" "iun." "iul." "aug." "sept." "oct." "nov." "dec."]
     :date-patterns            (:medium "dd.MM.yyyy" :long "dd MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z"))
    (ru
     :decimal-separator        ?,
     :eras                     ["до н.э." "н.э."]
     :month-context-abbr       ["янв" "фев" "мар" "апр" "мая" "июн" "июл" "авг" "сен" "окт" "ноя" "дек"]
     :month-context-names      ["января" "февраля" "марта" "апреля" "мая" "июня" "июля" "августа" "сентября" "октября" "ноября" "декабря"]
     :weekday-context-abbr     ["Пн" "Вт" "Ср" "Чт" "Пт" "Сб" "Вс"]
     :weekday-context-names    ["понедельник" "вторник" "среда" "четверг" "пятница" "суббота" "воскресенье"]
     :month-standalone-abbr    ["Янв." "Февр." "Март" "Апр." "Май" "Июнь" "Июль" "Авг." "Сент." "Окт." "Нояб." "Дек."]
     :month-standalone-names   ["Январь" "Февраль" "Март" "Апрель" "Май" "Июнь" "Июль" "Август" "Сентябрь" "Октябрь" "Ноябрь" "Декабрь"]
     :date-patterns            (:short "dd.MM.yy" :medium "dd.MM.yyyy" :long "d MMMM yyyy 'г.'")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss" :long "H:mm:ss z"))
    (sk
     :decimal-separator        ?,
     :eras                     ["pred n.l." "n.l."]
     :month-context-abbr       ["jan" "feb" "mar" "apr" "máj" "jún" "júl" "aug" "sep" "okt" "nov" "dec"]
     :month-context-names      ["januára" "februára" "marca" "apríla" "mája" "júna" "júla" "augusta" "septembra" "októbra" "novembra" "decembra"]
     :weekday-context-abbr     ["Po" "Ut" "St" "Št" "Pi" "So" "Ne"]
     :weekday-context-names    ["Pondelok" "Utorok" "Streda" "Štvrtok" "Piatok" "Sobota" "Nedeľa"]
     :month-standalone-names   ["január" "február" "marec" "apríl" "máj" "jún" "júl" "august" "september" "október" "november" "december"]
     :date-patterns            (:medium "d.M.yyyy" :long "EEEE, yyyy, MMMM d")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss" :long "H:mm:ss z"))
    (sl
     :decimal-separator        ?,
     :eras                     ["pr.n.š." "po Kr."]
     :month-context-abbr       ["jan." "feb." "mar." "apr." "maj" "jun." "jul." "avg." "sep." "okt." "nov." "dec."]
     :month-context-names      ["januar" "februar" "marec" "april" "maj" "junij" "julij" "avgust" "september" "oktober" "november" "december"]
     :weekday-context-abbr     ["Pon" "Tor" "Sre" "Čet" "Pet" "Sob" "Ned"]
     :weekday-context-names    ["Ponedeljek" "Torek" "Sreda" "Četrtek" "Petek" "Sobota" "Nedelja"]
     :month-standalone-abbr    ["jan" "feb" "mar" "apr" "maj" "jun" "jul" "avg" "sep" "okt" "nov" "dec"]
     :date-patterns            (:short "d.M.y" :medium "d.M.yyyy" :long "dd. MMMM y" :full "EEEE, dd. MMMM y")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss" :long "H:mm:ss z"))
    (sq
     :decimal-separator        ?,
     :eras                     ["p.e.r." "n.e.r."]
     :month-context-abbr       ["Jan" "Shk" "Mar" "Pri" "Maj" "Qer" "Kor" "Gsh" "Sht" "Tet" "Nën" "Dhj"]
     :month-context-names      ["janar" "shkurt" "mars" "prill" "maj" "qershor" "korrik" "gusht" "shtator" "tetor" "nëntor" "dhjetor"]
     :weekday-context-abbr     ["Hën" "Mar" "Mër" "Enj" "Pre" "Sht" "Die"]
     :weekday-context-names    ["e hënë" "e martë" "e mërkurë" "e enjte" "e premte" "e shtunë" "e diel"]
     :am-pm                    ["PD" "MD"]
     :date-patterns            (:short "yy-MM-dd" :medium "yyyy-MM-dd")
     :time-patterns            (:short "h.mm.a" :medium "h:mm:ss.a" :long "h.mm.ss.a z"))
    (sr
     :decimal-separator        ?,
     :eras                     ["п. н. е." "н. е"]
     :month-context-abbr       ["јан" "феб" "мар" "апр" "мај" "јун" "јул" "авг" "сеп" "окт" "нов" "дец"]
     :month-context-names      ["јануар" "фебруар" "март" "април" "мај" "јун" "јул" "август" "септембар" "октобар" "новембар" "децембар"]
     :weekday-context-abbr     ["пон" "уто" "сре" "чет" "пет" "суб" "нед"]
     :weekday-context-names    ["понедељак" "уторак" "среда" "четвртак" "петак" "субота" "недеља"]
     :date-patterns            (:short "d.M.yy." :medium "dd.MM.yyyy." :full "EEEE, dd.MMMM.yyyy.")
     :time-patterns            (:short "HH.mm" :medium "HH.mm.ss" :long "HH.mm.ss z"))
    (sr-BA
     :month-context-names      ["јануар" "фебруар" "март" "април" "мај" "јуни" "јули" "август" "септембар" "октобар" "новембар" "децембар"]
     :weekday-context-abbr     ["пон" "уто" "сри" "чет" "пет" "суб" "нед"]
     :weekday-context-names    ["понедељак" "уторак" "сриједа" "четвртак" "петак" "субота" "недеља"]
     :date-patterns            (:short "yy-MM-dd" :medium "yyyy-MM-dd" :long "dd. MMMM yyyy." :full "EEEE, dd. MMMM yyyy.")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH.mm.ss z" :full "HH 'часова', mm 'минута', ss' секунди'"))
    (sr-Latn
     :eras                     ["p. n. e." "n. e"]
     :month-context-abbr       ["jan" "feb" "mar" "apr" "maj" "jun" "jul" "avg" "sep" "okt" "nov" "dec"]
     :month-context-names      ["januar" "februar" "mart" "april" "maj" "jun" "jul" "avgust" "septembar" "oktobar" "novembar" "decembar"]
     :weekday-context-abbr     ["pon" "uto" "sre" "čet" "pet" "sub" "ned"]
     :weekday-context-names    ["ponedeljak" "utorak" "sreda" "četvrtak" "petak" "subota" "nedelja"]
     :date-patterns            (:short "d.M.yy." :medium "dd.MM.y." :long "dd. MMMM y." :full "EEEE, dd. MMMM y.")
     :time-patterns            (:short "HH.mm" :medium "HH.mm.ss" :long "HH.mm.ss z" :full "HH.mm.ss zzzz"))
    (sr-Latn-BA
     :eras                     ["p. n. e." "n. e"]
     :month-context-abbr       ["jan" "feb" "mar" "apr" "maj" "jun" "jul" "avg" "sep" "okt" "nov" "dec"]
     :month-context-names      ["januar" "februar" "mart" "april" "maj" "jun" "jul" "avgust" "septembar" "oktobar" "novembar" "decembar"]
     :weekday-context-abbr     ["pon" "uto" "sre" "čet" "pet" "sub" "ned"]
     :weekday-context-names    ["ponedeljak" "utorak" "sreda" "četvrtak" "petak" "subota" "nedelja"]
     :date-patterns            (:short "d.M.yy." :medium "dd.MM.y." :long "dd. MMMM y." :full "EEEE, dd. MMMM y.")
     :time-patterns            (:short "HH.mm" :medium "HH.mm.ss" :long "HH.mm.ss z" :full "HH.mm.ss zzzz"))
    (sr-Latn-ME
     :eras                     ["p. n. e." "n. e"]
     :month-context-abbr       ["jan" "feb" "mar" "apr" "maj" "jun" "jul" "avg" "sep" "okt" "nov" "dec"]
     :month-context-names      ["januar" "februar" "mart" "april" "maj" "jun" "jul" "avgust" "septembar" "oktobar" "novembar" "decembar"]
     :weekday-context-abbr     ["pon" "uto" "sre" "čet" "pet" "sub" "ned"]
     :weekday-context-names    ["ponedeljak" "utorak" "sreda" "četvrtak" "petak" "subota" "nedelja"]
     :date-patterns            (:short "d.M.yy." :medium "dd.MM.y." :long "d.MM.yyyy." :full "EEEE, dd. MMMM y.")
     :time-patterns            (:short "HH.mm" :medium "HH.mm.ss" :long "HH.mm.ss z" :full "HH.mm.ss zzzz"))
    (sr-Latn-RS
     :eras                     ["p. n. e." "n. e"]
     :month-context-abbr       ["jan" "feb" "mar" "apr" "maj" "jun" "jul" "avg" "sep" "okt" "nov" "dec"]
     :month-context-names      ["januar" "februar" "mart" "april" "maj" "jun" "jul" "avgust" "septembar" "oktobar" "novembar" "decembar"]
     :weekday-context-abbr     ["pon" "uto" "sre" "čet" "pet" "sub" "ned"]
     :weekday-context-names    ["ponedeljak" "utorak" "sreda" "četvrtak" "petak" "subota" "nedelja"]
     :date-patterns            (:short "d.M.yy." :medium "dd.MM.y." :long "dd. MMMM y." :full "EEEE, dd. MMMM y.")
     :time-patterns            (:short "HH.mm" :medium "HH.mm.ss" :long "HH.mm.ss z" :full "HH.mm.ss zzzz"))
    (sv
     :decimal-separator        ?,
     :eras                     ["före Kristus" "efter Kristus"]
     :month-context-abbr       ["jan" "feb" "mar" "apr" "maj" "jun" "jul" "aug" "sep" "okt" "nov" "dec"]
     :month-context-names      ["januari" "februari" "mars" "april" "maj" "juni" "juli" "augusti" "september" "oktober" "november" "december"]
     :weekday-context-abbr     ["må" "ti" "on" "to" "fr" "lö" "sö"]
     :weekday-context-names    ["måndag" "tisdag" "onsdag" "torsdag" "fredag" "lördag" "söndag"]
     :am-pm                    ["fm" "em"]
     :date-patterns            (:short "yyyy-MM-dd" :medium "yyyy-MMM-dd" :long "'den 'd MMMM yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z" :full "'kl 'H:mm z"))
    (th
     :eras                     ["ปีก่อนคริสต์กาลที่" "ค.ศ."]
     :month-context-abbr       ["ม.ค." "ก.พ." "มี.ค." "เม.ย." "พ.ค." "มิ.ย." "ก.ค." "ส.ค." "ก.ย." "ต.ค." "พ.ย." "ธ.ค."]
     :month-context-names      ["มกราคม" "กุมภาพันธ์" "มีนาคม" "เมษายน" "พฤษภาคม" "มิถุนายน" "กรกฎาคม" "สิงหาคม" "กันยายน" "ตุลาคม" "พฤศจิกายน" "ธันวาคม"]
     :weekday-context-abbr     ["จ." "อ." "พ." "พฤ." "ศ." "ส." "อา."]
     :weekday-context-names    ["วันจันทร์" "วันอังคาร" "วันพุธ" "วันพฤหัสบดี" "วันศุกร์" "วันเสาร์" "วันอาทิตย์"]
     :am-pm                    ["ก่อนเที่ยง" "หลังเที่ยง"]
     :date-patterns            (:short "d/M/yyyy" :medium "d MMM yyyy" :long "d MMMM yyyy" :full "EEEE'ที่ 'd MMMM G yyyy")
     :time-patterns            (:short "H:mm' น.'" :medium "H:mm:ss" :long "H' นาฬิกา 'm' นาที'" :full "H' นาฬิกา 'm' นาที 'ss' วินาที'")
     :date-time-pattern-rule   (t . ", "))
    (tr
     :decimal-separator        ?,
     :eras                     ["MÖ" "MS"]
     :month-context-abbr       ["Oca" "Şub" "Mar" "Nis" "May" "Haz" "Tem" "Ağu" "Eyl" "Eki" "Kas" "Ara"]
     :month-context-names      ["Ocak" "Şubat" "Mart" "Nisan" "Mayıs" "Haziran" "Temmuz" "Ağustos" "Eylül" "Ekim" "Kasım" "Aralık"]
     :weekday-context-abbr     ["Pzt" "Sal" "Çar" "Per" "Cum" "Cmt" "Paz"]
     :weekday-context-names    ["Pazartesi" "Salı" "Çarşamba" "Perşembe" "Cuma" "Cumartesi" "Pazar"]
     :date-patterns            (:short "dd.MM.yyyy" :medium "dd.MMM.yyyy" :long "dd MMMM yyyy EEEE")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z"))
    (uk
     :decimal-separator        ?,
     :eras                     ["до н.е." "після н.е."]
     :month-context-abbr       ["січ." "лют." "бер." "квіт." "трав." "черв." "лип." "серп." "вер." "жовт." "лист." "груд."]
     :month-context-names      ["січня" "лютого" "березня" "квітня" "травня" "червня" "липня" "серпня" "вересня" "жовтня" "листопада" "грудня"]
     :weekday-context-abbr     ["пн" "вт" "ср" "чт" "пт" "сб" "нд"]
     :weekday-context-names    ["понеділок" "вівторок" "середа" "четвер" "п'ятниця" "субота" "неділя"]
     :month-standalone-abbr    ["січ" "лют" "бер" "квіт" "трав" "черв" "лип" "серп" "вер" "жовт" "лист" "груд"]
     :month-standalone-names   ["Січень" "Лютий" "Березень" "Квітень" "Травень" "Червень" "Липень" "Серпень" "Вересень" "Жовтень" "Листопад" "Грудень"]
     :date-patterns            (:short "dd.MM.yy" :medium "d MMM yyyy" :long "d MMMM yyyy" :full "EEEE, d MMMM yyyy р.")
     :time-patterns            (:short "H:mm" :medium "H:mm:ss" :long "H:mm:ss z"))
    (und
     :month-context-abbr       ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
     :month-context-names      ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"]
     :weekday-context-abbr     ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"]
     :weekday-context-names    ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"]
     :date-patterns            (:short "M/d/yy" :medium "MMM d, yyyy" :long "MMMM d, yyyy" :full "EEEE, MMMM d, yyyy")
     :time-patterns            (:short "h:mm a" :medium "h:mm:ss a" :long "h:mm:ss a z"))
    (vi
     :decimal-separator        ?,
     :eras                     ["tr. CN" "sau CN"]
     :month-context-abbr       ["thg 1" "thg 2" "thg 3" "thg 4" "thg 5" "thg 6" "thg 7" "thg 8" "thg 9" "thg 10" "thg 11" "thg 12"]
     :month-context-names      ["tháng một" "tháng hai" "tháng ba" "tháng tư" "tháng năm" "tháng sáu" "tháng bảy" "tháng tám" "tháng chín" "tháng mười" "tháng mười một" "tháng mười hai"]
     :weekday-context-abbr     ["Th 2" "Th 3" "Th 4" "Th 5" "Th 6" "Th 7" "CN"]
     :weekday-context-names    ["Thứ hai" "Thứ ba" "Thứ tư" "Thứ năm" "Thứ sáu" "Thứ bảy" "Chủ nhật"]
     :am-pm                    ["SA" "CH"]
     :date-patterns            (:short "dd/MM/yyyy" :medium "dd-MM-yyyy" :long "'Ngày' dd 'tháng' M 'năm' yyyy" :full "EEEE, 'ngày' dd MMMM 'năm' yyyy")
     :time-patterns            (:short "HH:mm" :medium "HH:mm:ss" :long "HH:mm:ss z")
     :date-time-pattern-rule   (nil . " "))
    (zh
     :eras                     ["公元前" "公元"]
     :month-context-abbr       ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"]
     :month-context-names      ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"]
     :weekday-context-abbr     ["星期一" "星期二" "星期三" "星期四" "星期五" "星期六" "星期日"]
     :weekday-context-names    ["星期一" "星期二" "星期三" "星期四" "星期五" "星期六" "星期日"]
     :am-pm                    ["上午" "下午"]
     :date-patterns            (:short "yy-M-d" :medium "yyyy-M-d" :long "yyyy'年'M'月'd'日'" :full "yyyy'年'M'月'd'日' EEEE")
     :time-patterns            (:short "ah:mm" :medium "H:mm:ss" :long "ahh'时'mm'分'ss'秒'" :full "ahh'时'mm'分'ss'秒' z"))
    (zh-HK
     :eras                     ["西元前" "西元"]
     :month-context-abbr       ["1月" "2月" "3月" "4月" "5月" "6月" "7月" "8月" "9月" "10月" "11月" "12月"]
     :weekday-context-abbr     ["一" "二" "三" "四" "五" "六" "日"]
     :date-patterns            (:short "yy'年'M'月'd'日'" :medium "yyyy'年'M'月'd'日'" :long "yyyy'年'MM'月'dd'日' EEEE")
     :time-patterns            (:short "ah:mm" :medium "ahh:mm:ss" :long "ahh'時'mm'分'ss'秒'" :full "ahh'時'mm'分'ss'秒' z"))
    (zh-SG
     :weekday-context-abbr     ["周一" "周二" "周三" "周四" "周五" "周六" "周日"]
     :date-patterns            (:short "dd/MM/yy" :medium "dd-MMM-yy" :long "dd MMM yyyy" :full "dd MMMM yyyy")
     :time-patterns            (:medium "a hh:mm" :long "a hh:mm:ss"))
    (zh-TW
     :eras                     ["西元前" "西元"]
     :month-standalone-abbr    ["1月" "2月" "3月" "4月" "5月" "6月" "7月" "8月" "9月" "10月" "11月" "12月"]
     :date-patterns            (:medium "yyyy/M/d" :long "yyyy'年'M'月'd'日'" :full "yyyy'年'M'月'd'日' EEEE")
     :time-patterns            (:short "a h:mm" :medium "a hh:mm:ss" :long "ahh'時'mm'分'ss'秒'" :full "ahh'時'mm'分'ss'秒' z"))))


(provide 'datetime)

;;; datetime.el ends here
