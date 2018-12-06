;;; datetime.el --- Parsing, formatting and matching timestamps  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 Paul Pogonyshev

;; Author:     Paul Pogonyshev <pogonyshev@gmail.com>
;; Maintainer: Paul Pogonyshev <pogonyshev@gmail.com>
;; Version:    0.6.1
;; Keywords:   lisp, i18n
;; Homepage:   https://github.com/doublep/datetime
;; Package-Requires: ((emacs "24.1") (extmap "1.0"))

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

;; Library for generic timestamp handling.  It is targeted at bulk
;; processing, therefore many functions are optimized for speed, but
;; not necessarily for ease of use.  For example, formatting is done
;; in two steps: first you need to generate a formatting function for
;; given pattern, and only using it obtain formatted strings.
;;
;; Package's main feature is timestamp parsing and formatting based on
;; Java pattern.  Arbitrary timezones and locales (i.e. not
;; necessarily those used by the system) are supported.  However,
;; specifying timezone in the input string to the parser function is
;; not implemented yet.  See functions `datetime-parser-to-float' and
;; `datetime-float-formatter' for details.
;;
;; Library also supports timestamp matching.  It can generate regular
;; expressions that match timestamps corresponding to given pattern.
;; These regular expressions can give false positives, but for most
;; purposes are good enough to detect timestamps in text files,
;; e.g. in various application logs.  See `datetime-matching-regexp'.
;;
;; Finally, library provides functions to select an appropriate
;; timestamp format for given locale.  For example, function
;; `datetime-locale-date-pattern' returns a Java pattern suitable for
;; formatting (or parsing) date only, without time part.  However, it
;; is not required that patterns are generated this way.


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
;;   am-pm (full | abbreviated)
;;
;;   hour-0-23 (NUMBER)
;;   hour-1-24 (NUMBER)
;;   hour-am-pm-0-11 (NUMBER)
;;   hour-am-pm-1-12 (NUMBER)
;;
;;   minute (NUMBER)
;;   second (NUMBER)
;;   millisecond (NUMBER)
;;   second-fractional (NUMBER)
;;       this is a generalization used internally: (second-fractional . 3)
;;       means millis, (second-fractional . 6) -- micros, and so on;
;;
;;   decimal-separator (PREFERRED)
;;       either dot or comma;
;;
;;   timezone (?) -- currently not supported further than pattern parsing


(require 'extmap)


(defun datetime--define-error (name message)
  (if (fboundp #'define-error)
      (define-error name message)
    (put name 'error-conditions `(,name error))
    (put name 'error-message    message)))

(datetime--define-error 'datetime-invalid-string       "Date-time string is invalid")
(datetime--define-error 'datetime-unsupported-timezone "Timezones are currently not supported")


(defconst datetime--directory (file-name-directory (or load-file-name (buffer-file-name))))

;; Extracted from Java using `dev/HarvestData.java'.  All patterns are
;; obviously of `java' type.
;;
;; There are many fallbacks involved to reduce size:
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
(defvar datetime--locale-extmap (extmap-init (expand-file-name "locale-data.extmap" datetime--directory) :auto-reload t))

;; Extracted from Java using `dev/HarvestData.java'.
(defvar datetime--timezone-extmap (extmap-init (expand-file-name "timezone-data.extmap" datetime--directory) :weak-data t :auto-reload t))

(defvar datetime--pattern-parsers '((parsed . (lambda (pattern options) pattern))
                                    (java   . datetime--parse-java-pattern)))

(defvar datetime--pattern-formatters '((parsed . (lambda (parts options) parts))
                                       (java   . datetime--format-java-pattern)))


(defgroup datetime nil
  "Date-time handling library."
  :group 'i18n)

(defcustom datetime-locale nil
  "Default locale for date-time formatting and parsing.
Leave unset to let the library auto-determine it from your OS
when necessary."
  :group 'datetime
  :type  'symbol)

(defcustom datetime-timezone nil
  "Default timezone for date-time formatting and parsing.
Leave unset to let the library auto-determine it from your OS
when necessary."
  :group 'datetime
  :type  'symbol)


(defun datetime--get-locale (options)
  (let ((locale (plist-get options :locale)))
    (if (eq locale 'system)
        (or (when datetime-locale
              (if (extmap-contains-key datetime--locale-extmap datetime-locale)
                  datetime-locale
                (warn "Locale `%S' (value of `datetime-locale' variable) is not known")
                nil))
            (let ((system-locale (or (getenv "LC_ALL") (getenv "LC_TIME") (getenv "LANG")))
                  as-symbol)
              (when system-locale
                (save-match-data
                  (when (string-match "^[a-zA-Z_]+" system-locale)
                    (setq as-symbol (intern (replace-regexp-in-string "_" "-" (match-string 0 system-locale) t t))))))
              (if (extmap-contains-key datetime--locale-extmap as-symbol)
                  as-symbol
                (error "Failed to determine system locale; consider customizing `datetime-locale' variable"))))
      (or locale 'en))))

(defun datetime--get-timezone (options)
  (let ((timezone (plist-get options :timezone)))
    (if (eq timezone 'system)
        (or (when datetime-timezone
              (if (extmap-contains-key datetime--timezone-extmap datetime-timezone)
                  datetime-timezone
                (warn "Timezone `%S' (value of `datetime-timezone' variable) is not known")
                nil))
            (datetime--determine-system-timezone))
      (or timezone 'UTC))))

(defun datetime--determine-system-timezone ()
  ;; Unfortunately, there is no simple way.  `current-time-zone' might
  ;; look as one, but it often returns a name that is not understood
  ;; by this library.  These heuristics are certainly incomplete.
  (save-match-data
    (let ((system-timezone (intern (or (pcase system-type
                                         ((or `gnu `gnu/linux `gnu/kfreebsd)
                                          (or ;; For Debian-based distros.
                                             (when (file-exists-p "/etc/timezone")
                                               (condition-case nil
                                                   (with-temp-buffer
                                                     (insert-file-contents-literally "/etc/timezone")
                                                     (when (looking-at "\\S-+")
                                                       (match-string-no-properties 0)))
                                                 (error)))
                                             ;; Freedesktop standard (?).
                                             (let ((locatime (file-symlink-p "/etc/localtime")))
                                               (when (and locatime (string-match "/usr/share/zoneinfo/\\(.+\\)" locatime))
                                                 (match-string-no-properties 1 locatime))))))
                                       (cadr (current-time-zone))
                                       "?"))))
      (if (extmap-contains-key datetime--timezone-extmap system-timezone)
          system-timezone
        (error "Failed to determine system timezone; consider customizing `datetime-timezone' variable")))))


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
               (push (pcase character
                       ((or ?G ?a)
                        (cons (pcase character
                                (?G 'era)
                                (?a 'am-pm))
                              (if (>= num-repetitions 4) 'full 'abbreviated)))
                       ((or ?y ?Y)
                        (cons (if (= character ?y) 'year 'year-for-week)
                              (pcase num-repetitions
                                (1 'add-century-when-parsing)
                                (2 'always-two-digits)
                                (_ num-repetitions))))
                       ((or ?M ?L)
                        (if (<= num-repetitions 2)
                            (cons 'month num-repetitions)
                          (cons (if (= character ?M) 'month-context-name 'month-standalone-name)
                                (if (>= num-repetitions 4) 'full 'abbreviated))))
                       ((or ?E ?c)
                        (cons (if (= character ?E) 'weekday-context-name 'weekday-standalone-name)
                              (if (>= num-repetitions 4) 'full 'abbreviated)))
                       (?e (if (<= num-repetitions 2)
                               (cons 'weekday num-repetitions)
                             (cons 'weekday-context-name (if (>= num-repetitions 4) 'full 'abbreviated))))
                       (?w (cons 'week-in-year     num-repetitions))
                       (?W (cons 'week-in-month    num-repetitions))
                       (?D (cons 'day-in-year      num-repetitions))
                       (?d (cons 'day-in-month     num-repetitions))
                       (?F (cons 'weekday-in-month num-repetitions))
                       (?u (cons 'weekday          num-repetitions))
                       (?H (cons 'hour-0-23        num-repetitions))
                       (?k (cons 'hour-1-24        num-repetitions))
                       (?K (cons 'hour-am-pm-0-11  num-repetitions))
                       (?h (cons 'hour-am-pm-1-12  num-repetitions))
                       (?m (cons 'minute           num-repetitions))
                       (?s (cons 'second           num-repetitions))
                       (?S (cons (if (plist-get options :second-fractional-extension) 'second-fractional 'millisecond)
                                 num-repetitions))
                       (?z (cons 'timezone         'general))
                       (?Z (cons 'timezone         'rfc-822))
                       (?X (cons 'timezone         'iso-8601))
                       (_
                        (error "Illegal pattern character `%c'" character)))
                     parts))
              (t
               (if (and (or (= character ?.) (= character ?,))
                        (plist-get options :any-decimal-separator)
                        (eq (car-safe (car parts)) 'second)
                        (< scan length) (= (aref pattern scan) ?S))
                   (push (cons 'decimal-separator character) parts)
                 (datetime--extend-as-is-part parts (string character)))))))
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
                          ((or `month-context-name `month-standalone-name `weekday-context-name `weekday-standalone-name)
                           (cons (pcase type
                                   (`month-context-name      ?M)
                                   (`month-standalone-name   ?L)
                                   (`weekday-context-name    ?E)
                                   (`weekday-standalone-name ?c))
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
                          (`hour-am-pm-0-11   (cons ?K details))
                          (`hour-am-pm-1-12   (cons ?h details))
                          (`minute            (cons ?m details))
                          (`second            (cons ?s details))
                          (`decimal-separator details)
                          (`millisecond       (cons ?S details))
                          (`second-fractional (if (plist-get options :second-fractional-extension)
                                                  (cons ?S details)
                                                (error "`second-fractional' extension is not enabled")))
                          (`am-pm             "a")
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


(defsubst datetime--gregorian-leap-year-mod-400-p (year-mod-400)
  (aref (eval-when-compile (let (result)
                             (dotimes (year 400)
                               (push (and (= (% year 4) 0) (or (/= (% year 100) 0) (= (% year 400) 0))) result))
                             (with-no-warnings (apply (if (fboundp #'bool-vector) #'bool-vector #'vector) (nreverse result)))))
        year-mod-400))

(defsubst datetime--gregorian-leap-year-p (year)
  (datetime--gregorian-leap-year-mod-400-p (mod year 400)))

(defconst datetime--gregorian-cumulative-year-days (let ((days 0)
                                                         result)
                                                     (dotimes (year 400)
                                                       (push days result)
                                                       (setq days (+ days (if (datetime--gregorian-leap-year-mod-400-p year) 366 365))))
                                                     (push days result)
                                                     (apply #'vector (nreverse result))))
(defconst datetime--gregorian-days-in-400-years    (aref datetime--gregorian-cumulative-year-days 400))
(defconst datetime--gregorian-days-in-1970-years   (+ (* datetime--gregorian-days-in-400-years (/ 1970 400))
                                                      (aref datetime--gregorian-cumulative-year-days (% 1970 400))))

;; Conveniently, this also has a loop size of 400 years.
(defconst datetime--gregorian-first-day-of-year (let ((first-day 5)
                                                      result)
                                                  (dotimes (year 400)
                                                    (push first-day result)
                                                    (setq first-day (% (+ first-day (if (datetime--gregorian-leap-year-mod-400-p year) 2 1)) 7)))
                                                  (apply #'vector (nreverse result))))

(defconst datetime--average-seconds-in-year (/ (* datetime--gregorian-days-in-400-years 24 60 60) 400))

;; For non-leap years.
(defconst datetime--gregorian-month-days            [31 28 31 30 31 30 31 31 30 31 30 31])
(defconst datetime--gregorian-cumulative-month-days (let ((days   0)
                                                          (result (list 0)))
                                                      (dolist (month-days (append datetime--gregorian-month-days nil))
                                                        (push (setq days (+ days month-days)) result))
                                                      (apply #'vector (nreverse result))))


(defsubst datetime--digits-format (num-repetitions)
  (if (> num-repetitions 1) (format "%%0%dd" num-repetitions) "%d"))

(defun datetime-float-formatter (type pattern &rest options)
  "Return a function that formats date-time expressed as a float.
The returned function accepts single argument---a floating-point
number---and returns a string with given time formatted according
to given PATTERN of given TYPE.  Rest of the arguments must be a
property list, i.e. keywords interleaved with values.

OPTIONS should be any keyword arguments understood by
`datetime-recode-pattern' plus any from the list below, specific
to this function.

  :locale

    Locale (language) used for month, weekday etc. names.  Always
    defaults to English, even if system locale is different.  You
    can use special value \\='system to let the library find it.

  :timezone

    Timezone for time values to be formatted in.  Always defaults
    to UTC.  You can use special value \\='system to let the
    library find the value, suitable for the current machine.

  :debug

    Don't byte-compile the formatter function, leave it in the
    form of a Lisp lambda."
  (let* ((locale        (datetime--get-locale options))
         (timezone      (datetime--get-timezone options))
         (timezone-data (or (extmap-get datetime--timezone-extmap timezone t)
                            (error "Unknown timezone `%s'" timezone)))
         need-year need-month need-weekday need-day need-hour need-time
         format-parts
         format-arguments)
    (dolist (part (datetime--parse-pattern type pattern options))
      (if (stringp part)
          (push (replace-regexp-in-string "%" "%%" part t t) format-parts)
        (let ((type    (car part))
              (details (cdr part)))
          (pcase type
            (`era
             (setq need-year t)
             (push "%s" format-parts)
             (push `(aref ,(datetime-locale-field locale :eras) (if (> year 0) 1 0)) format-arguments))
            (`year
             (setq need-year t)
             (push (pcase details
                     (`add-century-when-parsing "%d")
                     (`always-two-digits        "%02d")
                     (_                         (datetime--digits-format details)))
                   format-parts)
             (push (if (eq type 'year)
                       `(if (> year 0) year (- 1 year))
                     (error "Formatting `%s' is currently not implemented" type))
                   format-arguments)
             (when (eq details 'always-two-digits)
               (setcar format-arguments `(mod ,(car format-arguments) 100))))
            (`year-for-week
             (error "Formatting `%s' is currently not implemented" type))
            (`month
             (setq need-month t)
             (push (datetime--digits-format details) format-parts)
             (push `(1+ month) format-arguments))
            ((or `month-context-name `month-standalone-name)
             (setq need-month t)
             (push "%s" format-parts)
             (push `(aref ,(datetime-locale-field locale
                                                  (if (eq type 'month-context-name)
                                                      (if (eq details 'full) :month-context-names    :month-context-abbr)
                                                    (if   (eq details 'full) :month-standalone-names :month-standalone-abbr)))
                          month)
                   format-arguments))
            (`week-in-year
             (error "Formatting `%s' is currently not implemented" type))
            (`week-in-month
             (error "Formatting `%s' is currently not implemented" type))
            (`day-in-year
             (setq need-day t)
             (push (datetime--digits-format details) format-parts)
             (push `(1+ year-day) format-arguments))
            (`day-in-month
             (setq need-day t)
             (push (datetime--digits-format details) format-parts)
             (push `(1+ day) format-arguments))
            (`weekday-in-month
             (error "Formatting `%s' is currently not implemented" type))
            (`weekday
             (setq need-weekday t)
             (push (datetime--digits-format details) format-parts)
             (push `(1+ weekday) format-arguments))
            ((or `weekday-context-name `weekday-standalone-name)
             (setq need-weekday t)
             (push "%s" format-parts)
             (push `(aref ,(datetime-locale-field locale
                                                  (if (eq type 'weekday-context-name)
                                                      (if (eq details 'full) :weekday-context-names    :weekday-context-abbr)
                                                    (if   (eq details 'full) :weekday-standalone-names :weekday-standalone-abbr)))
                          weekday)
                   format-arguments))
            (`am-pm
             (setq need-hour t)
             (push "%s" format-parts)
             (push `(aref ,(datetime-locale-field locale :am-pm) (if (>= hour 12) 1 0)) format-arguments))
            ((or `hour-0-23 `hour-1-24 `hour-am-pm-0-11 `hour-am-pm-1-12)
             (setq need-hour t)
             (push (datetime--digits-format details) format-parts)
             (push (pcase type
                     (`hour-0-23       `hour)
                     (`hour-1-24       `(if (> hour 0) hour 24))
                     (`hour-am-pm-0-11 `(% hour 12))
                     (`hour-am-pm-1-12 `(let ((hour (% hour 12))) (if (> hour 0) hour 12))))
                   format-arguments))
            (`minute
             (setq need-time t)
             (push (datetime--digits-format details) format-parts)
             (push `(/ (mod time ,(* 60 60)) 60) format-arguments))
            (`second
             (setq need-time t)
             (push (datetime--digits-format details) format-parts)
             (push `(mod time 60) format-arguments))
            ((or `millisecond `second-fractional)
             (setq need-time t)
             (push (datetime--digits-format details) format-parts)
             (let ((scale (if (eq type 'millisecond) 1000 (expt 10 details))))
               (push `(mod (* time ,scale) ,scale) format-arguments)))
            (`timezone
             (signal 'datetime-unsupported-timezone nil))
            (_ (error "Unexpected value %s" type))))))
    ;; 400 is the size of Gregorian calendar leap year loop.
    (let* ((days-in-400-years datetime--gregorian-days-in-400-years)
           (formatter `(lambda (date-time)
                         (setq date-time ,(pcase timezone-data
                                            (`(,constant-offset)
                                             (if (/= constant-offset 0)
                                                 `(+ (float date-time) ,constant-offset)
                                               `(float date-time)))
                                            (_
                                             `(datetime--convert-to-utc-float (float date-time) ,(datetime--macroexp-quote timezone-data)))))
                         (let* (,@(when (or need-year need-month need-weekday need-day)
                                    ;; Date in days, rebased from 1970-01-01 to 0000-01-01.
                                    `((date-0           (+ (floor (/ date-time ,(* 24 60 60))) ,datetime--gregorian-days-in-1970-years))
                                      (date-%-400-years (mod date-0 ,days-in-400-years))
                                      (full-400-years   (/ (- date-0 date-%-400-years) ,days-in-400-years))
                                      (year-%-400       (/ date-%-400-years 366))
                                      (year             (+ (* full-400-years 400)
                                                           (progn
                                                             (if (< date-%-400-years (aref ,datetime--gregorian-cumulative-year-days (1+ year-%-400)))
                                                                 year-%-400
                                                               (setq year-%-400 (1+ year-%-400))))))))
                                ,@(when (or need-month need-weekday need-day)
                                    `((year-day         (- date-0 (* full-400-years ,days-in-400-years) (aref ,datetime--gregorian-cumulative-year-days (mod year 400))))
                                      (day              year-day)
                                      (month            (let ((july-days (if (datetime--gregorian-leap-year-mod-400-p year-%-400)
                                                                             ,(+ 31 29 31 30 31 30)
                                                                           ,(+ 31 28 31 30 31 30))))
                                                          (if (>= day july-days)
                                                              (if (>= (setq day (- day july-days)) ,(+ 31 31 30))
                                                                  (cond ((< (setq day (- day ,(+ 31 31 30))) 31)  9)           ; October
                                                                        ((< (setq day (- day 31)) 30)            10)           ; November
                                                                        (t  (setq day (- day 30))                11))          ; December
                                                                (cond ((< day 31)                                 6)           ; July
                                                                      ((< (setq day (- day 31)) 31)               7)           ; August
                                                                      (t  (setq day (- day 31))                   8)))         ; September
                                                            (let ((february-days (- july-days ,(+ 31 30 31 30))))
                                                              (cond ((< day february-days)
                                                                     (cond ((< day 31)                            0)           ; January
                                                                           (t (setq day (- day 31))               1)))         ; February
                                                                    ((< (setq day (- day february-days)) ,(+ 31 30))
                                                                     (cond ((< day 31)                            2)           ; March
                                                                           (t (setq day (- day 31))               3)))         ; April
                                                                    (t
                                                                     (cond ((< (setq day (- day ,(+ 31 30))) 31)  4)           ; May
                                                                           (t (setq day (- day 31))               5))))))))))  ; June
                                ,@(when need-weekday
                                    `((weekday          (% (+ year-day (aref ,datetime--gregorian-first-day-of-year (mod year 400))) 7))))
                                ,@(when (or need-time need-hour)
                                    `((time (mod date-time ,(* 24 60 60)))))
                                ,@(when need-hour
                                    `((hour (/ (mod (floor time) ,(* 24 60 60)) ,(* 60 60))))))
                           (format ,(apply #'concat (nreverse format-parts)) ,@(nreverse format-arguments))))))
      (if (plist-get options :debug)
          formatter
        (byte-compile formatter)))))

;; Not available on older Emacs versions.  Copied from recent Emacs source.
(defun datetime--macroexp-quote (v)
  (if (and (not (consp v))
	   (or (keywordp v)
	       (not (symbolp v))
	       (memq v '(nil t))))
      v
    (list 'quote v)))

(defun datetime--convert-to-utc-float (date-time timezone-data)
  (let ((year-offset          (floor (/ (- date-time (car timezone-data)) datetime--average-seconds-in-year)))
        (all-year-transitions (nth 1 timezone-data)))
    (if (>= year-offset 0)
        (let* ((year-transitions (or (when (< year-offset (length all-year-transitions))
                                       (aref all-year-transitions year-offset))
                                     (datetime--calculate-year-transitions timezone-data year-offset)))
               (offset           (pop year-transitions)))
          (when year-transitions
            (let ((offset-in-year (floor (- date-time (car timezone-data) (* year-offset datetime--average-seconds-in-year)))))
              (while (and (>= offset-in-year (car year-transitions))
                          (setq offset           (cadr year-transitions)
                                year-transitions (cddr year-transitions))))))
          (+ date-time offset))
      ;; Offset before the very first transition.
      (+ date-time (car (aref all-year-transitions 0))))))

;; 146097 is the value of `datetime--gregorian-days-in-400-years'.
;; `eval-when-compile' doesn't allow referring to the mnemonic name.
;;
;; Likewise, 135140 is the value of
;; `(aref datetime--gregorian-cumulative-year-days (mod 1970 400))'.
(defsubst datetime--start-of-day (year year-day)
  (* (eval-when-compile (* 24 60 60.0))
     (+ (* (floor (/ (float year) 400)) (eval-when-compile 146097))
        (aref datetime--gregorian-cumulative-year-days (mod year 400))
        (eval-when-compile (- (+ (* (floor (/ (float 1970) 400)) 146097) 135140)))
        year-day)))

(defun datetime--calculate-year-transitions (timezone-data year-offset)
  (let* ((all-year-transitions (nth 1 timezone-data))
         (num-years            (length all-year-transitions))
         transitions)
    (when (>= year-offset num-years)
      (setcar (cdr timezone-data) (setq all-year-transitions (vconcat all-year-transitions (make-vector (max (1+ (- year-offset num-years)) (/ num-years 2) 10) nil)))))
    (let ((year      (+ (nth 2 timezone-data) year-offset))
          (year-base (+ (nth 0 timezone-data) (* year-offset datetime--average-seconds-in-year))))
      (dolist (rule (nth 3 timezone-data))
        (let* ((month           (plist-get rule :month))
               (day-of-month    (plist-get rule :day-of-month))
               (effective-month (if (< day-of-month 0) month (1- month)))
               (day-of-week     (plist-get rule :day-of-week))
               (year-day        (+ (aref datetime--gregorian-cumulative-month-days effective-month)
                                   (if (and (>= effective-month 2) (datetime--gregorian-leap-year-p year)) 1 0)
                                   day-of-month -1))
               (offset-before   (plist-get rule :before)))
          (unless transitions
            (push offset-before transitions))
          (when day-of-week
            (let ((current-weekday (% (+ year-day (aref datetime--gregorian-first-day-of-year (mod year 400))) 7)))
              (setq year-day (if (< day-of-month 0) (- year-day (mod (- day-of-week current-weekday) 7)) (+ year-day (mod (- day-of-week current-weekday) 7))))))
          (when (plist-get rule :end-of-day)
            (setq year-day (1+ year-day)))
          (push (- (+ (datetime--start-of-day year year-day) (plist-get rule :time))
                   (pcase (plist-get rule :time-definition)
                     (`utc      0)
                     (`standard (plist-get rule :standard-offset))
                     (`wall     offset-before)
                     (type      (error "Unhandled time definition type `%s'" type)))
                   year-base)
                transitions)
          (push (plist-get rule :after) transitions))))
    (aset all-year-transitions year-offset (nreverse transitions))))


;; There is horribly unreadable level of backquoting/unquoting inside this macro...
(defmacro datetime--parser-computation (pattern value-name validating min max &rest arguments)
  (let ((computations    (make-symbol "$computations"))
        (computation     (make-symbol "$computation"))
        (range-validated (make-symbol "$range-validated"))
        loops)
    (setq arguments (reverse arguments))
    (while arguments
      (let* ((set             (pop arguments))
             (part-indices    (nth 0 set))
             (builder         (nth 1 set))
             (self-validating (nth 2 set))
             (new-loop        `(while ,part-indices
                                 (push (,(if (consp builder) (car builder) builder) (pop ,part-indices) ,@(when (consp builder) (cdr builder)))
                                       ,computations))))
        (when (and self-validating (or min max))
          (setq new-loop `(progn (when ,part-indices (setq ,range-validated t)) ,new-loop)))
        (setq loops
              (if loops
                  `(,@(macroexp-unprogn new-loop)
                    (when (or ,validating (null ,computations))
                      ,@loops))
                `(,new-loop)))))
    `(let (,computations
           ,@(when (or min max) `(,range-validated)))
       ,@loops
       (when ,computations
         (let ((,computation (if (cdr ,computations)
                                 `(let ((x ,(car ,computations)))
                                    ,@(mapcar (lambda (computation)
                                                `(unless (eq ,computation x)
                                                   (signal 'datetime-invalid-string (list string ,,pattern ,,(format "inconsistent %s" value-name)))))
                                              (cdr ,computations))
                                    x)
                               (car ,computations))))
           ,@(when (or min max)
               `((when (and ,validating (not ,range-validated))
                   (setq ,computation `(let ((x ,,computation))
                                         (unless ,',(cond ((and min max) `(<= ,min x ,max))
                                                          (min           `(<= ,min x))
                                                          (t             `(<= x ,max)))
                                           (signal 'datetime-invalid-string (list string ,,pattern ,,(format "%s is out of range" value-name))))
                                         x)))))
           ,computation)))))

(defun datetime-parser-to-float (type pattern &rest options)
  "Return a function that parses date-time according to the PATTERN.
Argument TYPE defines how the pattern should be interpreted, see
library documentation.  Rest of the arguments must be a property
list, i.e. keywords interleaved with values.

The resulting function transforms a string to a float number of
seconds since the epoch (0:00:00 of 1st of January 1970), in UTC
timezone.  The function is byte-compiled, unless you specify
:debug option.  Behavior for invalid strings depends on whether
:non-validating option is specified.

OPTIONS should be any keyword arguments understood by
`datetime-recode-pattern' plus any from the list below, specific
to this function.  Default value of keyword arguments is nil
unless specified otherwise.

  :locale

    Locale (language) used for month, weekday etc. names.  Always
    defaults to English, even if system locale is different.

  :timezone

    The timezone for parsing input strings in.  Always defaults
    to UTC.  You can use special value \\='system to let the
    library find the value suitable for the current machine.

    If input string explicitly specifies a timezone (i.e. if
    PATTERN does), this value is essentially ignored.

  :defaults

    A plist of values for those date/time part that are not
    specified in the input.  Accepted keys:

      year   -- defaults to 1970 (the year of UNIX epoch);
      month  -- must be in the range 1 to 12, defaults to 1;
      day    -- must be in the range 1 to 31, defaults to 1; will
                cause validation errors if used and is too large
                for the parsed month and year;
      hour   -- must be in the range 0 to 23, defaults to 0;
      minute -- must be in the range 0 to 59, defaults to 0;
      second -- must be in the range 0 to 59, defaults to 0.

    Note that the set of accepted keys is substantially smaller
    than that of all understood pattern parts.  For example, eras
    are not supported (use negative years), or 12-hour clock time
    (convert to 24-hour).

    If PATTERN specifies a way certain value is encoded in input
    strings, corresponding value from this plist is ignored.

  :non-validating

    Validating parsers always signal a `datetime-invalid-string'
    error if given strings that cannot be parsed or contain
    invalid values like 30th of February.  Non-validating parsers
    can either return unspecified numeric result or signal
    arbitrary errors in such cases.  (But it is guaranteed they
    don't fall into an infinite loop or perform any other
    action.)

    Non-validating parsers are more efficient, for some patterns
    considerably so.

  :case-insensitive

    Accept text in any case.  This works both for literal text
    included in the pattern and for month etc. names.

  :lax-whitespace

    Match any whitespace in PATTERN against any whitespace in
    date-time string.  For this purpose \"whitespace\" is defined
    as space and tab characters only.

  :accept-leading-space

    Make variable-width numbers (e.g. day number without leading
    zero) match also if there is a leading space.

  :debug

    Don't byte-compile the parser function, leave it in the form
    of Lisp lambda."
  (let* ((locale           (datetime--get-locale options))
         (timezone         (datetime--get-timezone options))
         (timezone-data    (or (extmap-get datetime--timezone-extmap timezone t)
                               (error "Unknown timezone `%s'" timezone)))
         (defaults         (plist-get options :defaults))
         (validating       (not (plist-get options :non-validating)))
         (case-insensitive (and (plist-get options :case-insensitive) t))
         (lax-whitespace   (plist-get options :lax-whitespace))
         (part-index       0)
         regexp-parts
         ;; To handle excessive information patterns (e.g. "Mon 16 Sep 2018" is excessive,
         ;; since day of the week can be found from the day of the year), we keep track of
         ;; all the various groups and decide which to use later.  Groups are also stored
         ;; as a list (or alist in certain cases), though this is hardly necessary, since
         ;; normally patterns wouldn't repeat the same group.
         era-part-indices
         year-part-indices
         month-number-part-indices
         month-name-part-indices
         day-of-month-part-indices
         am-pm-part-indices
         hour-0-23-part-indices
         hour-1-24-part-indices
         hour-am-pm-1-12-part-indices
         hour-am-pm-0-11-part-indices
         minute-part-indices
         second-part-indices
         second-fractional-part-indices
         have-case-sensitive-parts)
    (dolist (part (datetime--parse-pattern type pattern options))
      (if (stringp part)
          (let ((quoted (regexp-quote part)))
            (when (not (or have-case-sensitive-parts (string= (upcase part) (downcase part))))
              (setq have-case-sensitive-parts t))
            (push (if lax-whitespace
                      (replace-regexp-in-string (rx (1+ (any blank))) (rx (1+ (any blank))) quoted t t)
                    quoted)
                  regexp-parts))
        (let* ((type    (car part))
               (details (cdr part))
               (regexp  (pcase type
                          (`era                   (when (or validating (null era-part-indices))
                                                    (push part-index era-part-indices))
                                                  (datetime-locale-field locale :eras))
                          (`year
                           (when (or validating (null year-part-indices))
                             (push (cons part-index details) year-part-indices))
                           (cond ((or (memq details '(1 add-century-when-parsing)) (not (plist-get options :require-leading-zeros)))
                                  (rx (1+ (any "0-9"))))
                                 ((memq details '(2 always-two-digits))
                                  (rx (any "0-9") (1+ (any "0-9"))))
                                 (t
                                  (format "[0-9]\\{%d\\}[0-9]+" (1- details)))))
                          (`year-for-week         (error "Parsing `%s' is currently not implemented" type))
                          (`month                 (when (or validating (null month-number-part-indices))
                                                    (push part-index month-number-part-indices))
                                                  12)
                          (`month-context-name    (let ((field (if (eq details 'abbreviated) :month-context-abbr :month-context-names)))
                                                    (when (or validating (null month-name-part-indices))
                                                      (push (cons part-index field) month-name-part-indices))
                                                    (datetime-locale-field locale field)))
                          (`month-standalone-name (let ((field (if (eq details 'abbreviated) :month-standalone-abbr :month-standalone-names)))
                                                    (when (or validating (null month-name-part-indices))
                                                      (push (cons part-index field) month-name-part-indices))
                                                    (datetime-locale-field locale field)))
                          (`week-in-year         (error "Parsing `%s' is currently not implemented" type))
                          (`week-in-month        (error "Parsing `%s' is currently not implemented" type))
                          (`day-in-month         (when (or validating (null day-of-month-part-indices))
                                                   (push part-index day-of-month-part-indices))
                                                 31)
                          (`weekday-in-month     (error "Parsing `%s' is currently not implemented" type))
                          (`weekday               7)
                          (`weekday-context-name
                           (datetime-locale-field locale (if (eq details 'abbreviated) :weekday-context-abbr    :weekday-context-names)))
                          (`weekday-standalone-name
                           (datetime-locale-field locale (if (eq details 'abbreviated) :weekday-standalone-abbr :weekday-standalone-names)))
                          (`am-pm                (when (or validating (null am-pm-part-indices))
                                                   (push part-index am-pm-part-indices))
                                                 (datetime-locale-field locale :am-pm))
                          (`hour-0-23            (when (or validating (null hour-0-23-part-indices))
                                                   (push part-index hour-0-23-part-indices))
                                                 23)
                          (`hour-1-24            (when (or validating (null hour-1-24-part-indices))
                                                   (push part-index hour-1-24-part-indices))
                                                 24)
                          (`hour-am-pm-0-11      (when (or validating (null hour-am-pm-0-11-part-indices))
                                                   (push part-index hour-am-pm-0-11-part-indices))
                                                 11)
                          (`hour-am-pm-1-12      (when (or validating (null hour-am-pm-1-12-part-indices))
                                                   (push part-index hour-am-pm-1-12-part-indices))
                                                 12)
                          (`minute               (when (or validating (null minute-part-indices))
                                                   (push part-index minute-part-indices))
                                                 59)
                          (`second               (when (or validating (null second-part-indices))
                                                   (push part-index second-part-indices))
                                                  59)
                          (`decimal-separator    (rx (or "." ",")))
                          (`millisecond          (push (cons part-index 1000.0) second-fractional-part-indices)
                                                 (rx (any "0-9") (any "0-9") (any "0-9")))
                          (`second-fractional    (push (cons part-index (expt 10.0 details)) second-fractional-part-indices)
                                                 (apply #'concat (make-list details (rx (any "0-9")))))
                          (`timezone
                           (signal 'datetime-unsupported-timezone nil))
                          (_ (error "Unexpected value %s" type)))))
          (push (cond ((integerp regexp)
                       ;; REGEXP is really the maximum value of this one- or two-digit
                       ;; number.  However, we don't include it in the regexp in most of
                       ;; the cases (unlike in `datetime-matching-regexp').
                       (if (<= regexp 9)
                           (format "0*[1-%d]" regexp)
                         (cond ((and (= details 1) (plist-get options :accept-leading-space))
                                (format "[ 0-%d]?[0-9]" (/ regexp 10)))
                               ((>= regexp 20)
                                (format "0*[1-%d]?[0-9]" (/ regexp 10)))
                               (t
                                "0*1?[0-9]"))))
                      ((vectorp regexp)
                       ;; A vector of options returned by `datetime-locale-field'.
                       (setq have-case-sensitive-parts t)
                       (regexp-opt (append regexp nil)))
                      (t
                       regexp))
                regexp-parts)))
      (setq part-index (1+ part-index)))
    (setq era-part-indices               (nreverse era-part-indices)
          year-part-indices              (nreverse year-part-indices)
          month-number-part-indices      (nreverse month-number-part-indices)
          month-name-part-indices        (nreverse month-name-part-indices)
          day-of-month-part-indices      (nreverse day-of-month-part-indices)
          am-pm-part-indices             (nreverse am-pm-part-indices)
          hour-0-23-part-indices         (nreverse hour-0-23-part-indices)
          hour-1-24-part-indices         (nreverse hour-1-24-part-indices)
          hour-am-pm-1-12-part-indices   (nreverse hour-am-pm-1-12-part-indices)
          hour-am-pm-0-11-part-indices   (nreverse hour-am-pm-0-11-part-indices)
          regexp-parts                   (nreverse regexp-parts)
          minute-part-indices            (nreverse minute-part-indices)
          second-part-indices            (nreverse second-part-indices)
          second-fractional-part-indices (nreverse second-fractional-part-indices))
    (unless validating
      (when month-number-part-indices
        (setq month-name-part-indices nil))
      (cond (hour-0-23-part-indices
             (setq hour-1-24-part-indices       nil
                   hour-am-pm-1-12-part-indices nil
                   hour-am-pm-0-11-part-indices nil))
            (hour-1-24-part-indices
             (setq hour-am-pm-1-12-part-indices nil
                   hour-am-pm-0-11-part-indices nil))
            (hour-am-pm-0-11-part-indices
             (setq hour-am-pm-1-12-part-indices nil))))
    (let* ((regexp-parts            regexp-parts)
           (substituting-indices-in (list era-part-indices
                                          year-part-indices month-number-part-indices month-name-part-indices day-of-month-part-indices
                                          am-pm-part-indices
                                          hour-0-23-part-indices hour-1-24-part-indices hour-am-pm-1-12-part-indices hour-am-pm-0-11-part-indices
                                          minute-part-indices second-part-indices second-fractional-part-indices))
           (part-index              0)
           (group-index             1))
      (while regexp-parts
        (let ((substituting-indices-in-scan substituting-indices-in))
          (while substituting-indices-in-scan
            (let ((listed-element (car substituting-indices-in-scan)))
              (when listed-element
                ;; To handle alists.
                (unless (numberp (car listed-element))
                  (setq listed-element (car listed-element)))
                (when (eq part-index (car listed-element))
                  (setf (car listed-element)               group-index
                        (car substituting-indices-in-scan) (cdar substituting-indices-in-scan)
                        (car regexp-parts)                 (concat "\\(" (car regexp-parts) "\\)")
                        group-index                        (1+ group-index)
                        substituting-indices-in-scan       nil))))
            (setq substituting-indices-in-scan (cdr substituting-indices-in-scan))))
        (setq regexp-parts (cdr regexp-parts)
              part-index   (1+ part-index))))
    (let* ((downcased          (and have-case-sensitive-parts case-insensitive))
           (year-computation   (datetime--parser-computation pattern "year" validating nil nil
                                                             (year-part-indices datetime--parser-year-computation)))
           (constant-year      (unless year-computation
                                 (or (plist-get 'year defaults) 1970)))
           (era-correction     (when (and year-computation era-part-indices)
                                 (datetime--parser-computation pattern "era" validating nil nil
                                                               (era-part-indices (datetime--parser-era-correction locale :eras downcased) t))))
           (month-computation  (or (datetime--parser-computation pattern "month" validating 0 11
                                                                 (month-number-part-indices (datetime--parser-int-computation t))
                                                                 (month-name-part-indices (datetime--parser-string-index-computation locale nil downcased) t))
                                   (let ((default (plist-get 'month defaults)))
                                     (if default (1- default) 0))))
           (day-computation    (or (datetime--parser-computation pattern "day of month" validating 0 nil
                                                                 (day-of-month-part-indices (datetime--parser-int-computation t)))
                                   (let ((default (plist-get 'day defaults)))
                                     (if default (1- default) 0))))
           (am-pm-computation  (or (datetime--parser-computation pattern "am-pm" validating nil nil (am-pm-part-indices (datetime--parser-string-if-computation locale :am-pm downcased 0 12) t))
                                   (plist-get 'am-pm 0)
                                   0))
           (hour-computation   (or (datetime--parser-computation pattern "hour" validating nil 23
                                                                 (hour-0-23-part-indices datetime--parser-int-computation)
                                                                 (hour-1-24-part-indices datetime--parser-hour-1-24-computation)
                                                                 (hour-am-pm-1-12-part-indices (datetime--parser-hour-am-pm-computation am-pm-computation t))
                                                                 (hour-am-pm-0-11-part-indices (datetime--parser-hour-am-pm-computation am-pm-computation nil)))
                                   (plist-get 'hour defaults) 0))
           (minute-computation (or (datetime--parser-computation pattern "minute" validating nil 59
                                                                 (minute-part-indices datetime--parser-int-computation))
                                   (plist-get 'minute defaults) 0))
           (second-computation (or (datetime--parser-computation pattern "second" validating nil 59
                                                                 (second-part-indices datetime--parser-int-computation))
                                   (plist-get 'second defaults) 0))
           (parser            `(+ ,@(when (or year-computation
                                              (not (memq constant-year     '(nil 1970)))
                                              (not (memq month-computation '(nil 0)))
                                              (not (memq day-computation   '(nil 0))))
                                      ;; FIXME: Optimize for constant year.
                                      `((* (let ((year  ,(or year-computation constant-year))
                                                 (month ,month-computation))
                                             ,@(when era-correction
                                                 `(,era-correction))
                                             (let ((year-mod-400 (mod year 400)))
                                               (+ (* (/ (- year year-mod-400) 400) ,datetime--gregorian-days-in-400-years)
                                                  (aref ,datetime--gregorian-cumulative-year-days year-mod-400)
                                                  ,(- datetime--gregorian-days-in-1970-years)
                                                  (aref ,datetime--gregorian-cumulative-month-days month)
                                                  (if (and (>= month 2) (datetime--gregorian-leap-year-mod-400-p year-mod-400)) 1 0)
                                                  ,(if validating
                                                       `(let ((day ,day-computation))
                                                          (unless (and (<= 0 day)
                                                                       (or (< day (aref ,datetime--gregorian-month-days month))
                                                                           (and (= month 1) (= day 28)
                                                                                (datetime--gregorian-leap-year-mod-400-p year-mod-400))))
                                                            (signal 'datetime-invalid-string (list string ,pattern "day is out of range")))
                                                          day)
                                                     day-computation))))
                                           ,(* 24 60 60))))
                                  (* ,hour-computation ,(* 60 60))
                                  (* ,minute-computation 60)
                                  ,second-computation
                                  ,@(when second-fractional-part-indices
                                      `((/ (string-to-number (match-string ,(caar second-fractional-part-indices) string))
                                           ,(cdar second-fractional-part-indices)))))))
      (pcase timezone-data
        (`(,constant-offset)
         (unless (= constant-offset 0)
           (setq parser `(- ,parser ,constant-offset))))
        (_
         (setq parser `(datetime--convert-from-utc-float ,parser ,(datetime--macroexp-quote timezone-data)))))
      (setq parser `(save-match-data
                      (if (string-match ,(concat "^" (apply #'concat regexp-parts) "$") ,(if downcased `(downcase string) 'string))
                          ,parser
                        (signal 'datetime-invalid-string (list string ,pattern "doesn't match the pattern")))))
      (when have-case-sensitive-parts
        (setq parser `(let ((case-fold-search ,case-insensitive))
                        ,parser)))
      (setq parser `(lambda (string) ,parser))
      (if (plist-get options :debug)
          parser
        (byte-compile parser)))))

(defun datetime--parser-year-computation (argument)
  (pcase (cdr argument)
    (`add-century-when-parsing `(let ((year ,(datetime--parser-int-computation (car argument))))
                                  (if (= (length (match-string ,(car argument) string)) 2)
                                      (+ year 2000)
                                    year)))
    (`always-two-digits        `(+ ,(datetime--parser-int-computation (car argument)) 2000))
    (_                         (datetime--parser-int-computation (car argument)))))

(defun datetime--parser-era-correction (argument locale field downcased)
  (datetime--parser-string-if-computation argument locale field downcased `(setq year (- 1 year)) nil))

(defun datetime--parser-hour-1-24-computation (argument)
  `(let ((hour-1-24 ,(datetime--parser-int-computation argument)))
     (if (< hour-1-24 24) hour-1-24 0)))

(defun datetime--parser-hour-am-pm-computation (argument am-pm-computation expect-1-12)
  (if expect-1-12
      `(let ((hour-am-pm-1-12 ,(datetime--parser-int-computation argument)))
         (+ (if (< hour-am-pm-1-12 12) hour-am-pm-1-12 0) ,am-pm-computation))
    `(+ ,(datetime--parser-int-computation argument) ,am-pm-computation)))

(defun datetime--parser-int-computation (argument &optional off-by-one)
  (let ((computation `(string-to-number (match-string ,(if (consp argument) (car argument) argument) string))))
    (if off-by-one
        `(1- ,computation)
      computation)))

(defun datetime--parser-string-index-computation (argument locale field downcased)
  (let ((strings (datetime-locale-field locale (or field (cdr argument)))))
    (when downcased
      (setq strings (vconcat (mapcar #'downcase (append strings nil)))))
    `(let ((match (match-string ,(if (consp argument) (car argument) argument) string))
           (n     0))
       (while (not (string= match (aref ,strings n)))
         (setq n (1+ n)))
       n)))

(defun datetime--parser-string-if-computation (argument locale field downcased if-first-form if-second-form)
  (let ((strings (datetime-locale-field locale field)))
    (unless (= (length strings) 2)
      (error "Must be called only for two-string fields"))
    `(if (string= (match-string ,(if (consp argument) (car argument) argument) string)
                  ,(if downcased (downcase (aref strings 0)) (aref strings 0)))
         ,if-first-form
       ,if-second-form)))

;; Pretty similar to `datetime--convert-to-utc-float', but not quite.
(defun datetime--convert-from-utc-float (date-time timezone-data)
  (let ((year-offset          (floor (/ (- date-time (car timezone-data)) datetime--average-seconds-in-year)))
        (all-year-transitions (nth 1 timezone-data)))
    (if (>= year-offset 0)
        (let* ((year-transitions (or (when (< year-offset (length all-year-transitions))
                                       (aref all-year-transitions year-offset))
                                     (datetime--calculate-year-transitions timezone-data year-offset)))
               (offset           (pop year-transitions)))
          (when year-transitions
            (let ((offset-in-year (floor (- date-time (car timezone-data) (* year-offset datetime--average-seconds-in-year)))))
              (while (and (>= (- offset-in-year offset) (car year-transitions))
                          (setq offset           (cadr year-transitions)
                                year-transitions (cddr year-transitions))))))
          (- date-time offset))
      ;; Offset before the very first transition.
      (- date-time (car (aref all-year-transitions 0))))))


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
    defaults to English, even if system locale is different.  You
    can use special value \\='system to let the library find it.

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
         (locale         (datetime--get-locale options))
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
                          (`era
                           (datetime-locale-field locale :era))
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
                           (datetime-locale-field locale (if (eq details 'abbreviated) :month-context-abbr      :month-context-names)))
                          (`month-standalone-name
                           (datetime-locale-field locale (if (eq details 'abbreviated) :month-standalone-abbr   :month-standalone-names)))
                          (`week-in-year         53)
                          (`week-in-month         5)
                          (`day-in-month         31)
                          (`weekday-in-month      5)
                          (`weekday               7)
                          (`weekday-context-name
                           (datetime-locale-field locale (if (eq details 'abbreviated) :weekday-context-abbr    :weekday-context-names)))
                          (`weekday-standalone-name
                           (datetime-locale-field locale (if (eq details 'abbreviated) :weekday-standalone-abbr :weekday-standalone-names)))
                          (`am-pm
                           (datetime-locale-field locale :am-pm))
                          (`hour-0-23            23)
                          (`hour-1-24            24)
                          (`hour-am-pm-0-11      11)
                          (`hour-am-pm-1-12      12)
                          (`minute               59)
                          (`second               59)
                          (`decimal-separator    (rx (or "." ",")))
                          ((or `millisecond `second-fractional)
                           (apply #'concat (make-list details (rx (any "0-9")))))
                          (`timezone
                           (signal 'datetime-unsupported-timezone nil))
                          (_ (error "Unexpected value %s" type)))))
          (push (cond ((integerp regexp)
                       ;; REGEXP is really the maximum value of this one- or two-digit
                       ;; number.
                       (if (<= regexp 9)
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
                                "0*1?[0-9]"))))
                      ((vectorp regexp)
                       ;; A vector of options returned by `datetime-locale-field'.
                       (regexp-opt (append regexp nil)))
                      (t
                       regexp))
                regexp-parts))))
    (apply #'concat (nreverse regexp-parts))))


(defun datetime-recode-pattern (from to pattern &rest options)
  "Recode PATTERN between two supported types.
As a special case, either of FROM and TO can be set to \\='parsed.
This is useful as a speed optimization in a few cases where you
perform several transformations on the same pattern.

Options can be a list of the following keyword arguments:

  :second-fractional-extension

    In Java patterns any number of \"S\" stands for milliseconds.
    With this extension they are instead interpreted according to
    how many \"S\" there is, e.g. \"SSSSSS\" means microseconds.

  :any-decimal-separator

    Treat a decimal dot or comma in pattern between seconds and
    milli- or microseconds (etc.) as a placeholder for _any_
    decimal separator and also accept commas in this place.  This
    only works if TO is \\='parsed."
  (datetime--format-pattern to (datetime--parse-pattern from pattern options) options))


;; Arguments are expected to be atoms.
(defmacro datetime--pattern-includes-p (type pattern options &rest part-types)
  `(let ((parts (datetime--parse-pattern ,type ,pattern ,options))
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

(defun datetime-pattern-locale-dependent-p (type pattern &rest options)
  "Determine if PATTERN includes any locale-based parts.
In other words, return non-nil if PATTERN includes any textual
names.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options era month-context-name month-standalone-name weekday-context-name weekday-standalone-name am-pm))

(defun datetime-pattern-includes-date-p (type pattern &rest options)
  "Determine if PATTERN includes any date parts.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options
                                era year year-for-week month month-context-name month-standalone-name week-in-year week-in-month
                                day-in-year day-in-month weekday-in-month weekday weekday-context-name weekday-standalone-name))

(defun datetime-pattern-includes-time-p (type pattern &rest options)
  "Determine if PATTERN includes any time parts.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options
                                am-pm hour-0-23 hour-1-24 hour-am-pm-0-11 hour-am-pm-1-12 minute second millisecond second-fractional))

(defun datetime-pattern-includes-era-p (type pattern &rest options)
  "Determine if PATTERN includes the date era.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options era))

(defun datetime-pattern-includes-year-p (type pattern &rest options)
  "Determine if PATTERN includes the year.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options year year-for-week))

(defun datetime-pattern-includes-month-p (type pattern &rest options)
  "Determine if PATTERN includes the month.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options month month-context-name month-standalone-name))

(defun datetime-pattern-includes-week-p (type pattern &rest options)
  "Determine if PATTERN includes the week.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options week-in-year week-in-month))

(defun datetime-pattern-includes-day-p (type pattern &rest options)
  "Determine if PATTERN includes the day.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options day-in-year day-in-month))

(defun datetime-pattern-includes-weekday-p (type pattern &rest options)
  "Determine if PATTERN includes the weekday.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options weekday-in-month weekday weekday-context-name weekday-standalone-name))

(defun datetime-pattern-includes-hour-p (type pattern &rest options)
  "Determine if PATTERN includes hours.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options hour-0-23 hour-1-24 hour-am-pm-0-11 hour-am-pm-1-12))

(defun datetime-pattern-includes-minute-p (type pattern &rest options)
  "Determine if PATTERN includes minutes.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options minute))

(defun datetime-pattern-includes-second-p (type pattern &rest options)
  "Determine if PATTERN includes seconds.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options second))

(defun datetime-pattern-includes-second-fractionals-p (type pattern &rest options)
  "Determine if PATTERN includes fractions of seconds.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options millisecond second-fractional))

(define-obsolete-function-alias 'datetime-pattern-includes-millisecond-p 'datetime-pattern-includes-second-fractionals-p)

(defun datetime-pattern-num-second-fractionals (type pattern &rest options)
  "Determine if PATTERN includes fractions of seconds.

OPTIONS are passed to `datetime-recode-pattern'.  At least
`:second-fractional-extension' can affect result of this
function."
  (let ((parts           (datetime--parse-pattern type pattern options))
        (num-fractionals 0))
    (while parts
      (let ((part (pop parts)))
        (when (consp part)
          (pcase (car part)
            (`millisecond       (setq num-fractionals (max num-fractionals 3)))
            (`second-fractional (setq num-fractionals (max num-fractionals (cdr part))))))))
    num-fractionals))

(defun datetime-pattern-includes-timezone-p (type pattern &rest options)
  "Determine if PATTERN includes timezone.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options timezone))


(defun datetime-list-locales (&optional include-variants)
  "List all locales for which the library has information.
If INCLUDE-VARIANTS is nil, only include “base” locales (in
format \"xx\"), if it is t then also include “variants” in format
\"xx-YY\".

Return value is a list of symbols in no particular order; it can
be modified freely."
  (if include-variants
      (extmap-keys datetime--locale-extmap)
    (let (locales)
      (extmap-mapc datetime--locale-extmap (lambda (locale data) (unless (plist-get data :parent) (push locale locales))))
      locales)))

(defun datetime-list-timezones ()
  "List all timezones for which the library has information.

Return value is a list of symbols in no particular order; it can
be modified freely."
  (extmap-keys datetime--timezone-extmap))


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
  (unless date-variant
    (setq date-variant :medium))
  (unless time-variant
    (setq time-variant date-variant))
  (let* ((date-time-pattern-rule (or (datetime-locale-field locale :date-time-pattern-rule) '(t . " ")))
         (separator              (cdr date-time-pattern-rule))
         (date-part              (datetime-locale-date-pattern locale date-variant))
         (time-part              (datetime-locale-time-pattern locale time-variant)))
    (unless (stringp separator)
      (setq separator (cdr (assoc (list date-variant time-variant) separator))))
    (if (car date-time-pattern-rule)
        (concat date-part separator time-part)
      (concat time-part separator date-part))))


(defconst datetime--english-eras  ["BC" "AD"])
(defconst datetime--english-am-pm ["AM" "PM"])

(defsubst datetime--do-get-locale-field (locale-data field)
  (or (plist-get locale-data field)
      ;; See `datetime--locale-extmap' for description of fallbacks.
      (pcase field
        (:month-standalone-abbr    (plist-get locale-data :month-context-abbr))
        (:month-standalone-names   (plist-get locale-data :month-context-names))
        (:weekday-standalone-abbr  (plist-get locale-data :weekday-context-abbr))
        (:weekday-standalone-names (plist-get locale-data :weekday-context-names)))))

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
  (let ((data (extmap-get datetime--locale-extmap locale t)))
    (or (datetime--do-get-locale-field data field)
        (let ((parent (plist-get data :parent)))
          (when parent
            (datetime--do-get-locale-field (extmap-get datetime--locale-extmap parent) field)))
        (pcase field
          (:decimal-separator ?.)
          (:eras              datetime--english-eras)
          (:am-pm             datetime--english-am-pm)))))


(defun datetime-locale-database-version ()
  "Return locale database version, a simple integer.
This version will be incremented each time locale database of the
package is updated.  It can be used e.g. to invalidate caches you
create based on locales `datetime' knows about."
  1)

(defun datetime-timezone-database-version ()
  "Return timezone database version, a simple integer.
This version will be incremented each time timezone database of the
package is updated.  It can be used e.g. to invalidate caches you
create based on timezone `datetime' knows about and their rules."
  1)


(provide 'datetime)

;;; datetime.el ends here
