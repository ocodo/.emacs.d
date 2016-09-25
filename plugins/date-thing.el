;;; date-thing.el --- Date conversion and formatting tools
;;
;; Author: Jason Milkins <jasonm23@gmail.com>
;; URL: https://github.com/ococo/.emacs.d/plugins/date-thing.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  Date tools in Emacs Lisp
;;
;;; Code:
(require 's)
(require 'dash)

(defvar date-thing-generic-regexp "\\(.*?\\)[-/ ]\\(.*?\\)[-/ ]\\(.*\\)"
  "A generic date reader regexp.")

(defvar date-thing-month-names '((:number 1  :short "jan" :long "january"  )
                       (:number 2  :short "feb" :long "february" )
                       (:number 3  :short "mar" :long "march"    )
                       (:number 4  :short "apr" :long "april"    )
                       (:number 5  :short "may" :long "may"      )
                       (:number 6  :short "jun" :long "june"     )
                       (:number 7  :short "jul" :long "july"     )
                       (:number 8  :short "aug" :long "august"   )
                       (:number 9  :short "sep" :long "september")
                       (:number 10 :short "oct" :long "october"  )
                       (:number 11 :short "nov" :long "november" )
                       (:number 12 :short "dec" :long "december" )))

(defun date-thing-year-p (year)
  "Naive YEAR detection."
  (and (stringp year)
       (= 4 (length (format "%s" year)))))

(defun date-thing-month-detect (value)
  "Detect if VALUE is a month."
  (let* ((value-num (string-to-number value))
         (month (cond
                 ((and (numberp value) (> value 0) (<= value 12)) value)
                 ((and (stringp value)
                       (> value-num 0)
                       (<= value-num 12))
                  (string-to-number value))
                 (t value))))
    (--find (date-thing-month-match it month) date-thing-month-names)))

(defun date-thing-day-detect (year month values)
  "Given a YEAR, MONTH and Pick the day from a list of VALUES."
  (first (--reject (or (string= it year) (string= it month)) values)))

(defalias 'date-thing-month-match (pcase-lambda (`(:number ,num :short ,short :long ,long) month)
                          "Select plist for MONTH."
                          (or (and (numberp month) (= num month))
                              (and (stringp month) (string= (downcase month) short))
                              (and (stringp month) (string= (downcase month) long)))))

(defun date-thing-month-get (month)
  "Get MONTH (prop autodetected) as plist."
  (--find (date-thing-month-match it  month)
          date-thing-month-names))

(defun date-thing-month-prop (month prop)
  "Get MONTH (prop autodetected) and pluck another PROP."
  (plist-get (date-thing-month-get month) prop))

(defun date-thing-month-insert-number (month)
  "Given MONTH insert the month number."
  (interactive (list
                (if (region-active-p)
                    (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
                      (save-mark-and-excursion
                       (kill-region (region-beginning) (region-end))
                       string))
                  (completing-read "Month: " (mapcar (lambda (it) (s-capitalize (plist-get it :long))) date-thing-month-names)))))
  (insert (format "%02i" (date-thing-month-prop month :number))))

(defun date-thing-guess (date-thing-string)
  "Guess the format of a DATE-THING-STRING.

Will correctly guess from the following date formats.

NOTE: All formats recognized below interchange with slash or space delimiters.

Key: MMM = short month e.g. Jan (jan, JAN)

yyyy-MMM-dd
dd-MMM-yyyy
dd-MMM-yyyy

Key: MMMM = long month e.g. January (january, JANUARY)

yyyy-MMMM-dd
dd-MMMM-yyyy
dd-MMMM-yyyy

For the following formats it will only guess 100% when the dd is
above 12. Use the optional flag US to force (mm-dd) order,
default will be (dd-mm) in the even of a tie-break.  For example
if the day is less than 12.

yyyy-dd-mm
yyyy-mm-dd
dd-mm-yyyy
mm-dd-yyyy"
  (pcase-let* ((rxp date-thing-generic-regexp)
               (`(,_ ,m1 ,m2 ,m3) (s-match rxp date-thing-string))
               (month (cond ((date-thing-month-detect m1) m1)
                            ((date-thing-month-detect m2) m2)
                            ((date-thing-month-detect m3) m3)))
               (year (cond ((date-thing-year-p m1) m1)
                           ((date-thing-year-p m2) m2)
                           ((date-thing-year-p m3) m3)))
               (day (date-thing-day-detect year month (list m1 m2 m3))))
    (list :day day
          :month (date-thing-month-get month)
          :year year)))

(defun date-thing-dd-mm-yyyy-to-iso8601 (date-thing-dd-mm-yyyy-string)
  "Convert DATE-THING-DD-MM-YYYY-STRING to an iso8601 date string."
  (pcase-let* ((rxp date-thing-generic-regexp)
               (`(,_
                  ,day
                  ,month
                  ,year)
                (s-match rxp date-thing-dd-mm-yyyy-string)))
    (when (and day month year)
      (format "%s-%s-%s"
              year
              (format "%02i"
                      (date-thing-month-prop month :number))
              day))))

(defun date-thing-guess-convert-to-iso8601 (date-thing-string)
  "Use `date-thing-guess' to convert DATE-THING-STRING to ISO8601."
  (pcase-let ((`(:day ,day :month (:number ,month ,_ ,_ ,_ ,_) :year ,year) (date-thing-guess date-thing-string)))
    (format "%s-%02i-%s" year month day)))

(defun date-thing-guess-insert-iso8601 (date-thing-string)
  "Use `date-thing-guess' to convert DATE-THING-STRING to ISO8601 and insert it."
  (interactive (list
                (if (region-active-p)
                    (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
                      (save-mark-and-excursion
                       (kill-region (region-beginning) (region-end))
                       string))
                  (read-string "Date: "))))
  (let ((date-thing-string-iso (date-thing-guess-convert-to-iso8601 date-thing-string)))
    (if date-thing-string-iso
        (insert date-thing-string-iso)
      (insert date-thing-string))))

(defun date-thing-dd-mm-yyyy-insert-iso8601 (dd-mm-yyyy)
  "Convert a DD-MM-YYYY and insert as an iso8601 date string."
  (interactive (list
                (if (region-active-p)
                    (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
                      (save-mark-and-excursion
                       (kill-region (region-beginning) (region-end))
                       string))
                  (read-string "Date (dd mm yyyy): "))))
  (let ((date-thing-string-iso (date-thing-dd-mm-yyyy-to-iso8601 dd-mm-yyyy)))
    (if date-thing-string-iso
        (insert date-thing-string-iso)
      (insert dd-mm-yyyy))))

(defun tweet-get-timestamp (user status-id)
  "Get the timestamp for a tweet given USER and STATUS-ID.
Requires pup https://github.com/ericchiang/pup."
  (s-chomp
   (shell-command-to-string
    (format
     "curl -s https://twitter.com/%s/status/%s | pup 'a.tweet-timestamp[href*=%s] attr{title}'"
     user
     status-id
     status-id ))))

(defun tweet-date-from-timestamp (timestamp)
  "Get the date substring from the tweet TIMESTAMP."
  (first
   (s-match
    "[0-9]\\{2\\} [A-z]\\{3\\} [0-9]\\{4\\}$"
    timestamp)))

(defun tweet-get-date-as-iso8601 (name status-id)
  "Get the iso8601 date for a tweet based on twitter NAME and STATUS-ID."
  (date-thing-dd-mm-yyyy-to-iso8601
   (tweet-date-from-timestamp
    (tweet-get-timestamp name status-id))))

(provide 'date-thing)

;;; date-thing.el ends here
