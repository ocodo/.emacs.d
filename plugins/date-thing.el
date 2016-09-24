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

(defvar month-names '((:number 1  :short "jan" :long "january"  )
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

(defun date-year-p (year)
  "Naive YEAR detection."
  (and (stringp year)
       (= 4 (length (format "%s" year)))))

(defun date-month-detect (value)
  "Detect if VALUE is a month."
  (let* ((value-num (string-to-number value))
         (month (cond
                 ((and (numberp value) (> value 0) (<= value 12)) value)
                 ((and (stringp value)
                       (> value-num 0)
                       (<= value-num 12))
                  (string-to-number value))
                 (t value))))
    (and (--find (date-month-match it month) month-names) t)))

(defun date-day-detect (year month values)
  "Given a YEAR, MONTH and Pick the day from a list of VALUES."
  (first (--reject (or (string= it year) (string= it month)) values)))

(defun-pcase date-month-match (`(:number ,num :short ,short :long ,long) month)
  "Select plist for MONTH."
  (or (and (numberp month) (= num month))
      (and (stringp month) (string= (downcase month) short))
      (and (stringp month) (string= (downcase month) long))))

(defun date-month-get (month)
  "Get MONTH (prop autodetected) as plist."
  (--find
   (date-month-match it  month)
   month-names))

(defun date-month-prop (month prop)
  "Get MONTH (prop autodetected) and pluck another PROP."
  (plist-get
   (--find
    (date-month-match it  month)
    month-names)
   prop))

(defun date-guess (date-string)
  "Guess the format of a DATE-STRING.

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
  (pcase-let* ((rxp "\\(.*?\\)[-/ ]\\(.*?\\)[-/ ]\\(.*\\)")
               (`(,_ ,m1 ,m2 ,m3) (s-match rxp date-string))
               (month (cond ((date-month-detect m1) m1)
                            ((date-month-detect m2) m2)
                            ((date-month-detect m3) m3)))
               (year (cond ((date-year-p m1) m1)
                           ((date-year-p m2) m2)
                           ((date-year-p m3) m3)))
               (day (date-day-detect year month (list m1 m2 m3))))
    (list :day day
          :month (date-month-get (string-to-number month))
          :year year)))

(provide 'date-thing)

;;; date-thing.el ends here
