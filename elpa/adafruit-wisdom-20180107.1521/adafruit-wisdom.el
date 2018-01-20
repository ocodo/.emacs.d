;;; adafruit-wisdom.el --- Get/display adafruit.com quotes

;; Author: Neil Okamoto <neil.okamoto+melpa@gmail.com>
;; Version: 0.2
;; Package-Version: 20180107.1521
;; Keywords: games
;; URL: https://github.com/gonewest818/adafruit-wisdom.el
;; Package-Requires: ((emacs "24"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; I've always enjoyed the engineering quotes found at the footer of
;; each page on adafruit.com ... now you can too!  This code is
;; derived from Dave Pearson's dad-joke.el, except adafruit.com
;; publishes their quotes as rss so we have to deal with that.

;;; Code:

(require 'dom)
(require 'url-vars)
(require 'xml)

(defconst adafruit-wisdom-quote-url "https://www.adafruit.com/feed/quotes.xml"
  "URL for the RSS quote feed served on adafruit.com.")

(defconst adafruit-wisdom-cache-file "adafruit-wisdom.cache"
  "Location for the local copy of the quotes file.")

(defconst adafruit-wisdom-cache-ttl (* 3600.0 24.0) ; 24 hours
  "Time-to-live for the local cache file.")

(defun adafruit-wisdom-cached-get ()
  "Retrieves RSS from adafruit.com, or from cache if TTL hasn't expired.
Returns the parsed XML."
  (let* ((cache (locate-user-emacs-file adafruit-wisdom-cache-file))
         (mtime (nth 5 (file-attributes cache)))
         (age   (and mtime (- (float-time (current-time))
                              (float-time mtime)))))
    (if (and age (< age adafruit-wisdom-cache-ttl))
        (with-temp-buffer
          (insert-file-contents cache)
          (xml-parse-region (point-min) (point-max)))
      (with-temp-file cache
        (url-insert-file-contents adafruit-wisdom-quote-url)
        (xml-parse-region (point-min) (point-max))))))

;;;###autoload
(defun adafruit-wisdom-select ()
  "Select a quote at random and return as a string.

Parse assuming the following RSS format:
     ((rss (channel (item ...) (item ...) (item ...) ...)))
where each item contains:
     (item (title nil \"the quote\") ...)
and we  need just \"the quote\"."
  (let* ((items (dom-by-tag (adafruit-wisdom-cached-get) 'item))
         (pick  (nth (random (length items)) items))
         (title (dom-text (car (dom-by-tag pick 'title)))))
    title))

;;;###autoload
(defun adafruit-wisdom (&optional insert)
  "Display one of Adafruit's quotes in the minibuffer.
If INSERT is non-nil the joke will be inserted into the current
buffer rather than shown in the minibuffer."
  (interactive "P")
  (let ((quote (adafruit-wisdom-select)))
    (if (null quote)
        (error "Couldn't retrieve a quote from adafruit")
      (if insert
          (insert quote)
        (message quote)))))

(provide 'adafruit-wisdom)

;;; adafruit-wisdom.el ends here
