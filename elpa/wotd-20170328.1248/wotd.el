;;; wotd.el --- Fetch word-of-the-day from multiple online sources  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: extensions
;; Package-Version: 20170328.1248
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (org "8.2.10"))

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

;;                            _________________

;;                             WORD OF THE DAY

;;                               Junpeng Qiu
;;                            _________________


;; Table of Contents
;; _________________

;; 1 Available Sources
;; 2 Commands
;; .. 2.1 `M-x wotd-select'
;; .. 2.2 `M-x wotd-all'
;; 3 Config
;; 4 Contribution


;; [[file:https://melpa.org/packages/wotd-badge.svg]]

;; Show /Word of the Day/ from 15 online sources in Emacs!


;; [[file:https://melpa.org/packages/wotd-badge.svg]]
;; https://melpa.org/#/wotd


;; 1 Available Sources
;; ===================

;;   - Merriam Webster
;;   - Wiktionary
;;   - Macmillan Dictionary
;;   - Wordsmith
;;   - Free Dictionary
;;   - Oxford English Dictionary
;;   - Urban Dictionary
;;   - WordThink
;;   - Oxford Dictionaries
;;   - Cambridge Dictionary
;;   - Collins Dictionary
;;   - Learners Dictionary
;;   - Wordnik
;;   - Dictionary.com
;;   - Bing dict (English word, Chinese definitions.  See also
;;     [bing-dict.el])


;; [bing-dict.el] https://github.com/cute-jumper/bing-dict.el


;; 2 Commands
;; ==========

;; 2.1 `M-x wotd-select'
;; ~~~~~~~~~~~~~~~~~~~~~

;;   Show a word-of-the-day by selecting a source from
;;   `wotd-enabled-sources'.

;;   This works asynchronously.


;; 2.2 `M-x wotd-all'
;; ~~~~~~~~~~~~~~~~~~

;;   Show all the "word-of-the-day"s from `wotd-enabled-sources' in a
;;   summary buffer, which enables `orgstruct-mode' for easier navigation.

;;   This works synchronously.  (may be changed later)

;;   Screenshot: [./screenshots/summary.png]


;; 3 Config
;; ========

;;   See `wotd-supported-sources' for the complete list of all the
;;   supported online sources.  You can set `wotd-enabled-sources' to
;;   controls which sources are enabled.  By default, all the sources
;;   except `bing dict' are enabled.


;; 4 Contribution
;; ==============

;;   Welcome to submit PRs to add more online sources!

;;; Code:

(require 'url)
(require 'xml)
(require 'shr)
(require 'json)
(require 'org-table)
(require 'org)

(defvar wotd-supported-sources
  '(merriam-webster
    wiktionary
    macmillan
    wordsmith
    free-dictionary
    oxford-english-dictionary
    urban-dictionary
    wordthink
    oxford-dictionaries
    cambridge-dictionary
    collins-dictionary
    learners-dictionary
    wordnik
    dictionary-dot-com
    ;; 5-minute-english
    bing-dict)
  "The complete list of the supported sources.")

(defvar wotd-enabled-sources (delq 'bing-dict wotd-supported-sources)
  "This controls which sources should be enabled.
See `wotd-supported-sources' for the complete list.")

(defvar wotd-buffer "*Summary: Word of The Day")
(defvar wotd-render-width (frame-width))

(defvar wotd--enable-debug nil)
(defvar wotd--debug-buffer "*WOTD Debug*")
(defvar wotd--default-buf-name "*Word-of-the-Day*")

(defun wotd--debug (s)
  (when wotd--enable-debug
    (with-current-buffer (get-buffer-create wotd--debug-buffer)
      (erase-buffer)
      (insert (format "%s" s))))
  s)

;; Steal from `elfeed'
(defun wotd--xml-parse-region (&optional beg end buffer parse-dtd _parse-ns)
  "Decode (if needed) and parse XML file. Uses coding system from
XML encoding declaration."
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (setf (point) beg)
  (when (re-search-forward
         "<\\?xml.*?encoding=[\"']\\([^\"']+\\)[\"'].*?\\?>" nil t)
    (let ((coding-system (intern-soft (downcase (match-string 1)))))
      (when (ignore-errors (check-coding-system coding-system))
        (let ((mark-beg (make-marker))
              (mark-end (make-marker)))
          ;; Region changes with encoding, so use markers to track it.
          (set-marker mark-beg beg)
          (set-marker mark-end end)
          (set-buffer-multibyte t)
          (recode-region mark-beg mark-end coding-system 'raw-text)
          (setf beg (marker-position mark-beg)
                end (marker-position mark-end))))))
  (let ((xml-default-ns ()))
    (xml-parse-region beg end buffer parse-dtd 'symbol-qnames)))

(defmacro wotd--retrieve (url sync &rest body)
  (if sync
      `(with-current-buffer (url-retrieve-synchronously ,url)
         (goto-char url-http-end-of-headers)
         (set-buffer-multibyte t)
         ,@body)
    `(url-retrieve ,url
                   (lambda (status)
                     (if (plist-get status :error)
                         (error "Error when retrieving %s" ,url)
                       (goto-char url-http-end-of-headers)
                       (set-buffer-multibyte t)
                       ,@body)))))

(defmacro wotd--def-parser (type name url &rest body)
  (declare (indent 3))
  (let ((func-name (intern (format "wotd--get-%s" name)))
        (real-buf-name (format "*Word of the Day: %s*" name)))
    `(defun ,func-name (&optional sync)
       (wotd--retrieve
        ,url sync
        (delete-region (point-min) (point))
        (let* (,(if (eq type 'html)
                    'it
                  '(it (wotd--xml-parse-region (point) (point-max))))
               (res (wotd--debug (progn ,@body)))
               (content (if sync (cdr res) res))
               dom)
          (setq content (with-temp-buffer
                          (set-buffer-multibyte t)
                          (insert content)
                          (setq dom (libxml-parse-html-region
                                     (point-min) (point-max)))
                          (erase-buffer)
                          (let ((shr-width wotd-render-width))
                            (shr-insert-document dom))
                          (buffer-string)))
          (if sync
              (cons (car res) content)
            (progn
              (with-current-buffer (get-buffer-create ,real-buf-name)
                (erase-buffer)
                (insert content))
              (display-buffer ,real-buf-name))))))))

(wotd--def-parser xml merriam-webster
                  "https://www.merriam-webster.com/wotd/feed/rss2"
  (let* ((item (car (xml-get-children
                     (car (xml-get-children (car it) 'channel))
                     'item)))
         (html (replace-regexp-in-string
                "&#149;" "&#8226;"
                (nth 2 (car (xml-get-children item 'description))))))
    (if sync
        (cons (nth 2 (car (xml-get-children item 'title)))
              html)
      html)))

(wotd--def-parser xml wiktionary
                  "https://en.wiktionary.org/w/api.php?action=featuredfeed&feed=wotd"
  (let* ((item (car (last (xml-get-children
                           (car (xml-get-children (car it) 'channel))
                           'item))))
         (html (nth 2 (car (xml-get-children
                            item
                            'description))))
         title)
    (with-temp-buffer
      (insert html)
      (goto-char (point-min))
      (re-search-forward "<span id=\"WOTD-rss-title\">\\(.*?\\)</span>")
      (setq title (match-string 1)))
    (if sync
        (cons title html)
      html)))

(wotd--def-parser xml macmillan
                  "http://www.macmillandictionary.com/wotd/wotdrss.xml"
  (let* ((entry (car (last (xml-get-children (car it) 'entry))))
         (title (nth 2 (car (xml-get-children entry 'title))))
         (href (cdr (nth 1 (cadr (car (xml-get-children entry 'link))))))
         (date (nth 2 (car (xml-get-children entry 'updated))))
         (summary (nth 2 (car (xml-get-children entry 'summary))))
         (html (format "<h1><a href=\"%s\">%s</a></h1><p>%s</p><p>%s</p>"
                       href
                       title
                       date
                       summary)))
    (if sync
        (cons title html)
      html)))

(wotd--def-parser xml wordsmith
                  "http://www.wordsmith.org/awad/rss1.xml"
  (let* ((item (car (xml-get-children
                     (car (xml-get-children (car it) 'channel))
                     'item)))
         (title (nth 2 (car (xml-get-children item 'title))))
         (href (nth 2 (car (xml-get-children item 'link))))
         (description (nth 2 (car (xml-get-children item 'description))))
         (html (format "<h1><a href=\"%s\">%s</a></h1><p>%s</p>" href title description)))
    (if sync
        (cons title html)
      html)))

(wotd--def-parser xml free-dictionary
                  "http://www.thefreedictionary.com/_/WoD/rss.aspx"
  (let* ((item (car (xml-get-children
                     (car (xml-get-children (car it) 'channel))
                     'item)))
         (title (nth 2 (car (xml-get-children item 'title))))
         (href (nth 2 (car (xml-get-children item 'link))))
         (date (nth 2 (car (xml-get-children item 'pubDate))))
         (description (nth 2 (car (xml-get-children item 'description))))
         (html (format "<h1><a href=\"%s\">%s</a></h1><p>%s</p><p>%s</p>"
                       href title date description)))
    (if sync
        (cons title html)
      html)))

(wotd--def-parser xml oxford-english-dictionary
                  "http://www.oed.com/rss/wordoftheday"
  (let* ((item (car (last (xml-get-children
                           (car (xml-get-children (car it) 'channel))
                           'item))))
         (title (nth 2 (car (xml-get-children item 'title))))
         (href (nth 2 (car (xml-get-children item 'link))))
         (date (nth 2 (car (xml-get-children item 'pubDate))))
         (description (nth 2 (car (xml-get-children item 'description))))
         (html (format "<h1><a href=\"%s\">%s</a></h1><p>%s</p><p>%s</p>"
                       href title date description)))
    (if sync
        (cons title html)
      html)))

(wotd--def-parser xml urban-dictionary
                  "http://feeds.urbandictionary.com/UrbanWordOfTheDay"
  (let* ((item (car (xml-get-children
                     (car (xml-get-children (car it) 'channel))
                     'item)))
         (title (nth 2 (car (xml-get-children item 'title))))
         (href (nth 2 (car (xml-get-children item 'link))))
         (date (nth 2 (car (xml-get-children item 'pubDate))))
         (description (nth 2 (car (xml-get-children item 'description))))
         (html (format "<h1><a href=\"%s\">%s</a></h1><p>%s</p><p>%s</p>"
                       href title date description)))
    (if sync
        (cons title html)
      html)))

(wotd--def-parser xml wordthink
                  "http://www.wordthink.com/feed/"
  (let* ((item (car (xml-get-children
                     (car (xml-get-children (car it) 'channel))
                     'item)))
         (title (nth 2 (car (xml-get-children item 'title))))
         (href (nth 2 (car (xml-get-children item 'link))))
         (date (nth 2 (car (xml-get-children item 'pubDate))))
         (description (nth 2 (car (xml-get-children item 'description))))
         (html (format "<h1><a href=\"%s\">%s</a></h1><p>%s</p><p>%s</p>"
                       href title date description)))
    (if sync
        (cons title html)
      html)))

(wotd--def-parser html oxford-dictionaries
                  "https://en.oxforddictionaries.com/"
  (when (re-search-forward
         "Word of the Day.*?<a.*?>\\(.*?\\)</a>"
         nil
         t)
    (let* ((title (match-string 1))
           (href (format "https://en.oxforddictionaries.com/definition/%s" title))
           (html (format "<h1>%s</h1><a href=\"%s\">See the definition and examples</a>" title href)))
      (if sync
          (cons title html)
        html))))

(wotd--def-parser html cambridge-dictionary
                  "http://dictionary.cambridge.org/us/"
  (when (re-search-forward
         "<p class=\"h4 feature-w-big wotd-hw\">\\(.*?\\)</p><p>\\(.*\\)</p>"
         nil
         t)
    (let* ((title (match-string 1))
           (href (format "http://dictionary.cambridge.org/us/dictionary/british/%s"
                         (url-hexify-string title)))
           (description (match-string 2))
           (html (format "<h1><a href=\"%s\">%s</a></h1><p>%s</p>"
                         href title description)))
      (if sync
          (cons title html)
        html))))

(wotd--def-parser html collins-dictionary
                  "https://www.collinsdictionary.com/dictionary/english"
  (replace-string "\n" "")
  (message "")
  (goto-char (point-min))
  (when (re-search-forward
         "Word of the day.*?\"promoBox-title\">\\(.*?\\)</div>.*?\"promoBox-description\">\\(.*?\\)</div>"
         nil
         t)
    (let* ((title (match-string 1))
           (href (format "https://www.collinsdictionary.com/dictionary/english/%s"
                         (url-hexify-string (replace-regexp-in-string " " "-" title))))
           (description (match-string 2))
           (html (format "<h1><a href=\"%s\">%s</a></h1><p>%s</p>"
                         href title description)))
      (if sync
          (cons title html)
        html))))

(wotd--def-parser html learners-dictionary
                  "http://learnersdictionary.com/word-of-the-day"
  (let* ((beg (re-search-forward "<!--WOD content-->" nil t))
         (end (re-search-forward "<!--WOD Archive-->" nil t))
         (html (buffer-substring beg end)))
    (setq html
          (with-temp-buffer
            (insert html)
            (goto-char (point-min))
            (replace-regexp "[\r\n]" "")
            (goto-char (point-min))
            (replace-regexp "<span class = \"hpron_word voces_font\">/.*?/</span>\\|\
<!--headword: mobile view-->.*<!--hwpost-->" "")
            (message "")
            (string-join
             (delq nil
                   (mapcar
                    (lambda (ch) (encode-coding-char ch 'utf-8 'unicode))
                    (buffer-string))))))
    (if sync
        (with-temp-buffer
          (insert html)
          (goto-char (point-min))
          (re-search-forward "<span class = \"hw_txt.*?>\\(.*?\\)</span>")
          (cons (match-string 1) html))
      html)))

(wotd--def-parser html wordnik
                  "https://www.wordnik.com/word-of-the-day"
  (let* ((beg (re-search-forward "<div class=\"word_of_the_day\">" nil t))
         (end (re-search-forward "<!-- Wordnik announcement -->" nil t))
         (html (buffer-substring beg end)))
    (if sync
        (with-temp-buffer
          (insert html)
          (goto-char (point-min))
          (re-search-forward "<h1><a.*?>\\(.*?\\)</a></h1>")
          (cons (match-string 1) html))
      html)))

(wotd--def-parser html dictionary-dot-com
                  (concat "http://www.dictionary.com/wordoftheday/"
                          (format-time-string "%Y/%m/%d"))
  (let ((json-key-type 'string)
        wotd-alist
        word
        html)
    (re-search-forward "return {\"leader" nil t)
    (goto-char (match-beginning 0))
    (forward-word)
    (buffer-substring (point)
                      (save-excursion
                        (forward-sexp)
                        (point)))
    (setq wotd-alist
          (cdr (car (last
                     (assoc-default "days"
                                    (json-read-from-string
                                     (buffer-substring (point)
                                                       (save-excursion
                                                         (forward-sexp)
                                                         (point)))))))))
    (setq word (assoc-default
                "word" wotd-alist))
    (setq html
          (concat
           ;; word
           (format "<h1><a href=\"%s\">%s</a></h1>"
                   (format "http://www.dictionary.com/browse/%s"
                           (url-hexify-string word))
                   word)
           ;; pos & pronunciation
           (format "<p>%s [%s]</p>"
                   (assoc-default "pos" wotd-alist)
                   (assoc-default "pronunciation" wotd-alist))
           ;; definitions
           (format "<h3><strong>- Definitions</strong></h3><ul><li>%s</ul>"
                   (mapconcat #'identity
                              (assoc-default "definitions" wotd-alist)
                              "<li>"))
           ;; origin
           (format "<h3><strong>- Origin</strong></h3>%s"
                   (assoc-default "origin" wotd-alist))
           ;; citations
           (format "<h3><strong>- Citations</strong></h3><ul>%s</ul>"
                   (mapconcat (lambda (cite)
                                (format "<li>%s<br/> -- %s, %s"
                                        (assoc-default "quote" cite)
                                        (assoc-default "author" cite)
                                        (assoc-default "source" cite)
                                        ))
                              (assoc-default "citations" wotd-alist)
                              ""))))
    (if sync
        (cons word html)
      html)))

(wotd--def-parser html bing-dict
                  "http://www.bing.com/dict/?mkt=zh-cn"
  (let* ((beg (re-search-forward "<div class=\"client_daily_words_panel\">" nil t))
         (end (re-search-forward "</div><div class=\"client_daily_pic_bar\">" nil t))
         (html (replace-regexp-in-string
                "/dict/search" "http://www.bing.com/dict/search"
                (buffer-substring beg end))))
    (if sync
        (with-temp-buffer
          (insert html)
          (goto-char (point-min))
          (re-search-forward "<a .*?>\\(.*?\\)</a>")
          (cons (match-string 1) html))
      html)))

(wotd--def-parser html 5-minute-english
                  "http://www.5minuteenglish.com/iframes/word.php"
  (re-search-forward "<b style='text-transform:capitalize'>\\(.*?\\)</b>" nil t)
  (let ((title (match-string 1))
        (html (buffer-substring (match-beginning 0) (point-max))))
    (if sync
        (cons title html)
      html)))

;;;###autoload
(defun wotd-select (source)
  "Show word-of-the-day from SOURCE."
  (interactive
   (list (completing-read "Select a source: " wotd-enabled-sources)))
  (let ((func (intern (format "wotd--get-%s" source))))
    (funcall func)))

;;;###autoload
(defun wotd-all ()
  "Show all the `word-of-the-day's."
  (interactive)
  (with-current-buffer (get-buffer-create wotd-buffer)
    (erase-buffer)
    (unless (bound-and-true-p orgstruct-mode)
      (orgstruct-mode +1))
    (dolist (source wotd-enabled-sources)
      (let* ((func (intern (format "wotd--get-%s" source)))
             (result (funcall func t)))
        (insert (with-temp-buffer
                  (insert (propertize (format "* [%s] %s" source (car result))
                                      'face
                                      'org-level-1)
                          "\n  ")
                  (insert (replace-regexp-in-string "\n" "\n  " (cdr result)))
                  (delete-char -2)
                  (buffer-string))
                "\n")))
    (goto-char (point-min))
    (save-excursion
      (while (re-search-forward "^* " nil t)
        (goto-char (match-beginning 0))
        (org-cycle)
        (forward-line))))
  (switch-to-buffer-other-window wotd-buffer))

(provide 'wotd)
;;; wotd.el ends here
