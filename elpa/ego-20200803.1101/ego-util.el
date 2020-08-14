;;; ego-util.el --- Common utility functions required by ego

;; Copyright (C)  2015 Feng Shu, Kuangdash
;;                2012, 2013, 2014, 2015 Kelvin Hu

;; Author: Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;;         Feng Shu  <tumashu AT 163.com>
;;         Kuangdash <kuangdash AT 163.com>
;; Keywords: org-mode, convenience, beautify
;; Homepage: https://github.com/emacs-china/EGO

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

;; ego-util.el contains several utility functions

;;; Code:

(require 'ht)
(require 'ego-config)
(require 'cl-lib)

(defun ego--compare-standard-date (date1 date2)
  "Compare two standard ISO 8601 format dates, format is as below:
2012-08-17
1. if date1 is earlier than date2, returns 1
2. if equal, returns 0
3. if date2 is earlier than date1, returns -1"
  (let* ((date-list1 (parse-time-string date1))
         (year1 (nth 5 date-list1))
         (month1 (nth 4 date-list1))
         (day1 (nth 3 date-list1))
         (date-list2 (parse-time-string date2))
         (year2 (nth 5 date-list2))
         (month2 (nth 4 date-list2))
         (day2 (nth 3 date-list2)))
    (cond ((< year1 year2) 1)
          ((> year1 year2) -1)
          (t (cond ((< month1 month2) 1)
                   ((> month1 month2) -1)
                   (t (cond ((< day1 day2) 1)
                            ((> day1 day2) -1)
                            (t 0))))))))

(defun ego--fix-timestamp-string (date-string)
  "This is a piece of code copied from Xah Lee (I modified a little):
Returns yyyy-mm-dd format of date-string
For examples:
   [Nov. 28, 1994]     => [1994-11-28]
   [November 28, 1994] => [1994-11-28]
   [11/28/1994]        => [1994-11-28]
Any \"day of week\", or \"time\" info, or any other parts of the string, are
discarded.
Code detail: URL `http://xahlee.org/emacs/elisp_parse_time.html'"
  (let ((date-str (string-trim date-string))
        date-list year month date yyyy mm dd)
    (cond
     ;; USA convention of mm/dd/yyyy
     ((string-match
       "^\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)$"
       date-str)
      (concat (match-string 3 date-str) "-" (match-string 1 date-str) "-"
              (match-string 2 date-str)))
     ((string-match
       "^\\([0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)$"
       date-str)
      (concat (match-string 3 date-str) "-" (match-string 1 date-str) "-"
              (match-string 2 date-str)))
     ;; some ISO 8601. yyyy-mm-dd
     ((string-match
       "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)$\
T[0-9][0-9]:[0-9][0-9]" date-str)
      (concat (match-string 1 date-str) "-" (match-string 2 date-str) "-"
              (match-string 3 date-str)))
     ((string-match
       "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)$"
       date-str)
      (concat (match-string 1 date-str) "-" (match-string 2 date-str) "-"
              (match-string 3 date-str)))
     ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)$" date-str)
      (concat (match-string 1 date-str) "-" (match-string 2 date-str)))
     ((string-match "^\\([0-9][0-9][0-9][0-9]\\)$" date-str)
      (match-string 1 date-str))
     (t (progn
          ;; 兼容 2018年 07月 20日 星期五 18:15:29 CST 这种格式
          (setq date-str
                (replace-regexp-in-string "年 " "-" date-str))
          (setq date-str
                (replace-regexp-in-string "月 " "-" date-str))
          (setq date-str
                (replace-regexp-in-string "January " "Jan. " date-str))
          (setq date-str
                (replace-regexp-in-string "February " "Feb. " date-str))
          (setq date-str
                (replace-regexp-in-string "March " "Mar. " date-str))
          (setq date-str
                (replace-regexp-in-string "April " "Apr. " date-str))
          (setq date-str
                (replace-regexp-in-string "May " "May. " date-str))
          (setq date-str
                (replace-regexp-in-string "June " "Jun. " date-str))
          (setq date-str
                (replace-regexp-in-string "July " "Jul. " date-str))
          (setq date-str
                (replace-regexp-in-string "August " "Aug. " date-str))
          (setq date-str
                (replace-regexp-in-string "September " "Sep. " date-str))
          (setq date-str
                (replace-regexp-in-string "October " "Oct. " date-str))
          (setq date-str
                (replace-regexp-in-string "November " "Nov. " date-str))
          (setq date-str
                (replace-regexp-in-string "December " "Dec. " date-str))
          (setq date-str
                (replace-regexp-in-string " 1st," " 1" date-str))
          (setq date-str
                (replace-regexp-in-string " 2nd," " 2" date-str))
          (setq date-str
                (replace-regexp-in-string " 3rd," " 3" date-str))
          (setq date-str
                (replace-regexp-in-string "\\([0-9]\\)th," "\\1" date-str))
          (setq date-str
                (replace-regexp-in-string " 1st " " 1 " date-str))
          (setq date-str
                (replace-regexp-in-string " 2nd " " 2 " date-str))
          (setq date-str
                (replace-regexp-in-string " 3rd " " 3 " date-str))
          (setq date-str
                (replace-regexp-in-string "\\([0-9]\\)th " "\\1 " date-str))
          (setq date-list (parse-time-string date-str))
          (setq year (nth 5 date-list))
          (setq month (nth 4 date-list))
          (setq date (nth 3 date-list))
          (setq yyyy (number-to-string year))
          (setq mm (if month (format "%02d" month) ""))
          (setq dd (if date (format "%02d" date) ""))
          (concat yyyy "-" mm "-" dd))))))

(defun ego--confound-email-address (email)
  "Confound email to prevent spams using simple rule:
replace . with <dot>, @ with <at>, e.g.
name@domain.com => name <at> domain <dot> com"
  (if (not (ego--get-config-option :confound-email)) email
    (replace-regexp-in-string
     " +" " " (replace-regexp-in-string
               "@" " <at> " (replace-regexp-in-string "\\." " <dot> " email)))))


(defun ego--encode-string-to-url (string)
  "Encode STRING to legal URL. Why we do not use `url-encode-url' to encode the
string, is that `url-encode-url' will convert all not allowed characters into
encoded ones, like %3E, but we do NOT want this kind of url."
  (downcase (replace-regexp-in-string "[ :/\\\\?\\#]+" "-" string)))

(defun ego--get-full-url (uri)
  "Get the full url of URI, by joining site-domain with URI."
  (concat (replace-regexp-in-string "/?$" "" (ego--get-site-domain)) uri))

(defun ego--file-to-string (file)
  "Read the content of FILE and return it as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun ego--string-to-file (string file &optional mode)
  "Write STRING into FILE, only when FILE is writable. If MODE is a valid major mode, format the string with MODE's format settings."
  (when (file-writable-p file)
	(with-temp-buffer
	  (insert string)
	  (set-buffer-file-coding-system 'utf-8-unix)
	  (when (and mode (functionp mode))
		(funcall mode)
		(flush-lines "^[ \\t]*$" (point-min) (point-max))
		(delete-trailing-whitespace (point-min) (point-max))
		(indent-region (point-min) (point-max)))
      (write-region (point-min) (point-max) file))))

(defun ego--relative-url-to-absolute (html-content)
  "Force convert relative url of `html-content' to absolute url."
  (let ((site-domain (ego--get-site-domain))
        url)
    (with-temp-buffer
      (insert html-content)
      (goto-char (point-min))
      (while (re-search-forward
                ;;; TODO: not only links need to convert, but also inline
                ;;; images, may add others later
              ;; "<a[^>]+href=\"\\([^\"]+\\)\"[^>]*>\\([^<]*\\)</a>" nil t)
              "\\(<[a-zA-Z]+[^/>]+\\)\\(src\\|href\\)\\(=\"\\)\\([^\"]+\\)\\(\"[^>]*>\\)" nil t)
        (setq url (match-string 4))
        (when (string-prefix-p "/" url)
          (setq url (concat
                     (match-string 1)
                     (match-string 2)
                     (match-string 3)
                     site-domain url
                     (match-string 5)))
          (replace-match url)))
      (buffer-string))))

(defun ego--absolute-url-to-relative (html-content file-path)
  "Force convert relative url of `html-content' to absolute url."
  (let ((store-dir (file-name-as-directory (ego--get-config-option :store-dir)))
        (file-dir  (file-name-directory (expand-file-name file-path)))
        url)
    (with-temp-buffer
      (insert html-content)
      (goto-char (point-min))
      (while (re-search-forward
                ;;; TODO: not only links need to convert, but also inline
                ;;; images, may add others later
              ;; "<a[^>]+href=\"\\([^\"]+\\)\"[^>]*>\\([^<]*\\)</a>" nil t)
              "\\(<[a-zA-Z]+[^/>]+\\)\\(src\\|href\\)\\(=\"\\)\\([^\"]+\\)\\(\"[^>]*>\\)" nil t)
        (setq url (match-string 4))
        (when (string-prefix-p "/" url)
          (let* ((url-in-store-path (concat store-dir (substring-no-properties url 1)))
                 (relative-url (file-relative-name url-in-store-path file-dir))
                 (link (concat
                        (match-string 1)
                        (match-string 2)
                        (match-string 3)
                        relative-url
                        (match-string 5))))
            (replace-match link))))
      (buffer-string))))

(defun ego--html-link-transformer (content file)
  "Transform links in the CONTENT."
  (if (ego--get-config-option :force-absolute-url)
      (ego--relative-url-to-absolute content)
    (ego--absolute-url-to-relative content file)))

(defun ego--save-to-file (content file)
  "Save CONTENT into a html FILE, only when FILE is writable. Maybe do some transformation with the links.

If MODE is a valid major mode, format the string with MODE's format settings."
  (let* ((case-fold-search t)
         (file (expand-file-name file))
         (mode (and (string-match-p "html" (file-name-extension file))
                    'html)))
    (ego--string-to-file
     (ego--html-link-transformer content file)
     file mode)))

(defun ego--convert-plist-to-hashtable (plist)
  "Convert normal property list PLIST into hash table, keys of PLIST should be
in format :key, and it will be converted into \"key\" in hash table. This is an
alternative to `ht-from-plist'."
  (let ((h (ht-create)))
    (dolist (pair (ht/group-pairs plist) h)
      (let ((key (substring (symbol-name (car pair)) 1))
            (value (cadr pair)))
        (ht-set h key value)))))

(defun ego-add-to-alist (alist-var new-alist)
  "Add NEW-ALIST to the ALIST-VAR.
If an element with the same key as the key of an element of
NEW-ALIST is already present in ALIST-VAR, add the new values to
it; if a matching element is not already present, append the new
element to ALIST-VAR."
  ;; Loop over all elements of NEW-ALIST.
  (while new-alist
    (let* ((new-element (car new-alist))
		   ;; Get the element of ALIST-VAR with the same key of the current
		   ;; element of NEW-ALIST, if any.
		   (old-element (assoc (car new-element) (symbol-value alist-var))))
      (if old-element
		  (progn
			(set alist-var (delete old-element (symbol-value alist-var)))
			;; Append to `old-element' the values of the current element of
			;; NEW-ALIST.
			(mapc (lambda (elt) (add-to-list 'old-element elt t))
				  (cdr new-element))
			(set alist-var (add-to-list alist-var old-element t)))
		(add-to-list alist-var new-element t)))
    ;; Next element of NEW-ALIST.
    (setq new-alist (cdr new-alist))))

(defun ego--completing-read-multiple (prompt choices &optional predicate require-match initial-input hist def sentinel)
  "Read multiple items with completing-read. Reading stops
  when the user enters SENTINEL. By default, SENTINEL is
  \"*done*\". SENTINEL is disambiguated with clashing completions
  by appending _ to SENTINEL until it becomes unique. So if there
  are multiple values that look like SENTINEL, the one with the
  most _ at the end is the actual sentinel value. See
  documentation for `completing-read' for details on the
  other parameters."
  (let
      ((sentinel (or sentinel
                     "*done*"))
       (done-reading nil)
       (remain-choices choices)
       (res ()))

    ;; uniquify the SENTINEL value
    (while (cl-find sentinel choices)
      (setq sentinel (concat sentinel "_")))
    (setq remain-choices (cons sentinel choices))

    ;; read some choices
    (while (not done-reading)
      (let ((this-choice (completing-read prompt remain-choices predicate
                                              require-match initial-input hist def)))
        (if (equal this-choice sentinel)
            (setq done-reading t)
          (setq res (cons this-choice res))
          (setq remain-choices (delete this-choice remain-choices)))))

    ;; return the result
    res
    ))

(provide 'ego-util)

;;; ego-util.el ends here
