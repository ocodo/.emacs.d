;;; ac-html.el --- auto complete source for html tags and attributes

;; Copyright (C) 2014 - 2015 Zhang Kai Yu

;; Author: Zhang Kai Yu <yeannylam@gmail.com>
;; Version: 0.4.alpha
;; Keywords: html, auto-complete, slim, haml, jade
;; Package-Requires: ((auto-complete "1.4") (s "1.9") (f "0.17") (dash "2.10"))
;; URL: https://github.com/cheunghy/ac-html

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

;;; Code:

(require 'ac-html-core)

(defun ac-html--inside-attrv ()
  "Return t if cursor inside attrv aka string.
Has bug for quoted quote."
  (save-match-data
    (save-excursion
      (re-search-backward "\\w+[\n\t ]*=[\n\t ]*[\"']\\([^\"']*\\)" nil t))
    (equal (match-end 1) (point))))

(defun ac-html--inside-comment ()
  "Return t if cursor inside comment.
Not implemented yet.")

;;; auto complete HTML for html-mode and web-mode

(defun ac-html-tag-prefix ()
  (if (ac-html--inside-attrv)
      nil
    (save-match-data
      (save-excursion
        (re-search-backward "<\\([^\n\t >'\"]*\\)" nil t))
      (match-beginning 1))))

(defun ac-html-attr-prefix ()
  (if (ac-html--inside-attrv)
      nil
    (save-match-data
      (save-excursion
        (re-search-backward "<\\w[^>]*[[:space:]]+\\(.*\\)" nil t))
      (match-beginning 1))))

(defun ac-html-value-prefix ()
  (if (re-search-backward "\\w=[\"]\\([^\"]+[ ]\\|\\)\\(.*\\)" nil t)
      (match-beginning 2)))

(defun ac-html-current-tag ()
  "Return current html tag user is typing on.
There is a bug if attrv contains string like this <a"
  (save-excursion
    (save-match-data
      (re-search-backward "<\\(\\w+\\)[[:space:]]+" nil t)
      (match-string 1))))

(defun ac-html-current-attr ()
  "Return current html tag's attribute user is typing on.
There is a bug if attrv contains string like this href="
  (save-excursion
    (re-search-backward "[^a-z-]\\([a-z-]+\\)[\n\t ]*=" nil t)
    (match-string 1)))

(ac-html-define-ac-source "html"
  :tag-prefix ac-html-tag-prefix
  :attr-prefix ac-html-attr-prefix
  :attrv-prefix ac-html-value-prefix
  :current-tag-func ac-html-current-tag
  :current-attr-func ac-html-current-attr)

(provide 'ac-html)
;;; ac-html.el ends here
