;;; ac-jade.el --- auto complete source for html tag and attributes

;; Copyright (C) 2014 Zhang Kai Yu, Olexandr Sydorchuck

;; Author: Zhang Kai Yu <yeannylam@gmail.com>
;; Keywords: html, auto-complete, jade, node

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

;; Configuration:
;;

;;; Code:

(require 'ac-html-core)

(defun ac-jade-current-tag ()
  "Return current jade tag user is typing on."
  (save-excursion (re-search-backward "^[\t ]*\\(\\w+\\)" nil t))
  (match-string 1))

(defun ac-jade-current-attr ()
  "Return current html tag's attribute user is typing on."
  (save-excursion (re-search-backward "[^a-z-]\\([a-z-]+\\) *=" nil t))
  (match-string 1))

(defun ac-jade-attrv-prefix ()
  (if (re-search-backward "\\w *= *[\"]\\([^\"]+[ ]\\|\\)\\(.*\\)" nil t)
      (match-beginning 2)))

(ac-html-define-ac-source "jade"
  :tag-prefix "^[\t ]*\\(.*\\)"
  :attr-prefix "\\(?:,\\|(\\)[ ]*\\(.*\\)"
  :attrv-prefix ac-jade-attrv-prefix
  :current-tag-func ac-jade-current-tag
  :current-attr-func ac-jade-current-attr)

(provide 'ac-jade)
;;; ac-jade.el ends here
