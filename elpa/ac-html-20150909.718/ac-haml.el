;;; ac-haml.el --- auto complete source for html tag and attributes

;; Copyright (C) 2014 - 2015 Zhang Kai Yu

;; Author: Zhang Kai Yu <yeannylam@gmail.com>
;; Keywords: html, auto-complete, rails, ruby

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

(defun ac-haml-current-tag ()
  "Return current haml tag user is typing on."
  (save-excursion (re-search-backward "%\\(\\w+\\)" nil t))
  (match-string 1))

(defun ac-haml-current-attr ()
  "Return current html tag's attribute user is typing on."
  (save-excursion (re-search-backward "[^a-z-]\\([a-z-]+\\) *=" nil t))
  (match-string 1))

(defun ac-haml-attrv-prefix ()
  (if (re-search-backward "\\w+ *=[>]? *[\"]\\([^\"]+[ ]\\|\\)\\(.*\\)" nil t)
      (match-beginning 2)))

(ac-html-define-ac-source "haml"
  :tag-prefix "%\\(.*\\)"
  :attr-prefix ":\\(.*\\)"
  :attrv-prefix ac-haml-attrv-prefix
  :current-tag-func ac-haml-current-tag
  :current-attr-func ac-haml-current-attr)

(provide 'ac-haml)
;;; ac-haml.el ends here
