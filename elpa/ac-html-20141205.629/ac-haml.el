;;; ac-haml.el --- auto complete source for html tag and attributes

;; Copyright (C) 2014 Zhang Kai Yu

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

;; If you are using haml-mode, you need require this file.

;;; Code:

(require 'ac-html)

(defun ac-html--current-haml-tag ()
  "Return current haml tag user is typing on."
  (let* ((tag-search (save-excursion
                       (re-search-backward "%\\(\\w+\\)" nil t)))
         (tag-string (match-string 1)))
    (message tag-string)
    tag-string))

(defun ac-source-haml-attribute-candidates ()
  (ac-html--attribute-candidates (ac-html--current-haml-tag)))

(defun ac-source-haml-tag-candidates ()
  ac-html-all-element-list)

(defun ac-source-haml-attribute-documentation (symbol)
  (ac-html--attribute-documentation symbol
                                    (ac-html--current-haml-tag)))

(defvar ac-source-haml-tag
  '((candidates . ac-source-haml-tag-candidates)
    (prefix . "%\\(.*\\)")
    (symbol . "t")
    (document . ac-source-html-tag-documentation)))

(defvar ac-source-haml-attribute
  '((candidates . ac-source-haml-attribute-candidates)
    (prefix . ":\\(.*\\)")
    (symbol . "a")
    (document . ac-source-haml-attribute-documentation)))

(provide 'ac-haml)
;;; ac-haml.el ends here
