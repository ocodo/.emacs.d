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

;; Configuration:
;;
;; Add to hook `ac-haml-enable'
;; 
;; (add-hook 'haml-mode-hook 'ac-haml-enable)


;;; Code:

(require 'ac-html)

(defun ac-html--current-haml-tag ()
  "Return current haml tag user is typing on."
  (let* ((tag-search (save-excursion
                       (re-search-backward "%\\(\\w+\\)" nil t)))
         (tag-string (match-string 1)))
    tag-string))

(defun ac-html--current-haml-attribute ()
  "Return current html tag's attribute user is typing on."
  (let* ((attr-search (save-excursion
		       (re-search-backward "[^a-z-]\\([a-z-]+\\) *=" nil t)))
	 (attr-string (match-string 1)))
    attr-string))

(defun ac-source-haml-attribute-candidates ()
  (ac-html--attribute-candidates (ac-html--current-haml-tag)))

(defun ac-source-haml-tag-candidates ()
  ac-html-all-element-list)

(defun ac-source-haml-attribute-documentation (symbol)
  (ac-html--attribute-documentation symbol
                                    (ac-html--current-haml-tag)))

(defun ac-source-haml-value-candidates ()
  (ac-source--html-attribute-values
   (ac-html--current-haml-tag) (ac-html--current-haml-attribute)))

(defun ac-source-haml-attribute-value-document (symbol)
  (ac-source--html-attribute-value-document symbol
                                            (ac-html--current-haml-tag) (ac-html--current-haml-attribute)))

(defun ac-haml-value-prefix ()
  ;; %foo{ bar => ""}
  ;; %foo( :bar = "")
  (if (re-search-backward "\\w+ *=[>]? *[\"]\\([^\"]+[ ]\\|\\)\\(.*\\)" nil t)
      (match-beginning 2)))

(defvar ac-source-haml-tag
  '((candidates . ac-source-haml-tag-candidates)
    (prefix . "%\\(.*\\)")
    (symbol . "t")
    (document . ac-source--html-tag-documentation)))

(defvar ac-source-haml-attribute
  '((candidates . ac-source-haml-attribute-candidates)
    (prefix . ":\\(.*\\)")
    (symbol . "a")
    (document . ac-source-haml-attribute-documentation)))

(defvar ac-source-haml-attribute-value
  '((candidates . ac-source-haml-value-candidates)
    (prefix . ac-haml-value-prefix)
    (symbol . "v")
    (document . ac-source-haml-attribute-value-document)
    ))

(defun ac-haml-enable ()
  "Add ac-haml sources into ac-sources and enable auto-comple-mode"
  (interactive)
  (mapc (lambda (source)
	  (if (not (memq source ac-sources))
	      (add-to-list 'ac-sources source)))
	'(ac-source-haml-attribute-value ac-source-haml-attribute ac-source-haml-tag))

  ;; ac-source-haml-attribute-value complete in font-lock-string-face, must not be disabled
  (make-local-variable 'ac-disable-faces)
  (setq ac-disable-faces (remove 'font-lock-string-face ac-disable-faces))
  (auto-complete-mode t))

(provide 'ac-haml)
;;; ac-haml.el ends here
