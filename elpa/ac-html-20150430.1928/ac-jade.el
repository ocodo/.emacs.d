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
;; Add to hook `ac-jade-enable'
;; 
;; (add-hook 'jade-mode-hook 'ac-jade-enable)

;;; Code:

(require 'ac-html)

(defun ac-jade-current-tag ()
  "Return current jade tag user is typing on."
  (save-excursion (re-search-backward "^[\t ]*\\(\\w+\\)" nil t))
  (match-string 1))

(defun ac-jade-current-attribute ()
  "Return current html tag's attribute user is typing on."
  (save-excursion (re-search-backward "[^a-z-]\\([a-z-]+\\) *=" nil t))
  (match-string 1))

(defun ac-source-jade-attribute-candidates ()
  (ac-html--attribute-candidates (ac-jade-current-tag)
				 #'(lambda (symbol)
				     (ac-html--attribute-documentation symbol (ac-jade-current-tag)))))

(defun ac-source-jade-tag-candidates ()
  (ac-html--tags))

(defun ac-source-jade-attribute-documentation (symbol)
  (ac-html--attribute-documentation symbol
                                    (ac-jade-current-tag)))

(defun ac-source-jade-value-candidates ()
   (ac-source--html-attribute-values
    (ac-jade-current-tag) (ac-jade-current-attribute))
  )

(defun ac-jade-value-prefix ()
  (if (re-search-backward "\\w *= *[\"]\\([^\"]+[ ]\\|\\)\\(.*\\)" nil t)
      (match-beginning 2)))

(defvar ac-source-jade-tag
  '((candidates . ac-source-jade-tag-candidates)
    (prefix . "^[\t ]*\\(.*\\)")
    (symbol . "t")))

(defvar ac-source-jade-attribute
  '((candidates . ac-source-jade-attribute-candidates)
    (prefix . "\\(?:,\\|(\\)[ ]*\\(.*\\)")
    (symbol . "a")))

(defvar ac-source-jade-attribute-value
  '((candidates . ac-source-jade-value-candidates)
    (prefix . ac-jade-value-prefix)
    (symbol . "v")))

;;;###autoload
(defun ac-jade-enable ()
  "Add ac-jade sources into ac-sources and enable auto-comple-mode"
  (interactive)
  (mapc (lambda (source)
	  (if (not (memq source ac-sources))
	      (add-to-list 'ac-sources source)))
	'(ac-source-jade-attribute-value ac-source-jade-attribute ac-source-jade-tag))

  ;; ac-source-jade-attribute-value complete in font-lock-string-face, must not be disabled
  (make-local-variable 'ac-disable-faces)
  (setq ac-disable-faces (remove 'font-lock-string-face ac-disable-faces))
  (auto-complete-mode t))

(provide 'ac-jade)
;;; ac-jade.el ends here
