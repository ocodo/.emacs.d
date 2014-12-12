;;; ac-slim.el --- auto complete source for html tag and attributes

;; Copyright (C) 2014 Zhang Kai Yu, Olexandr Sydorchuck

;; Author: Zhang Kai Yu <yeannylam@gmail.com>
;; Keywords: html, auto-complete, slim, ruby

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
;; Add to hook `ac-slim-enable'
;; 
;; (add-hook 'slim-mode-hook 'ac-slim-enable)

;;; Code:

(require 'ac-html)

(defun ac-html--current-slim-tag ()
  "Return current slim tag user is typing on."
  (let* ((tag-search (save-excursion
                       (re-search-backward "^[\t ]*\\(\\w+\\)" nil t)))
         (tag-string (match-string 1)))
    tag-string))

(defun ac-html--current-slim-attribute ()
  "Return current html tag's attribute user is typing on."
  (let* ((attr-search (save-excursion
		       (re-search-backward "[^a-z-]\\([a-z-]+\\) *=" nil t)))
	 (attr-string (match-string 1)))
    attr-string))

(defun ac-source-slim-attribute-candidates ()
  (ac-html--attribute-candidates (ac-html--current-slim-tag)))

(defun ac-source-slim-tag-candidates ()
  ac-html-all-element-list)

(defun ac-source-slim-attribute-documentation (symbol)
  (ac-html--attribute-documentation symbol
                                    (ac-html--current-slim-tag)))

(defun ac-source-slim-value-candidates ()
  (ac-source--html-attribute-values
    (ac-html--current-slim-tag) (ac-html--current-slim-attribute))
  )

(defun ac-source-slim-attribute-value-document (symbol)
  (ac-source--html-attribute-value-document symbol
                                            (ac-html--current-slim-tag) (ac-html--current-slim-attribute)))


(defun ac-slim-value-prefix ()
  (if (re-search-backward "\\w *= *[\"]\\([^\"]+[ ]\\|\\)\\(.*\\)" nil t)
      (match-beginning 2)))

(defvar ac-source-slim-tag
  '((candidates . ac-source-slim-tag-candidates)
    (prefix . "^[\t ]*\\(.*\\)")
    (symbol . "t")
    (document . ac-source--html-tag-documentation)))

(defvar ac-source-slim-attribute
  '((candidates . ac-source-slim-attribute-candidates)
    (prefix . " \\(.*\\)")
    (symbol . "a")
    (document . ac-source-slim-attribute-documentation)
))

(defvar ac-source-slim-attribute-value
  '((candidates . ac-source-slim-value-candidates)
    (prefix . ac-slim-value-prefix)
    (symbol . "v")
    (document . ac-source-slim-attribute-value-document)
    ))

(defun ac-slim-enable ()
  "Add ac-slim sources into ac-sources and enable auto-comple-mode"
  (interactive)
  (mapc (lambda (source)
	  (if (not (memq source ac-sources))
	      (add-to-list 'ac-sources source)))
	'(ac-source-slim-attribute-value ac-source-slim-attribute ac-source-slim-tag))

  ;; ac-source-slim-attribute-value complete in font-lock-string-face, must not be disabled
  (make-local-variable 'ac-disable-faces)
  (setq ac-disable-faces (remove 'font-lock-string-face ac-disable-faces))
  (auto-complete-mode t))

(provide 'ac-slim)
;;; ac-slim.el ends here
