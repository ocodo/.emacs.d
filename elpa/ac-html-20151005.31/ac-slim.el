;;; ac-slim.el --- auto complete source for html tag and attributes

;; Copyright (C) 2015  Zhang Kai Yu

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

;;; Code:

(require 'ac-html-core)

(require 's)

(defun ac-slim-inside-ruby-code ()
  "Return t if inside ruby code."
  (let ((line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (numberp (string-match-p "^[\t ]*[-=]" line))))

(defun ac-slim--line-leading-spaces ()
  "Return leading space of current line."
  (let ((saved-point (point)) (number-of-spaces 0) (cur-point nil))
    (goto-char (line-beginning-position))
    (setq cur-point (point))
    (while (-contains-p '(?  ?\t) (char-after cur-point))
      (setq number-of-spaces (1+ number-of-spaces))
      (setq cur-point (1+ cur-point))
      (goto-char cur-point))
    (goto-char saved-point)
    number-of-spaces))

(defun ac-slim--line-is-block-indicator ()
  "Return t if line indicate a non slim block."
  (let ((content-of-line (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))))
    (numberp (string-match-p
              "[ \t]*\\(ruby\\|javascript\\|coffee\\):[ \t]*"
              content-of-line))))

(defun ac-slim--line-is-empty ()
  "Return t if line is empty."
  (s-matches-p "^[ \t]*$" (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))

(defun ac-slim-inside-non-slim-block ()
  "Return t if inside ruby block, coffee block."
  (catch 'blk
    (save-excursion
      (let ((min-number-of-leading-spaces (ac-slim--line-leading-spaces)))
        (if (ac-slim--line-is-block-indicator)
            (throw 'blk t))
        (while (not (or (= min-number-of-leading-spaces 0)
                        (= 1 (line-number-at-pos))))
          (forward-line -1)
          (if (ac-slim--line-is-empty)
              nil
            (if (< (ac-slim--line-leading-spaces) min-number-of-leading-spaces)
                (progn
                  (setq min-number-of-leading-spaces (ac-slim--line-leading-spaces))
                  (if (ac-slim--line-is-block-indicator)
                      (throw 'blk t))))))))))

(defun ac-slim-tag-prefix ()
  (and (not (ac-slim-inside-ruby-code))
       (not (ac-slim-inside-non-slim-block))
       (save-match-data
         (save-excursion
           (re-search-backward "\\(^[\t ]*\\|:[\t ]*\\)\\([a-zA-Z]*\\)" nil t)
           (match-beginning 2)))))

(defun ac-slim-attr-prefix ()
  (and (not (ac-slim-inside-ruby-code))
       (not (ac-slim-inside-non-slim-block))
       (not (ac-slim-attrv-prefix))
       (save-match-data
         (save-excursion
           (re-search-backward " \\(.*\\)" nil t)
           (match-beginning 1)))))

(defun ac-slim-attrv-prefix ()
  (and (not (ac-slim-inside-ruby-code))
       (not (ac-slim-inside-non-slim-block))
       (let (mb2 me2)
         (save-excursion
           (save-match-data
             (if (re-search-backward
                  "\\w *= *[\"']\\([^\"']+[ ]\\|\\)\\([^\"']*\\)"
                  (line-beginning-position) t)
                 (progn
                   (setq mb2 (match-beginning 2))
                   (setq me2 (match-end 2))))))
         (if (and mb2 (>= me2 (point)))
             mb2))))

(defun ac-slim-class-prefix ()
  (and (not (ac-slim-inside-ruby-code))
       (not (ac-slim-inside-non-slim-block))
       (save-match-data
         (save-excursion
           (re-search-backward "\\(^[\t ]*\\|:[\t ]*\\)\\([a-zA-Z0-9#.]*\\.\\)\\([a-zA-Z0-9]\\)" nil t)
           (match-beginning 3)))))

(defun ac-slim-id-prefix ()
  (and (not (ac-slim-inside-ruby-code))
       (not (ac-slim-inside-non-slim-block))
       (save-match-data
         (save-excursion
           (re-search-backward "\\(^[\t ]*\\|:[\t ]*\\)\\([a-zA-Z0-9.]*#\\)\\([a-zA-Z0-9]\\)" nil t)
           (match-beginning 3)))))

(defun ac-slim-current-tag ()
  "Return current slim tag user is typing on."
  (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                               (line-end-position)))
         (match-result (s-match "\\(^[\t ]*\\|:[\t ]*\\)\\(\\w+\\)" line)))
    (if match-result
        (nth 2 match-result)
      "div")))

(defun ac-slim-current-attr ()
  "Return current html tag's attribute user is typing on."
  (save-excursion (re-search-backward "[^a-z-]\\([a-z-]+\\) *=" nil t))
  (match-string 1))

(ac-html-define-ac-source "slim"
  :tag-prefix ac-slim-tag-prefix
  :attr-prefix ac-slim-attr-prefix
  :attrv-prefix ac-slim-attrv-prefix
  :class-prefix ac-slim-class-prefix
  :id-prefix ac-slim-id-prefix
  :current-tag-func ac-slim-current-tag
  :current-attr-func ac-slim-current-attr)

(provide 'ac-slim)
;;; ac-slim.el ends here
