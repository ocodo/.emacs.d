;;; multi-line-shared.el --- multi-line statements -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Ivan Malison

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

;; multi-line-shared defines functions that are generally useful in
;; building multi-line strategies.

;;; Code:

(defun multi-line-clear-whitespace-at-point ()
  "Erase any surrounding whitespace."
  (interactive)
  (re-search-backward "[^[:space:]\n]")
  (forward-char)
  (let ((start (point)))
    (re-search-forward "[^[:space:]\n]")
    (backward-char)
    (kill-region start (point))))

(defun multi-line-add-trailing-comma (index markers)
  "Add a trailing comma when at the last marker.

INDEX is the index that will be used to determine whether or not
the action should be taken.  MARKERS is the list of markers that
were generated for the statement."
  (when (equal index (- (length markers) 1))
    (re-search-backward "[^[:space:]\n]")
    (when (not (looking-at ","))
      (forward-char)
      (insert ","))))

(provide 'multi-line-shared)
;;; multi-line-shared.el ends here
