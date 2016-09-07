;;; multi-line-enter.el --- multi-line statements -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Ivan Malison

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

;; multi-line-enter defines various approaches for going to the
;; beginning of a statement that can be multi-lined.

;;; Code:

(require 'eieio)
(require 'multi-line-shared)

(defclass multi-line-up-list-enter-strategy ()
  ((skip-chars :initarg :skip-chars :initform nil)))

(defmethod multi-line-enter ((enter multi-line-up-list-enter-strategy)
                             &optional _context)
  (multi-line-up-list-back)
  (when (oref enter skip-chars)
      (while (looking-at (format "[%s]" (oref enter skip-chars)))
    (forward-char)))
  (forward-char))

(provide 'multi-line-enter)
;;; multi-line-enter.el ends here
