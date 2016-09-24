;;; memory-values.el --- A set of computer memory value functions (convert to bytes)

;; Author: Jason Milkins <jasonm23@gmail.com>

;; URL: https://github.com/ocodo/.emacs.d/plugins/memory-values.el

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
;;  A small collection of computer memory convenience functions,
;;  to convert large denominations to bytes.
;;

;;; Code:

(defun kilobyte (n)
  "N kilobytes to bytes."
  (* n 1024))

(defalias 'Kb 'kilobyte)

(defun megabyte (n)
  "N megabytes to bytes."
  (* (kilobyte n) 1024))

(defalias 'Mb 'megabyte)

(defun gigabyte (n)
  "N gigabytes to bytes."
  (* (megabyte n) 1024))

(defalias 'Gb 'gigabyte)

(defun terabyte (n)
  "N terabytes to bytes."
  (* (gigabyte n) 1024))

(defalias 'Tb 'terabyte)

(provide 'memory-values)
;;; memory-values.el ends here
