;;; zappers.el --- A set of zap to / zap up to commands

;; Author: Jason Milkins <jasonm23@gmail.com>

;; URL: https://github.com/ocodo/.emacs.d/plugins/zappers.el

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
;;  Zap up to / Zap to commands (to string / regexp)
;;

;;; Code:

(defun zap-to-string (&optional arg)
  "Zap text up to a string, ARG can be minus to zap backwards."
  (interactive "p")
  (let ((text (read-from-minibuffer "Zap to string: ")))
    (kill-region (point) (progn
                           (search-forward text nil nil arg)
                           (point)))))

(defun zap-up-to-string (&optional arg)
  "Zap text up to a string, ARG can be minus to zap backwards."
  (interactive "p")
  (let ((text (read-from-minibuffer "Zap up to string: ")))
    (kill-region (point) (progn
                           (search-forward text nil nil arg)
                           (backward-char (* arg (length text)))
                           (point)))))

(defun zap-to-regexp (&optional arg)
  "Zap text up to a regexp, ARG can be minus to zap backwards."
  (interactive "p")
  (let ((regexp (read-from-minibuffer "Zap to regexp: ")))
    (kill-region (point) (progn
                           (re-search-forward regexp nil nil arg)
                           (point)))))

(defun zap-up-to-regexp (&optional arg)
  "Zap text up to a regexp, ARG can be minus to zap backwards."
  (interactive "p")
  (let
      ((regexp (read-from-minibuffer "Zap up to regexp: ")))
    (kill-region (point) (progn
                           (re-search-forward regexp nil nil arg)
                           (re-search-backward regexp nil nil arg)
                           (point)))))

(provide 'zappers)
;;; zappers.el ends here
