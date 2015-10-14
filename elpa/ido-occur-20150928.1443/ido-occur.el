;;; ido-occur.el --- Yet another `occur' with `ido'.

;; Copyright (C) 2015 Danil <danil@kutkevich.org>.
;; Author: Danil <danil@kutkevich.org>
;; Version: 0.1.0
;; Package-Version: 20150928.1443
;; Package-Requires: ((ido-vertical-mode "1.0.0") (dash "2.11.0"))
;; Keywords: inner buffer search
;; URL: https://github.com/danil/ido-occur

;;; Commentary:
;; Yet another `occur' with `ido'.
;; See the README.md for more details.

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'ido)
(require 'ido-vertical-mode)
(require 'dash)

(defgroup ido-occur nil
  "Yet another `occur' with `ido'."
  :group 'help
  :group 'convenience)

(defcustom ido-occur--prompt "List lines matching: "
  "Minibuffer prompt."
  :type 'string
  :group 'ido-occur)

(defun ido-occur--lines-as-string (buffer)
  "Get all lines with properties of the `BUFFER'."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring (point-min) (point-max)))))

(defun ido-occur--decorate-lines-list (lines)
  "Add line number to each string in `LINES'."

  (let* ((lines-count (number-to-string (length lines))))

    (-map-indexed (lambda (index str)
                    "Transform `INDEX' to number and add to `STR'."

                    (let* ((line-number (+ index 1))

                           (number-length (number-to-string (length lines-count)))

                           (formated-line-number (format (concat "%" number-length "s")
                                                         line-number)))

                      (format "%s %s" formated-line-number str)))

                  lines)))

(defun ido-occur--lines-as-list (buffer line-number)
  "List all lines of `BUFFER' with respects to current `LINE-NUMBER'.
List lines from `LINE-NUMBER' to end of `BUFFER'
and from end of `BUFFER' to beginning of `BUFFER'."

  (let ((line-number (line-number-at-pos))

        (lines (ido-occur--decorate-lines-list
                (split-string (ido-occur--lines-as-string buffer) "\n"))))

    (-concat (-drop (- line-number 1) lines)
             (-take line-number lines))))

(defun ido-occur--strip-text-properties (txt)
  "Strip text properties from `TXT'."
  (set-text-properties 0 (length txt) nil txt) txt)

;;;###autoload
(defun ido-occur ()
  "Yet another `occur' with `ido'."

  (interactive)
  (ido-vertical-mode t)

  (let* ((initial-column (current-column))

         (line (ido-occur--strip-text-properties
                (ido-completing-read ido-occur--prompt
                                     (ido-occur--lines-as-list (current-buffer)
                                                               (line-number-at-pos)))))
         (line-length (length line))

         (new-column (if (<= line-length initial-column)
                         line-length
                       initial-column))

         (line-number (string-to-number (car (split-string line)))))

    (goto-line line-number)
    (beginning-of-line)
    (move-to-column new-column)))

(provide 'ido-occur)

;;; ido-occur.el ends here
