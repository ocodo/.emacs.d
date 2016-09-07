;;; multi-line-highlight.el --- multi-line statements -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: multi line length whitespace programming
;; URL: https://github.com/IvanMalison/multi-line
;; Package-Requires: ((emacs "24") (s "1.9.0") (cl-lib "0.5") (dash "2.12.0") (shut-up "0.3.2"))
;; Version: 0.1.4

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

;; Highlight respacing candidates.

;;; Code:

(require 'multi-line)

(defvar multi-line-overlays-to-remove nil)

(defun multi-line-clean-regions ()
  "Remove any existing multi-line highlight overlays."
  (interactive)
  (cl-loop for overlay in multi-line-overlays-to-remove
            do (delete-overlay overlay)))

;;;###autoload
(defun multi-line-highlight-current-candidates ()
  "Highlight the positions at which multi-line will consider adding newlines."
  (interactive)
  (let ((candidates (save-excursion
                      (multi-line-find (oref multi-line-current-strategy find)))))
    (setq multi-line-overlays-to-remove
          (nconc
           (cl-loop for candidate in candidates
                    collect (multi-line-highlight-candidate candidate))))))

(defun multi-line-highlight-candidate (candidate)
  (let* ((position (multi-line-candidate-position candidate))
         (overlay (make-overlay position (1+ position))))
    (overlay-put overlay 'face 'highlight)
    overlay))

(provide 'multi-line-highlight)
;;; multi-line-highlight.el ends here
