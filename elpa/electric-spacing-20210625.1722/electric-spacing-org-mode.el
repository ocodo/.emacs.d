;;; electric-spacing-org-mode.el --- org mode tunings

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: William Xu <william.xwl@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'electric-spacing)

(defun electric-spacing-org-mode-inline-marker (op)
  "For inserting inline markers, like *bold* or /italic/."
  (let ((line (buffer-substring-no-properties (line-beginning-position) (point))))
    (if (zerop (% (count (string-to-char op) line) 2))
        (electric-spacing-insert op 'before)
      (electric-spacing-insert op 'after))))

(defun electric-spacing-org-mode-* ()
  (electric-spacing-org-mode-inline-marker "*"))

(defun electric-spacing-org-mode-/ ()
  (electric-spacing-org-mode-inline-marker "/"))

(provide 'electric-spacing-org-mode)
;;; electric-spacing-org-mode.el ends here
