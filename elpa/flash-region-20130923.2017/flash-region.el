;;; flash-region.el --- Flash a region

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: utility
;; Version: 20130923.2017
;; X-Original-Version: 1.0
;; Created: 19th Sep 2013

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

;; This package provides one function, `flash-region' that will
;; highlight a region with the supplied face for a specified amount
;; of time.  By default, the `highlight' face is used and the
;; timeout is set to 0.5

;;; Code:

(defvar flash-region-ovl nil
  "The overlay used for flash.")
(make-variable-buffer-local 'flash-region-ovl)

(defun flash-region--remove-ovl (buf)
  "Remove the flash overlay if it exists in BUF."
  (with-current-buffer buf
    (when (overlayp flash-region-ovl)
      (delete-overlay flash-region-ovl))
    (setq flash-region-ovl nil)))

;;;###autoload
(defun flash-region (beg end &optional face timeout)
  "Show an overlay from BEG to END using FACE to set display
properties.  The overlay automatically vanishes after TIMEOUT
seconds."
  (interactive "r")
  (setq face (or face 'highlight))
  (setq timeout (or (and (numberp timeout) (< 0 timeout) timeout) 0.5))
  (flash-region--remove-ovl (current-buffer))
  (setq flash-region-ovl (make-overlay beg end))
  (overlay-put flash-region-ovl 'face face)
  (when (< 0 timeout)
    (run-with-idle-timer timeout
                         nil 'flash-region--remove-ovl (current-buffer))))

(provide 'flash-region)

;;; flash-region.el ends here
