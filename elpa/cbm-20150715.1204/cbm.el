;;; cbm.el --- Cycle through buffers with the same `major-mode'.

;; Copyright 2015 Lukas Fürmetz

;; Author: Lukas Fürmetz <fuermetz@mailbox.org>
;; URL: http://github.com/akermu/cbm.el
;; Package-Version: 20150715.1204
;; Version: 0.1
;; Keywords: buffers, cycling

;; cmb.el is free software: you can redistribute it and/or modify
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

;; Installation:

;; Put cbm.el in your `load-path' and require it:
;; (require 'cbm)

;; It is recommended to bind `cbm-cycle' to a key:
;; (global-set-key (kbd "C-'") #'cbm-cycle)

;; Usage:

;; This package provides one usefull command `cbm-cycle', which cycles
;; through all buffers with the same `major-mode' as the
;; current-buffer.

;;; Code:

(defvar cbm-buffers nil
  "Holds current cycling-list.")

(defun cbm-rotate ()
  "Rotates `cbm-buffers' until `current-buffer' is the first element."
  (while (not (eq (car cbm-buffers) (current-buffer)))
    (let ((elem (car cbm-buffers))
          (rest (cdr cbm-buffers)))
      (setq cbm-buffers (append rest `(,elem))))))

(defun cbm-make-buffer-list ()
  "Initialize `cbm-buffers' with all buffers with the same `major-mode'."
  (let* ((mode (with-current-buffer (current-buffer)
                 major-mode)))
    (dolist (buffer (buffer-list))
      (when (eq
             (with-current-buffer buffer
               major-mode)
             mode)
        (push buffer cbm-buffers))))
  (setq cbm-buffers (sort cbm-buffers #'(lambda (buffer1 buffer2)
                                          (string< (buffer-name buffer1) (buffer-name buffer2)))))
  (cbm-rotate))

;;;###autoload
(defun cbm-cycle ()
  "Cycles through buffers with same `major-mode'."
  (interactive)
  (unless (eq last-command #'cbm-cycle)
    (setq cbm-buffers nil))
  (unless cbm-buffers
    (cbm-make-buffer-list))
  (setq cbm-buffers (delq (current-buffer) cbm-buffers))
  (setq cbm-buffers (append cbm-buffers `(,(current-buffer))))
  (let ((buffer (car cbm-buffers)))
    (when (bufferp buffer)
      (switch-to-buffer buffer))))

(provide 'cbm)
;;; cbm.el ends here
