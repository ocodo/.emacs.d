;;; cbm.el --- Switch to similar buffers.

;; Copyright 2015 Lukas Fürmetz

;; Author: Lukas Fürmetz <fuermetz@mailbox.org>
;; URL: http://github.com/akermu/cbm.el
;; Package-Version: 20171116.440
;; Package-Requires: ((cl-lib "0.5"))
;; Version: 0.3
;; Keywords: buffers

;; cbm.el is free software: you can redistribute it and/or modify
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
;;
;; (require 'cbm)

;; It is recommended to bind `cbm-cycle', `cbm-switch-buffer' and
;; `cbm-find-org-agenda-file' to a key:
;;
;; (global-set-key (kbd "C-;") #'cbm-cycle)
;; (global-set-key (kbd "C-'") #'cbm-switch-buffer)
;; (global-set-key (kbd "C-c o") #'cbm-find-org-agenda-file)
;; (define-key rcirc-mode-map (kbd "M-i") #'cbm-rcirc-switch-to-channel)

;; Usage:

;; This package provides useful commands for switching to similar
;; buffers. It's particularly handy for switching between buffers in
;; the same major mode.

;;; Code:
(require 'cl-lib)

(declare-function org-agenda-files "org")
(declare-function rcirc-mode "rcirc")

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
  (setq cbm-buffers (sort cbm-buffers
                          #'(lambda (buffer1 buffer2)
                              (string< (buffer-name buffer1)
                                       (buffer-name buffer2)))))
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

;;;###autoload
(defun cbm-switch-buffer ()
  "Switch to buffer, filtered by `major-mode'."
  (interactive)
  (let* ((mm (with-current-buffer (current-buffer)
               major-mode))
         (buffer-list
          (mapcar #'buffer-name
                  (remove (current-buffer)
                          (cl-remove-if-not
                           (lambda (buf)
                             (eq mm (with-current-buffer buf
                                      major-mode)))
                           (buffer-list))))))
    (switch-to-buffer
     (completing-read (format "Switch to %s buffer: " mm)
                      buffer-list nil t))))

;;;###autoload
(defun cbm-find-org-agenda-file ()
  "Switch to a file from `org-agenda-files'."
  (interactive)
  (require 'org nil :noerror)
  (unless (fboundp #'org-agenda-files)
    (error "Please install `org-mode'"))
  (let* ((files (remove (buffer-file-name) (org-agenda-files t)))
         (file-alist (mapcar #'(lambda (elem)
                                 `(,(file-name-nondirectory elem) . ,elem))
                             files)))
    (when (eq (length file-alist) 0)
      (error "Cannot find another file in `org-agenda-files'"))
    (if (eq (length file-alist) 1)
        (find-file (cdar file-alist))
      (let* ((name (completing-read "Switch to org-file: " file-alist nil t))
             (elem (assoc name file-alist))
             (path (cdr elem)))
      (find-file path)))))

;;;###autoload
(defun cbm-rcirc-switch-to-channel ()
  "Switch to a rcirc channel."
  (interactive)
  (let* ((channel-alist
          (mapcar (lambda (buf)
                    `(,(replace-regexp-in-string "@.*" "" (buffer-name buf)) . ,buf))
                  (cl-remove-if-not (lambda (buf)
                                      (with-current-buffer buf
                                        (eq major-mode #'rcirc-mode)))
                                    (buffer-list))))
         (channel
          (completing-read "Switch to channel: "
                           channel-alist
                           nil
                           t
                           "#")))
    (when channel
      (switch-to-buffer (cdr (assoc channel channel-alist))))))


(provide 'cbm)
;;; cbm.el ends here
