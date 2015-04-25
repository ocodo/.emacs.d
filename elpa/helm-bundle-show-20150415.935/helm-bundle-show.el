;;; helm-bundle-show.el --- bundle show with helm interface

;; Copyright (C) 2015 by Takashi Masuda

;; Author: Takashi Masuda <masutaka.net@gmail.com>
;; URL: https://github.com/masutaka/emacs-helm-bundle-show
;; Package-Version: 20150415.935
;; Version: 1.0.1
;; Package-Requires: ((helm "1.6.9"))

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
;; helm-bundle-show.el provides a helm interface to "bundle show".

;;; Code:

(require 'helm)

(defmacro helm-bundle-show--line-string ()
  `(buffer-substring-no-properties
    (line-beginning-position) (line-end-position)))

(defmacro helm-bundle-show--gem-string ()
  `(nth 1 (split-string (helm-bundle-show--line-string))))

(defun helm-bundle-show--list-candidates ()
  (with-temp-buffer
    (unless (zerop (call-process "bundle" nil t nil "show"))
      (error "Failed: bundle show'"))
    (let ((gems))
      (goto-char (point-min))
      (forward-line 1)
      (while (not (eobp))
        (let ((gem (helm-bundle-show--gem-string)))
          (push gem gems))
        (forward-line 1))
      (reverse gems))))

(defvar helm-bundle-show--action
  '(("Open Directory" . helm-bundle-show--find-file)
    ("Open Directory other window" . helm-bundle-show--find-file-other-window)
    ("Open Directory other frame" . helm-bundle-show--find-file-other-frame)))

(defun helm-bundle-show--find-file (gem)
  (find-file (helm-bundle-show--full-path gem)))

(defun helm-bundle-show--find-file-other-window (gem)
  (find-file-other-window (helm-bundle-show--full-path gem)))

(defun helm-bundle-show--find-file-other-frame (gem)
  (find-file-other-frame (helm-bundle-show--full-path gem)))

(defun helm-bundle-show--full-path (gem)
  (with-temp-buffer
    (unless (zerop (call-process "bundle" nil t nil "show" gem))
      (error (format "Failed: bundle show %s" gem)))
    (goto-char (point-min))
    (helm-bundle-show--line-string)))

(defvar helm-bundle-show--source
  `((name . "bundle show")
    (candidates . helm-bundle-show--list-candidates)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (action . ,helm-bundle-show--action))
  "Helm source for bundle show.")

;;;###autoload
(defun helm-bundle-show ()
  (interactive)
  (helm :sources helm-bundle-show--source
	:buffer "*helm-bundle-show*"))

(provide 'helm-bundle-show)

;;; helm-bundle-show.el ends here
