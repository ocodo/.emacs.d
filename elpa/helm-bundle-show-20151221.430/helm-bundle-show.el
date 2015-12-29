;;; helm-bundle-show.el --- bundle show with helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2015 by Takashi Masuda

;; Author: Takashi Masuda <masutaka.net@gmail.com>
;; URL: https://github.com/masutaka/emacs-helm-bundle-show
;; Package-Version: 20151221.430
;; Version: 1.1.5
;; Package-Requires: ((helm "1.8.0"))

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
(require 'helm-files)

(defgroup helm-bundle-show nil
  "bundle show with helm interface"
  :prefix "helm-bundle-show-"
  :group 'helm)

(defcustom helm-bundle-show-command-bundle
  "bundle"
  "*A bundle command"
  :type 'string
  :group 'helm-bundle-show)

(defmacro helm-bundle-show--line-string ()
  `(buffer-substring-no-properties
    (line-beginning-position) (line-end-position)))

(defmacro helm-bundle-show--gem-string ()
  `(nth 1 (split-string (helm-bundle-show--line-string))))

(defun helm-bundle-show--list-candidates ()
  (with-temp-buffer
    (unless (zerop (apply #'call-process
			  helm-bundle-show-command-bundle nil t nil
			  (list "show")))
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
    ("Open Directory other frame" . helm-bundle-show--find-file-other-frame)
    ("Browse RubyGems url" . helm-bundle-show--browse-rubygems-url)
    ("Copy RubyGems url" . helm-bundle-show--copy-rubygems-url)))

(defun helm-bundle-show--find-file (gem)
  (find-file (helm-bundle-show--full-path gem)))

(defun helm-bundle-show--find-file-other-window (gem)
  (find-file-other-window (helm-bundle-show--full-path gem)))

(defun helm-bundle-show--find-file-other-frame (gem)
  (find-file-other-frame (helm-bundle-show--full-path gem)))

(defun helm-bundle-show--browse-rubygems-url (gem)
  (browse-url (helm-bundle-show--rubygems-url gem)))

(defun helm-bundle-show--copy-rubygems-url (gem)
  (let ((url (helm-bundle-show--rubygems-url gem)))
    (kill-new url)
    (message url)))

(defun helm-bundle-show--full-path (gem)
  (with-temp-buffer
    (unless (zerop (apply #'call-process
			  helm-bundle-show-command-bundle nil t nil
			  (list "show" gem)))
      (error (format "Failed: bundle show %s" gem)))
    (goto-char (point-min))
    (helm-bundle-show--line-string)))

(defun helm-bundle-show--rubygems-url (gem)
  (concat "https://rubygems.org/gems/" gem))

(defvar helm-bundle-show--source
  `((name . "bundle show")
    (candidates . helm-bundle-show--list-candidates)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (action . ,helm-bundle-show--action))
  "Helm source for bundle show.")

;;;###autoload
(defun helm-bundle-show ()
  (interactive)
  (helm :sources helm-bundle-show--source
	:buffer "*helm-bundle-show*"))

(provide 'helm-bundle-show)

;;; helm-bundle-show.el ends here
