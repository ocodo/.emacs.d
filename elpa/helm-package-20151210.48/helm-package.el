;;; helm-package.el --- Listing ELPA packages with helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-package
;; Package-Version: 20151210.48
;; Version: 0.03
;; Package-Requires: ((helm "1.7.7") (cl-lib "0.5"))

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

;; Show available packages and installed packages with helm interface

;;; Code:

(require 'cl-lib)

(require 'helm)
(require 'package)

(defun helm-package--extract-package-name (package-info)
  (cl-typecase package-info
    (vector (replace-regexp-in-string
             "\\s-*\\[source:[^\]]+\\]\\s-*" "" (aref package-info 2)))
    ;; Emacs 24.4 or higher uses `package-desc' for package information
    (cons (package-desc-summary (car package-info)))))

(defun helm-package--collect-packages (pred)
  (let ((copyed (copy-sequence package-archive-contents)))
    (cl-loop with sorted = (sort copyed (lambda (a b)
                                          (string< (car a) (car b))))
             for (package . package-info) in sorted
             for package-name = (symbol-name package)
             for candidate = (if (> (length package-name) 30)
                                 (concat (substring package-name 0 27) "...")
                               package-name)
             for desc = (helm-package--extract-package-name package-info)
             when (funcall pred package)
             collect
             (cons (format "%-30s| %s"
                           candidate
                           (truncate-string-to-width desc (- (frame-width) 32)))
                   package))))

(defun helm-package--install (_candidate)
  (cl-loop for package in (helm-marked-candidates)
           do
           (package-install (intern package))))

(defun helm-package--initialize ()
  (unless package--initialized
    (package-initialize t)))

(defun helm-package--persistent-show-detail (package)
  (with-help-window (help-buffer)
    (princ (describe-package package))))

(defvar helm-package--available-source
  (helm-build-sync-source "Available Packages"
    :init #'helm-package--initialize
    :candidates (lambda ()
                  (helm-package--collect-packages 'identity))
    :candidate-number-limit 9999
    :persistent-action #'helm-package--persistent-show-detail
    :action #'helm-package--install))

(defvar helm-package--installed-source
  (helm-build-sync-source "Installed Packages"
    :init #'helm-package--initialize
    :candidates (lambda ()
                  (helm-package--collect-packages 'package-installed-p))
    :candidate-number-limit 9999
    :persistent-action #'helm-package--persistent-show-detail
    :action #'helm-package--install))

;;;###autoload
(defun helm-package ()
  (interactive)
  (when current-prefix-arg
    (package-refresh-contents))
  (let ((buf (get-buffer-create "*helm-package*")))
    (helm :sources '(helm-package--available-source helm-package--installed-source)
          :buffer buf)))

(provide 'helm-package)

;;; helm-package.el ends here
