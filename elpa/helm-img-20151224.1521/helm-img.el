;;; helm-img.el --- Utilities for making image sources for helm.

;; Description: Utilities for making image sources for helm.
;; Author: Sho Matsumoto <l3msh0_at_gmail.com>
;; Maintainer: l3msh0
;; Copyright (C) 2015 l3msh0 all rights reserved.
;; Created: :2015-12-20
;; Version: 0.0.1
;; Keywords: convenience
;; URL: https://github.com/l3msh0/helm-img
;; Package-Requires: ((helm "1.7.7") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; TODO initializeとactionに任意の値を指定できるように
;; TODO 画像の取得をdeferredで非同期化

;;; Code

(require 'helm)
(require 'json)
(require 'cl-macs)

(defgroup helm-img nil
  "Utilities for making image sources for helm."
  :group 'helm
  :group 'image)

(defcustom helm-img-thumbnail-height 100
  "Thumbnail height"
  :type 'integer
  :group 'helm-img)

(defvar helm-img-query nil)

(defun helm-img-url-p (path)
  (if (string-match "^https*://" path)
      t
    nil))

(defun helm-img-extract-body (response-buffer)
  "Extract body from HTTP response buffer."
  (with-current-buffer response-buffer
    (goto-char (point-min))
    (search-forward "\n\n")
    (buffer-substring-no-properties (point) (point-max))))

(defun helm-img-create-image-from-url (url)
  (let* ((response (url-retrieve-synchronously url))
         (body (helm-img-extract-body response)))
    (create-image body 'imagemagick t :height helm-img-thumbnail-height)))

(defun helm-img-create-image-from-file (path)
  (create-image path 'imagemagick nil :height helm-img-thumbnail-height))

(defun helm-img-create-image (path)
  "Create image object from URL or path."
  (if (helm-img-url-p path)
      (helm-img-create-image-from-url path)
    (helm-img-create-image-from-file path)))

(defun helm-img-make-string-with-image (image)
  "Create string with image object."
  (with-temp-buffer
    (insert-image image)
    (buffer-substring (point-min) (point-max))))

(cl-defmacro helm-img-define-source (name &key candidates)
  `(set (intern (concat "helm-img-source-" ,name))
        (helm-build-sync-source ,name
          :candidates (lambda () (mapcar (lambda (x)
                                           (let ((thumb (if (listp x) (cdr (assoc 'thumb x)) x))
                                                 (path (if (listp x) (cdr (assoc 'full x)) x)))
                                             (cons
                                              (helm-img-make-string-with-image (helm-img-create-image thumb))
                                              path)))
                                         (funcall (symbol-function ,candidates) helm-img-query)))
          :volatile t
          :action '(("Kill URL" . (lambda (url)
                                    (let ((x-select-enable-clipboard t))
                                      (kill-new url))
                                    (message url)))
                    ("Browse URL" . (lambda (url) (browse-url url)))))))

(provide 'helm-img)
;;; helm-img.el ends here
