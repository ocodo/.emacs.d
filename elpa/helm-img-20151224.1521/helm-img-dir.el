;;; helm-img-dir --- Show local images on helm buffer

;; Description: Show local images on helm buffer
;; Author: Sho Matsumoto <l3msh0_at_gmail.com>
;; Maintainer: l3msh0
;; Copyright (C) 2015 l3msh0 all rights reserved.
;; Created: :2015-12-20
;; Version: 0.0.1
;; Keywords: convenience
;; URL: https://github.com/l3msh0/helm-img

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

;;; Commentary

;; TODO 拡張子の指定をまともにする

;;; Code:
(require 'helm-img)

(defun helm-img-dir-make-candidates (dir)
  (directory-files (expand-file-name dir) t "\\.png\\|\\.PNG\\|\\.jpg\\|\\.JPG"))

(helm-img-define-source "dir" :candidates 'helm-img-dir-make-candidates)

;;;###autoload
(defun helm-img-dir (dir)
    (interactive "DDirectory: ")
    (let ((helm-img-query dir))
      (helm
       :sources helm-img-source-dir
       :buffer "*helm img-dir*")))

(provide 'helm-img-dir)
