;;; helm-img-bing --- Search images via Bing Search API and show results on helm buffer

;; Description: Search images via Bing Search API and show results on helm buffer
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

(require 'helm-img)
(require 'url-util)
(require 'cl-extra)

(defcustom helm-img-bing-query-url "https://api.datamarket.azure.com/Bing/Search/v1/Composite?Sources=%27image%27&Market=%27ja-JP%27&Adult=%27Moderate%27&ImageFilters=%27Size%3AMedium%27&$format=json" "Image Search URL" :group 'helm-img)

(defcustom helm-img-bing-account-key "" "Microsoft account key." :group 'helm-img)
(defcustom helm-img-bing-candidate-limit 10 "Maximum candidates." :group 'helm-img)

(defun helm-img-bing-search (query)
  (let* ((url-request-extra-headers
          `(("Authorization" . ,(concat "Basic " (base64-encode-string (concat ":" helm-img-bing-account-key))))))
         (result-buffer (url-retrieve-synchronously (concat helm-img-bing-query-url "&Query=" (url-hexify-string (concat "'" query "'")))))
         (body (helm-img-extract-body result-buffer)))
    (json-read-from-string body)))

(defun helm-img-bing-make-url (img-info size)
  (cond ((eq 'thumb size)
         (cdr (assoc 'MediaUrl (cdr (assoc 'Thumbnail img-info)))))
        ((eq 'full size)
         (cdr (assoc 'MediaUrl img-info)))))

(defun helm-img-bing-make-candidates (query)
  (let ((results (helm-img-bing-search query)))
    (cl-subseq (mapcar (lambda (img-info)
                         (list
                          (cons 'thumb (helm-img-bing-make-url img-info 'thumb))
                          (cons 'full (helm-img-bing-make-url img-info 'full))))
                       (append (cdr (assoc 'Image (car (append (cdr (assoc 'results (cdr (assoc 'd results)))) nil)))) nil)) 0 helm-img-bing-candidate-limit)))

(helm-img-define-source "bing" :candidates 'helm-img-bing-make-candidates)

;;;###autoload
(defun helm-img-bing (query)
  (interactive "Mquery: ")
  (let ((helm-img-query query))
    (helm
     :sources helm-img-source-bing
     :buffer "*helm-img-bing*")))

(provide 'helm-img-bing)
