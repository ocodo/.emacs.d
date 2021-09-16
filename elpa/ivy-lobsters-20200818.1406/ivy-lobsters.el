;;; ivy-lobsters.el --- Browse lobste.rs stories with ivy.

;; Copyright (C) 2020 by Julien Blanchard
;; Author: Julien Blanchard <https://github.com/julienXX>
;; URL: https://github.com/julienXX/ivy-lobsters
;; Package-Version: 20200818.1406
;; Package-Commit: 3f7f90751d15ebcf91253ef3cda18c0aa7d856ff
;; Package: ivy-lobsters
;; Package-Requires: ((ivy "0.8.0") (cl-lib "0.5"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;; Makes it easier to browse lobste.rs from Emacs.

;;; Code:

(require 'cl-lib)
(require 'ivy)
(require 'browse-url)
(require 'json)

(defgroup  ivy-lobsters nil
  "Customs for `ivy-lobsters'"
  :group 'applications)

(defcustom ivy-lobsters-url "https://lobste.rs/newest.json"
  "Variable to define lobste.rs newest articles url."
  :type 'string :group 'ivy-lobsters)

(defcustom ivy-lobsters-keep-focus nil
  "Variable to control if the browser takes focus from Emacs.  Currently only work for macOS."
  :type 'boolean :group 'ivy-lobsters)

(defun ivy-lobsters-get-posts ()
  "Get newest posts json and store parsed stories."
  (with-temp-buffer
    (unless (zerop (call-process "curl" nil t nil "-s" ivy-lobsters-url))
      (error "Failed: 'curl -s %s'" ivy-lobsters-url))
    (let* ((json nil)
           (ret (ignore-errors
                  (setq json (json-read-from-string
                              (buffer-substring-no-properties
                               (point-min) (point-max))))
                  t)))
      (unless ret
        (error "Error: Can't get JSON response"))
      json)))

(defun ivy-lobsters-parse (stories)
  "Parse the json provided by STORIES."
  (cl-loop for story being the elements of stories
           for score = (ivy-lobsters-extract 'score story)
           for title = (ivy-lobsters-format-title story)
           for url = (ivy-lobsters-extract 'url story)
           for comment-count = (ivy-lobsters-extract 'comment_count story)
           for comments-url = (ivy-lobsters-extract 'comments_url story)
           for cand = (format "%s %s (%s comments)"
                              (format "[%d]" score)
                              title
                              comment-count)
           collect (cons cand (list :url url :score score :comments-url comments-url))))

(defun ivy-lobsters-tree-assoc (key tree)
  "Build the tree-assoc from KEY TREE."
  (when (consp tree)
    (cl-destructuring-bind (x . y)  tree
      (if (eql x key) tree
        (or (ivy-lobsters-tree-assoc key x) (ivy-lobsters-tree-assoc key y))))))

(defun ivy-lobsters-extract (key story)
  "Extract KEY from STORY tree-assoc."
  (cdr (ivy-lobsters-tree-assoc key story)))

(defun ivy-lobsters-format-title (story)
  "Format STORY title."
  (decode-coding-string
   (string-make-multibyte
    (ivy-lobsters-extract 'title story))
   'utf-8))

(defun ivy-lobsters-browse (url)
  "Optionally keep focus in Emacs when opening a link.  Takes the URL as an arguement."
  (if (and ivy-lobsters-keep-focus (eq system-type 'darwin))
      (start-process (concat "ivy-lobsters-" url) nil "open" url "-g")
    (browse-url url)))

;;;###autoload
(defun ivy-lobsters ()
  "Show latest lobste.rs stories."
  (interactive)
  (with-temp-message "Loading stories..."
    (let ((stories (ivy-lobsters-parse (ivy-lobsters-get-posts))))
      (ivy-read "Lobste.rs latest stories: "
                stories
                :action (lambda (story)
                  (ivy-lobsters-browse (plist-get (cdr story) :url)))))))

(ivy-set-actions
 'ivy-lobsters
 '(("c" (lambda (story)
          (ivy-lobsters-browse (plist-get (cdr story) :comments-url))) "Browse Comments")))


(provide 'ivy-lobsters)
;;; ivy-lobsters.el ends here
