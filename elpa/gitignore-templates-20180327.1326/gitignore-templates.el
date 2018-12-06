;;; gitignore-templates.el --- Access GitHub .gitignore templates  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/gitignore-templates.el
;; Keywords: tools
;; Package-Version: 20180327.1326
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; https://developer.github.com/v3/gitignore/

;;; Code:

(require 'json)
(require 'url)

(defvar url-http-end-of-headers)

(defvar gitignore-templates-names nil
  "List of names of available templates.")

(defvar gitignore-templates-alist nil
  "List of (name . content).")

(defun gitignore-templates--request (url)
  "Make a request for URL and return the response body."
  (with-current-buffer (url-retrieve-synchronously url)
    (set-buffer-multibyte t)
    (goto-char url-http-end-of-headers)
    (let ((json-array-type 'list))
      (json-read))))

(defun gitignore-templates-names ()
  "Return list of names of available templates."
  (unless gitignore-templates-names
    (setq gitignore-templates-names
          (gitignore-templates--request
           "https://api.github.com/gitignore/templates")))
  gitignore-templates-names)

(defun gitignore-templates (name)
  "Return .gitignore template for NAME."
  (unless (member name (gitignore-templates-names))
    (user-error "Invaild template name %s" name))
  (unless (assoc name gitignore-templates-alist)
    ;; -------------------------------------------------------------------------
    ;; https://developer.github.com/v3/#rate-limiting says "For unauthenticated
    ;; requests, the rate limit allows for up to 60 requests per hour." A
    ;; work-around is to download the file from the git repo, for example,
    ;; https://raw.githubusercontent.com/github/gitignore/master/Elisp.gitignore
    ;; -------------------------------------------------------------------------
    (let* ((response (gitignore-templates--request
                      (concat "https://api.github.com/gitignore/templates/"
                              name)))
           (content (cdr (assq 'source response))))
      (push (cons name content) gitignore-templates-alist)))
  (cdr (assoc name gitignore-templates-alist)))

;;;###autoload
(defun gitignore-templates-insert (name)
  "Insert .gitignore template for NAME."
  (interactive
   (list (completing-read ".gitignore template: "
                          (gitignore-templates-names)
                          nil t)))
  (insert (gitignore-templates name)))

;;;###autoload
(defun gitignore-templates-new-file (name &optional directory)
  "Create a .gitignore file for NAME in DIRECTORY.
With a prefix argument prompt for a directory to use.
If DIRECTORY is nil, use `default-directory'."
  (interactive
   (list (completing-read ".gitignore template: "
                          (gitignore-templates-names)
                          nil t)
         (if current-prefix-arg
             (read-directory-name "Creat .gitignore in directory: ")
           default-directory)))
  (let ((file (expand-file-name ".gitignore" directory)))
    (when (file-exists-p file)
      (user-error "Can't create '%s', because it already exists"
                  (abbreviate-file-name file)))
    (write-region (gitignore-templates name) nil file)))

(provide 'gitignore-templates)
;;; gitignore-templates.el ends here
