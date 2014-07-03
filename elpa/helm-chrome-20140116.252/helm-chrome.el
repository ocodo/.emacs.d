;;; helm-chrome.el --- Helm interface for Chrome bookmarks -*- lexical-binding: t -*-

;; Filename: helm-chrome.el
;; Description: Helm interface for Chrome bookmarks
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2013-12-25
;; Version: 20140116.252
;; X-Original-Version: 1.131226
;; Package-Requires: ((helm "1.0") (cl-lib "0.3") (emacs "24"))
;; Keywords: tools
;; Human-Keywords: chrome bookmarks
;; URL: https://github.com/kawabata/helm-chrome

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

;; Helm interface for Chrome bookmarks.
;;
;; Warning: Multiple bookmarks with the same name will be overridden.
;; This restriction is for better performance.  If we use Bookmark IDs with
;; candidate-transformer, then the speed would be quite slow.

;;; Code:

(require 'helm)
(require 'cl-lib)
(require 'json)

(defgroup helm-chrome nil
  "Helm interface for Chrome Bookmarks."
  :group 'helm)

(defcustom helm-chrome-file
  (car
   (cl-delete-if-not
    'file-exists-p
    '("~/Library/Application Support/Google/Chrome/Default/Bookmarks"
      "~/AppData/Local/Google/Chrome/User Data/Default/Bookmarks"
      "~/.config/chromium/Default/Bookmarks"
      "~/.config/google-chrome/Default/Bookmarks")))
  "The bookmark file for Chrome."
  :group 'helm-chrome
  :type 'file)

(defvar helm-chrome--json nil)
(defvar helm-chrome--bookmarks nil)
(defconst helm-chrome--buffer "*Chrome Bookmarks*")

(defun helm-chrome--add-bookmark (json)
  "Add bookmarks from JSON."
  (when (and (listp json) (listp (cdr json)))
    (cond
     ((assoc 'roots json)
      (dolist (item (cdr (assoc 'roots json)))
        (helm-chrome--add-bookmark item)))
     ((equal (cdr (assoc 'type json)) "folder")
      (cl-loop for item across (cdr (assoc 'children json))
               do (helm-chrome--add-bookmark item)))
     ((equal (cdr (assoc 'type json)) "url")
      (puthash (cdr (assoc 'name json)) (cdr (assoc 'url json))
               helm-chrome--bookmarks)))))

(defun helm-chrome-reload-bookmarks ()
  "Reload Chrome bookmarks."
  (interactive)
  (unless (file-exists-p helm-chrome-file)
    (error "File %s does not exist" helm-chrome-file))
  (setq helm-chrome--json (with-temp-buffer
                           (insert-file-contents helm-chrome-file)
                           (json-read)))
  (setq helm-chrome--bookmarks (make-hash-table :test 'equal))
  (helm-chrome--add-bookmark helm-chrome--json)
  (let ((buffer (get-buffer helm-chrome--buffer)))
    (when buffer (kill-buffer buffer))))

(defun helm-chrome--init ()
  "Initialize an helm buffer with Chrome bookmarks."
  (when (null helm-chrome--json)
    (helm-chrome-reload-bookmarks))
  (with-current-buffer (helm-candidate-buffer
                        (get-buffer-create helm-chrome--buffer))
    (cl-loop for name being the hash-keys of helm-chrome--bookmarks
             do (insert name "\n"))))

(defvar helm-chrome-source
  `((name . "Chrome::Bookmarks")
    (init . helm-chrome--init)
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action
     ("Browse URL" . (lambda (candidate)
                       (browse-url (gethash candidate helm-chrome--bookmarks))))
     ("Show URL" . (lambda (candidate)
                     (message (gethash candidate helm-chrome--bookmarks)))))))

;;;###autoload
(defun helm-chrome-bookmarks ()
  "Search Chrome Bookmark using `helm'."
  (interactive)
  (helm '(helm-chrome-source) nil "Find Bookmark: " nil nil))

(provide 'helm-chrome)

;;; helm-chrome.el ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:
