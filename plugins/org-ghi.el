;; org-ghi.el --- Provides GitHub integration for Org-mode. 
     
;; Author: Puneeth Chaganti <punchagan@muse-amuse.in>
;; Created: 25 May 2011
;; Keywords: github orgmode

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;; A portion of the code in this file is based on github.el available
;; at https://github.com/abhiyerra/github.el/blob/master/github.el
;; copyrighted by Abhi Yerra and gist.el available at
;; https://github.com/defunkt/gist.el.git copyrighted by Christian
;; Neukirchen.

;;; Commentary:

;; Provides integration for GitHub Issues for Org-mode.  

;; To use this code add (require 'org-ghi) to your .emacs
;; `org-ghi-fetch-all' is a possible entry point to start using this
;; module

;;; Code:

;; (eval-when-compile (require 'cl))
(require 'json)

(defvar github-user nil
  "If non-nil, will be used as your GitHub username without checking
git-config(1).")

(defvar github-password nil
  "If non-nil, will be used as your GitHub token without checking
git-config(1).")

(defun github-get-auth-info ()
  (unless github-user
    (setq github-user (or (github-config "user") 
                          (read-string "GitHub username: "))))
  (unless github-password
    (setq github-password (or (github-config "password") 
                          (read-passwd "GitHub password: ")))))


(defun github-request (method url &optional params)
  "Makes a request to `url' synchronously, notifying `callback'
when complete. The github parameters are included in the
request. Optionally accepts additional POST `params' as a list
of (key . value) conses."
  (github-get-auth-info)
  (let ((url-request-data params)
        (url-request-extra-headers
         `(("Content-Type" . "application/x-www-form-urlencoded")
           ("Authorization" . ,(concat "Basic "
                                       (base64-encode-string
                                        (concat github-user ":" 
                                                github-password))))))
          (url-max-redirecton -1)
          (url-request-method method))
      (url-retrieve-synchronously url)))

(defun org-ghi-make-query-string (params)
  "Returns a query string constructed from PARAMS, which should be
a list with elements of the form (KEY . VALUE). KEY and VALUE
should both be strings."
  (mapconcat
   (lambda (param)
     (concat (url-hexify-string (car param)) "="
             (url-hexify-string (cdr param))))
   params "&"))

(defun github-config (key)
  "Returns a GitHub specific value from the global Git config."
  (let ((strip (lambda (string)
                 (if (> (length string) 0)
                     (substring string 0 (- (length string) 1)))))
        (git (executable-find "git")))
  (funcall strip (shell-command-to-string
                  (concat git " config --global github." key)))))

(defun github-set-config (key value)
  "Sets a GitHub specific value to the global Git config."
  (let ((git (executable-find "git")))
    (shell-command-to-string
     (format "%s config --global github.%s %s" git key value))))

(defvar org-ghi-org-file "github.org"
  "File to use for saving issues in")

(defvar org-ghi-file-under-repo-headline t
  "Use repo headline as heading, and file issues under it")

(defvar org-ghi-todo-keyword
  "TODO"
  "Default todo keyword to use for issues.")

(defvar org-ghi-done-keyword
  "DONE"
  "Default done keyword to use for issues.")

(defvar org-ghi-manage-comments nil
  "Should we manage comments, too?")

(defvar org-ghi-comment-level 2
  "Headline level for comments")

(defvar org-ghi-issue-level 1
  "Headline level for issues")

(defvar org-ghi-interesting-repos '()
  "Repos you are interested in; used only for autocompletion, now.")

(defun org-ghi-list (repo &optional status)
  "Displays a list of all of the current user's gists in a new buffer."
  (message "Retrieving list of issues for %s..." repo)
  (with-current-buffer
   (github-request
    "GET" (format "https://api.github.com/repos/%s/issues"
            repo)
    (org-ghi-make-query-string (list (cons "status" status))))
   (org-ghi-lists-retrieved-callback))
  (message "Finished retrieving & updating list of issues for %s..." repo))

(defun org-ghi-lists-retrieved-callback ()
  "Called when the list of issues has been retrieved. Parses the
result and updates the TODO list."
  (let ((issues (org-ghi-get-json-as-list))
        (p (if (file-name-absolute-p org-ghi-org-file)
               org-ghi-org-file
             (expand-file-name org-ghi-org-file org-directory))))
    (when issues
      (with-current-buffer (or (find-buffer-visiting p)
                               (find-file-noselect p))
        (save-excursion
          (save-restriction
            (org-mode)
            (org-content)
            (mapc 'org-ghi-insert-or-update-issue issues)))))))

(defun org-ghi-get-json-as-list ()
  "Get the json from the request buffer as a list."
  (goto-char (point-min))
  (when (search-forward "{" nil t)
    (forward-char -2)
    (let* ((json-list
            (let ((json-array-type 'vector)
                  (json-key-type 'string)) 
              (json-read))))
      json-list)))

(defun org-ghi-insert-or-update-issue (issue)
  "Reads an issue in the form of alist and writes it to org-format"
  (let* ((title (cdr (assoc "title" issue)))
         (url (cdr (assoc "html_url" issue)))
         (marker (org-find-exact-headline-in-buffer 
                  (format "[[%s][%s]]" url title))))
    (if marker
        (org-ghi-update-current-issue issue marker)
      (org-ghi-insert-issue issue))))


(defun org-ghi-fetch-issue (repo number)
  "Fetch the issue with the given number"
  (message "Retrieving issue %s of %s..." number repo)
  (with-current-buffer
      (github-request "GET" (format 
                             "https://api.github.com/repos/%s/issues/%s"
                             repo number))
    (org-ghi-get-json-as-list)))

(defun org-ghi-issue-reduced-alist (issue)
  "Return issue's reduced alist with only relevant keys."
  (let ((assignee (cons "assignee" 
                            (or (cdr (assoc "login" 
                                        (cdr (assoc "assignee" issue)))) "")))
        (due_on (cons "due_on" 
                            (cdr (assoc "due_on" 
                                        (cdr (assoc "milestone" issue))))))
        (tags (cons "labels" 
                            (mapcar (lambda (tag)
                                      (cdr (assoc "name" tag)))
                                    (cdr (assoc "labels" issue))))))
    (dolist (item issue)
      (unless (member (car item) (list "body" "html_url" "title" "number" 
                                       "state" "created_at" "updated_at"))
      (setq issue (remove item issue))))
  (add-to-list 'issue assignee)
  (add-to-list 'issue due_on)
  (add-to-list 'issue tags)
  issue))

(defun org-ghi-at-issue-p (&optional pos)
  "Returns whether POS is at a issue."
  (save-excursion
    (org-back-to-heading)
    (if (org-entry-get (point) "REPO") t nil)))

(defun org-ghi-insert-issue (issue)
  "Inserts a new issue, at location based on file-under variables."
    (org-mode)
    (org-content)
    (when org-ghi-file-under-repo-headline
      (org-ghi-goto-repo repo))
    (goto-char (point-max))
    (org-ghi-insert-issue-at-point issue))

(defun org-ghi-get-tag-names (labels)
  "Gets the names of the labels."
  (mapcar (lambda (label) (cdr (assoc "name" label))) labels))

(defun org-ghi-insert-issue-at-point (issue)
  "Reads an issue in the form of alist and writes it to org-format"
  (let* ((issue (org-ghi-issue-reduced-alist issue))
         (title (cdr (assoc "title" issue)))
         (url (cdr (assoc "html_url" issue)))
         (repo (org-ghi-get-repo url))
         (tags (cdr (assoc "labels" issue)))
;;         (body (cdr (assoc "body" issue)))
         (state (cdr (assoc "state" issue)))
         (due_on (cdr (assoc "due_on" issue)))
         (level org-ghi-issue-level))
    (org-ghi-insert-newline-maybe)
    (insert (format "* %s [[%s][%s]]" 
                    (org-ghi-select-keyword state)
                    url title) "\n")
    (org-set-tags-to tags)
;;    (insert (org-ghi-insert-body body))
    (dotimes (n (- level (org-current-level)) nil) (org-do-demote))
    (mapc 'org-ghi-set-property issue)
    (org-set-property "REPO" repo)
    (if due_on (org-deadline nil (org-ghi-get-time due_on)))
    (when org-ghi-manage-comments
      (org-ghi-comment-list issue))))

(defun org-ghi-entry-to-alist (&optional point)
  "Converts an entry to an alist."
  (interactive)
  (org-ghi-update-issue-state)
  (let* ((props (org-entry-properties nil 'standard))
         (heading (nth 4 (org-heading-components)))
         (tags (org-get-tags-at nil t))
         (due_on (org-get-deadline-time (point)))
         (due_on (if due_on (format-time-string "%Y-%m-%dT07:00:00Z" due_on)
                   nil))
        ;;  (body 
        ;;  (save-excursion
        ;;    (org-back-to-heading)
        ;;    (save-match-data 
        ;;      (re-search-forward
        ;;       "^#\\+begin_comment[ \t]*\n\\([^\000]*?\\)\n#\\+end_comment\\>.*"
        ;;       nil t)
        ;;      (match-string-no-properties 1))))
        ;; (body (org-ghi-parse-body body))
        j-alist title url)
    (setq j-alist (mapcar 'org-ghi-property-to-key-val props))
    (delete nil j-alist)
    (when (org-ghi-at-issue-p)
      (with-temp-buffer
        (save-match-data
          (insert heading)
          (goto-char (point-min))
          (if (re-search-forward org-any-link-re nil t)
              (progn
                (setq title (match-string-no-properties 4))
                (setq url (match-string-no-properties 2)))
            (setq title heading)
            (setq url ""))))
      (add-to-list 'j-alist (cons "title" title))
      (add-to-list 'j-alist (cons "html_url" url))
      (add-to-list 'j-alist (cons "labels" tags))
      (add-to-list 'j-alist (cons "due_on" due_on))
;;      (add-to-list 'j-alist (cons "body" body))
      )
    (delete nil j-alist)
    j-alist))


(defun org-ghi-compare-alists (alist-1 alist-2)
  "Compares two alists and returns t only when they are equal."
  (let* ((fl-p (< (length alist-1) (length alist-2)))
         (l1 (if fl-p alist-2 alist-1))
         (l2 (if fl-p alist-1 alist-2))
         equal)
    (dolist (p l1)
      (unless (member p l2)
        (add-to-list 'equal (car p))))
    equal))

(defun org-ghi-property-to-key-val (property)
  "Converts a propery cons cell into a json type cons cell."
  (let ((key (car property))
        (val (cdr property)))
    (when (string= "GH_" (substring key 0 3))
      (setq key (substring key 3))
      (if (member key
                  (list "NUMBER" "ID" "COMMENTS"))
          (setq val (string-to-number val)))
      (cons (downcase key) val))))

(defun org-ghi-parse-body (body)
  "Inserts the body of an issue or comment inside a blockquote."
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (delete-matching-lines "^#\\+[[:alpha:]]*_comment")
    (dotimes (n (count-lines (point-min) (point-max)) nil)
      (beginning-of-line)
      (delete-char 2)
      (forward-line 1))
    (org-no-properties (buffer-string))))

(defun org-ghi-insert-body (body)
  "Inserts the body of an issue or comment inside a blockquote."
  (with-temp-buffer
    (insert (or body ""))
    (goto-char (point-min))
    (dotimes (n (count-lines (point-min) (point-max)) nil)
      (beginning-of-line)
      (insert "  ")
      (forward-line 1))
    (format "\n%s \n%s\n%s \n\n" 
            "#+begin_comment"
            (buffer-string)
            "#+end_comment")))

(defun org-ghi-select-keyword (state)
  "Returns the correct TODO keyword, depending on the state"
  (if (string= state "open") 
      (or org-ghi-todo-keyword "TODO")
    (or org-ghi-done-keyword "DONE")))

(defun org-ghi-update-current-issue (&optional remote pos)
  "Updates the issue at POS."
  (interactive)
  (save-excursion
    (when pos
      (goto-char pos))
    (let* ((repo (org-entry-get (point) "REPO"))
           (number (org-entry-get (point) "GH_NUMBER"))
           (remote (if (not remote)
                       (org-ghi-fetch-issue repo number)
                     remote))
           (local (org-ghi-entry-to-alist))
           (changes (org-ghi-compare-alists 
                     (org-ghi-issue-reduced-alist remote) local)))
      (if (not changes)
          (message "No updates to the Issue")
        (if (time-less-p
             (org-ghi-get-time (cdr (assoc "updated_at" local)))
             (org-ghi-get-time (cdr (assoc "updated_at" remote))))
            (if (y-or-n-p (format "The following items changed remotely -- %s. Do you want to Update? Warning: Local changes will be lost." changes))
                (org-ghi-replace-issue remote)
              (message "Remote changes present. Not updated locally!"))
          (org-ghi-replace-issue 
           (with-current-buffer (github-request 
                                 "PATCH" 
                                 (format 
                                  "https://api.github.com/repos/%s/issues/%s"
                                  repo number)
                                 (let ((json-array-type 'vector)
                                       (json-key-type 'string))
                                   (json-encode local)))
             (when (org-ghi-request-status-success-p)
                 (error "ERROR -- %s. Can't Update" 
                        (org-ghi-request-status-success-p)))
             (org-ghi-get-json-as-list)))
          (message "Updates pushed!"))))))

(defun org-ghi-request-status-success-p ()
  (goto-char (point-min))
  (let ((fl (buffer-substring (line-beginning-position) (line-end-position))))
    (and (not (or (equal "HTTP/1.1 200 OK" fl) 
                  (equal "HTTP/1.1 201 Created" fl)))
         fl)))

(defun org-ghi-get-time (time-string)
  "Return parsed time list from timezone string."
  (apply 'encode-time
         (parse-time-string (timezone-make-date-arpa-standard time-string))))

(defun org-ghi-update-issue-state (&optional pos)
  "Update the GH_STATE property of the headline based on TODO state."
  (save-excursion
    (when pos
      (goto-char pos))
    (org-set-property "GH_STATE" (if (org-entry-is-done-p) "closed" "open"))))

(defun org-ghi-replace-issue (issue &optional pos)
  "Reads an issue in the form of alist and writes it to org-format"
  (save-excursion
    (when pos
      (goto-char pos))
    (delete-region (save-excursion (org-back-to-heading) (point))
                   (save-excursion (outline-next-heading) (1- (point))))
    (org-ghi-insert-issue-at-point issue)))

(defun org-ghi-set-property (kvp)
  "Save a key value pair as an org-property."
  (let* ((key (car kvp))
         (value (if (numberp (cdr kvp)) 
                    (number-to-string (cdr kvp)) 
                  (cdr kvp))))
    (when
        (member key '("state" "updated_at" "created_at" "number" "assignee"))
       (org-set-property (format "GH_%s" (upcase key)) (or value "")))))

(defun org-ghi-get-repo (url)
  "Get the repo given an issue url"
  (let* ((url-split (split-string url "/"))
         (repo (format "%s/%s" (nth 3 url-split) (nth 4 url-split))))
    repo))

(defun org-ghi-goto-repo (repo)
  "Find the repo headline and go there. OR create one."
  (let ((pos (org-find-exact-headline-in-buffer repo)))
    (if pos
        (goto-char pos)
      (goto-char (point-max))
      (org-ghi-insert-newline-maybe)
      (insert "* " repo "\n"))
    (end-of-line)
    (org-narrow-to-subtree)
    (setq org-ghi-issue-level (1+ (org-current-level)))
    (setq org-ghi-comment-level (1+ org-ghi-issue-level))))

(defun org-ghi-insert-newline-maybe ()
  "Insert a new line if not on blank line"
  (beginning-of-line)
  (unless (looking-at "^[ \t\r]*\n\\|$")
    (end-of-line)
    (insert "\n")))

(defun org-ghi-make-todo-an-issue ()
  "Pushes a TODO item as an issue. Should have a property REPO."
  (interactive)
  (let ((issue (org-ghi-entry-to-alist))
        (repo (org-entry-get (point) "REPO"))
        (number (org-entry-get (point) "GH_NUMBER")))
    (when number
      (error "Issue already has a GH_NUMBER. Does it already exist?"))
    (org-ghi-replace-issue 
     (with-current-buffer (github-request 
                         "POST" 
                         (format 
                          "https://api.github.com/repos/%s/issues"
                          repo)
                         (let ((json-array-type 'vector)
                               (json-key-type 'string))
                           (json-encode issue)))
       (when (org-ghi-request-status-success-p)
         (error "ERROR -- %s. Can't Update" 
                (org-ghi-request-status-success-p)))
       (org-ghi-get-json-as-list)))))

(defun org-ghi-sync-all (&optional repo)
  "Update all issues, irrespective of status."
  (interactive)
  (unless repo
    (setq repo (completing-read "Repository: "
                                org-ghi-interesting-repos
                                nil nil
                                (concat github-user "/"))))
  (org-ghi-list repo "open")
  (org-ghi-list repo "closed")
  (save-buffer)
  (message "Sync-ed all issues for %s" repo))

(provide 'org-ghi)
