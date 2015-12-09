;;; pivotal-tracker.el --- Interact with Pivotal Tracker through its API

;; Author: John Andrews
;; URL: http://github.com/jxa/pivotal-tracker
;; Package-Version: 20151203.1150
;; Created: 2010.11.14
;; Version: 1.3.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2, or any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:

;; Pivotal Tracker Mode provides a mode and functions for interacting
;; with Pivotal Tracker through its API.
;; It is designed to give most of the functionality that is important to a developer.

;; Before using the tracker you must customize your pivotal API key.
;; You can obtain the key from the 'My Profile' link in the Pivotal Tracker
;; web application.
;; M-x customize-group RET pivotal RET

;;; Code:

(require 'cl)
(require 'xml)
(require 'url)
(require 'json)
(require 'magit-popup)

;;;###autoload
(progn
  (defgroup pivotal nil
    "Pivotal Tracker"
    :group 'external)

  (defcustom pivotal-api-token ""
    "API key found on the /profile page of pivotal tracker"
    :group 'pivotal
    :type 'string))

(defconst pivotal-base-url "https://www.pivotaltracker.com/services/v3"
  "format string to use when creating endpoint urls")

(defconst pivotal-states `("unstarted" "started" "finished" "delivered" "accepted" "rejected")
  "story status will be one of these values")

(defconst pivotal-current-iteration-number -1)

(defvar *pivotal-current-project*)
(defvar *pivotal-iteration* pivotal-current-iteration-number)

;;;;;;;; INTERACTIVE USER FUNS

;;;###autoload
(defun pivotal ()
  "launch pivotal-projects window, or just switch to it"
  (interactive)
  (let ((buffer (get-buffer "*pivotal-projects*")))
    (if buffer
        (switch-to-buffer buffer)
      (pivotal-get-projects))))

;;;###autoload
(defun pivotal-get-projects ()
  "show a buffer of all projects you have access to"
  (interactive)
  (assert-pivotal-api-token)
  (pivotal-api (pivotal-url "projects") "GET" 'pivotal-projects-callback))

(defun pivotal-get-current ()
  "show a buffer of all stories in the currently selected iteration"
  (interactive)
  (pivotal-get-iteration *pivotal-iteration*))

(defun pivotal-get-iteration (iteration)
  (let ((query-string (if (= pivotal-current-iteration-number iteration)
                          "iterations/current"
                        (format "iterations/backlog?offset=%s&limit=1" iteration))))

    (assert-pivotal-api-token)
    (pivotal-api (pivotal-url "projects" *pivotal-current-project* query-string)
                 "GET"
                 'pivotal-iteration-callback)))

(defun pivotal-next-iteration ()
  "replace iteration view with the next upcoming iteration"
  (interactive)
  (setq *pivotal-iteration* (+ 1 *pivotal-iteration*))
  (pivotal-get-iteration *pivotal-iteration*))

(defun pivotal-previous-iteration ()
  "replace iteration view with previous iteration. if you try to go before 0 it just reloads current"
  (interactive)
  (setq *pivotal-iteration*
        (if (= pivotal-current-iteration-number *pivotal-iteration*)
            pivotal-current-iteration-number
          (- *pivotal-iteration* 1)))
  (pivotal-get-iteration *pivotal-iteration*))

(defun pivotal-set-project ()
  "set the current project, and load the current iteration for that project"
  (interactive)
  (setq *pivotal-current-project* (pivotal-project-id-at-point))
  (setq *pivotal-iteration* pivotal-current-iteration-number)
  (pivotal-get-current))

(defun pivotal-get-story (id)
  "Open a single story for view / edit"
  (interactive)
  (assert-pivotal-api-token)
  (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" id)
               "GET"
               'pivotal-story-callback))

(defun pivotal-toggle-visibility ()
  "show/hide story detail"
  (interactive)
  (progn
    (let ((cur-invisible (member (pivotal-story-at-point) buffer-invisibility-spec)))
      (if cur-invisible
          (pivotal-show)
        (pivotal-hide)))
    (force-window-update (current-buffer))))

(defun pivotal-estimate-story (estimate)
  "assign an estimate to the story on the current line"
  (interactive "NEstimate: ")
  (message "going to set estimate to %s" estimate)
  (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" (pivotal-story-id-at-point))
             "PUT"
             'pivotal-update-current-story
             (format "<story><estimate>%s</estimate></story>" estimate)))

(defun pivotal-set-status ()
  "transition status according to the current status. assigns the story to user."
  (interactive)
  (let ((new-state (completing-read "Status: " pivotal-states nil t)))
    (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" (pivotal-story-id-at-point))
                 "PUT"
                 'pivotal-update-current-story
                 (format "<story><current_state>%s</current_state></story>" new-state))))

(defun pivotal-set-owner (new-owner-id)
  "set owner for the current story."
  (interactive
   (let ((member-name-id-alist (pivotal-project->member-name-id-alist *pivotal-current-project*)))
     (list (cdr (assoc (completing-read "New owner: "
                                        member-name-id-alist
                                        nil
                                        t
                                        nil
                                        'pivotal-story-owner-history)
                       member-name-id-alist)))))
  (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" (pivotal-story-id-at-point))
	       "PUT"
	       'pivotal-update-current-story
	       (format "<story><owned_by_id>%s</owned_by_id></story>" new-owner-id)))

(defun pivotal-add-comment (comment)
  "prompt user for comment and add it to the current story"
  (interactive "sAdd Comment: ")
  (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" (pivotal-story-id-at-point) "notes")
               "POST"
               'pivotal-add-comment-callback
               (format "<note><text>%s</text></note>" (xml-escape-string comment))))

(defun pivotal-add-task (task)
  "prompt user for a task and add it to the current story"
  (interactive "sAdd Task: ")
  (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" (pivotal-story-id-at-point) "tasks")
               "POST"
               'pivotal-add-task-callback
               (format "<task><description>%s</description></task>" (xml-escape-string task))))

(defun pivotal-check-task ()
  "marks current task as done"
  (interactive)
  (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" (pivotal-story-id-at-point) "tasks" (pivotal-task-id-at-point))
               "PUT"
               'pivotal-check-task-callback
               (format "<task><complete>true</complete></task>")))

(defun pivotal-kill-ring-save-story-url ()
  "saves the external story URL as if killed, but don't kill anything"
  (interactive)
  (let ((story-url (pivotal-story-url-at-point)))
    (kill-new story-url)
    (message (concat "copied story URL to kill ring: " story-url))))

(defun pivotal-open-story-in-browser ()
  "asks a WWW browser to load the story"
  (interactive)
  (browse-url (pivotal-story-url-at-point)))

(defun pivotal-open-current-project-in-browser ()
  "asks a WWW browser to load the current project"
  (interactive)
  (browse-url (pivotal-get-project-url *pivotal-current-project*)))

(defun pivotal-open-project-at-point-in-browser ()
  "asks a WWW browser to open the project at point"
  (interactive)
  (browse-url (pivotal-get-project-url (pivotal-project-id-at-point))))

;;;;;;;; CALLBACKS


(defun pivotal-iteration-callback (status)
  (let ((xml (pivotal-get-xml-from-current-buffer)))
    (with-current-buffer (get-buffer-create "*pivotal-iteration*")
      (pivotal-mode)
      (delete-region (point-min) (point-max))
      (switch-to-buffer (current-buffer))

      ;; for some reason trying to load an iteration that doesn't
      ;; exist returns the following xml
      ;; ((nil-classes ((type . array)))
      ;;  - Peer has closed the GnuTLS connection
      ;;  )
      (if (eq 'nil-classes (first (first xml)))
          (insert "No stories in this iteration yet")
        (pivotal-insert-iteration xml)))))

(defun pivotal-projects-callback (status)
  (let ((xml (pivotal-get-xml-from-current-buffer)))
    (with-current-buffer (get-buffer-create "*pivotal-projects*")
      (pivotal-project-mode)
      (delete-region (point-min) (point-max))
      (switch-to-buffer (current-buffer))
      (pivotal-insert-projects xml))))

(defun pivotal-story-callback (status)
  (let ((xml (pivotal-get-xml-from-current-buffer)))
    (delete-region (point-min) (point-max))
    (insert (pivotal-format-story xml)) (rename-buffer (concat "*pivotal-" (pivotal-story-attribute xml 'id) "*"))
    (switch-to-buffer (current-buffer))))

(defun pivotal-update-current-story (status)
  (let ((xml (pivotal-get-xml-from-current-buffer)))
    (if (eq :error (car status))
        (message "Error: %s" (pivotal-parse-errors xml))
      (with-current-buffer (get-buffer-create "*pivotal-iteration*")
        (pivotal-remove-story-at-point)
        (pivotal-insert-story xml)))))

(defun pivotal-add-comment-callback (status)
  (let* ((xml (pivotal-get-xml-from-current-buffer))
         (comment (pivotal-format-comment (car xml))))
    (with-current-buffer (get-buffer-create "*pivotal-iteration*")
      (pivotal-append-to-current-story comment))))

(defun pivotal-add-task-callback (status)
  (let* ((xml (pivotal-get-xml-from-current-buffer))
         (task (pivotal-format-task (car xml))))
    (with-current-buffer (get-buffer-create "*pivotal-iteration*")
      (pivotal-append-task-to-current-story task))))

(defun pivotal-check-task-callback (status)
  (let ((xml (pivotal-get-xml-from-current-buffer)))
    (if (eq :error (car status))
        (message "Error: %s" (pivotal-parse-errors xml))
      (with-current-buffer (get-buffer-create "*pivotal-iteration*")
        (let* ((task (car xml))
                (task-id (pivotal-element-value task 'id)))
          (save-excursion
            (goto-char (point-min))
            (re-search-forward (concat "ID:#" task-id))
            (beginning-of-line)
            ;; Looking at [ ]
            (forward-char 1)
            (delete-char 1)
            (insert "X")))))))

(defun pivotal-parse-errors (xml)
  (mapconcat (lambda (error)
               (car (last error)))
             (xml-get-children (car xml) 'error)
             " "))



;;;;;;;; MODE DEFINITIONS

(defface pivotal-title-face
  '((t :height 1.2 :underline t))
  "Face for iteration heading"
  :group 'pivotal)

(defface pivotal-section-face
  '((t :underline t))
  "Face for iteration heading"
  :group 'pivotal)

(defconst pivotal-font-lock-keywords
  `(("^\\(\\[.*?\\]\\)+" 0 font-lock-doc-face)
    ("^-.*-$" . 'pivotal-title-face)
    ("^--- [a-zA-Z]+$" . 'pivotal-section-face)))


(magit-define-popup pivotal-link-popup
  "Popup for opening stories in a web browser"
  :actions '((?o "Current story" pivotal-open-story-in-browser)
             (?p "Current project" pivotal-open-current-project-in-browser)
             (?l "Copy current story URL" pivotal-kill-ring-save-story-url)))

(magit-define-popup pivotal-story-popup
  "Popup for interacting with stories"
  :actions '((?e "Estimate" pivotal-estimate-story)
             (?c "Comment" pivotal-add-comment)
             (?s "Set status" pivotal-set-status)
             (?o "Set owner" pivotal-set-owner)
             (?t "Add task" pivotal-add-task)
             (?v "Check task" pivotal-check-task)))

(magit-define-popup pivotal-dispatch-popup
  "Popup console for dispatching other popups"
  :actions '("Popup commands"
             (?o "Openening in a browser" pivotal-link-popup)
             (?s "Modifying stories" pivotal-story-popup)
             "\
g      refresh current buffer
TAB    toggle story details
+      add new story
N      next iteration
P      previous iteration
^      list all pivotal projects

C-h m  show all keybindings"))



(define-derived-mode pivotal-mode fundamental-mode "Pivotal"
  (suppress-keymap pivotal-mode-map)
  (define-key pivotal-mode-map (kbd "n") 'next-line)
  (define-key pivotal-mode-map (kbd "p") 'previous-line)
  (define-key pivotal-mode-map (kbd "?") 'pivotal-dispatch-popup)

  (define-key pivotal-mode-map (kbd "<tab>") 'pivotal-toggle-visibility)
  (define-key pivotal-mode-map (kbd "g") 'pivotal-get-current)
  (define-key pivotal-mode-map (kbd "^") 'pivotal)
  (define-key pivotal-mode-map (kbd "+") 'pivotal-add-story)
  (define-key pivotal-mode-map (kbd "N") 'pivotal-next-iteration)
  (define-key pivotal-mode-map (kbd "P") 'pivotal-previous-iteration)

  ;; SubMenus
  (define-key pivotal-mode-map (kbd "o") 'pivotal-link-popup)
  (define-key pivotal-mode-map (kbd "s") 'pivotal-story-popup)

  (setq font-lock-defaults '(pivotal-font-lock-keywords))
  (font-lock-mode))

(define-derived-mode pivotal-project-mode fundamental-mode "PivotalProjects"
  (suppress-keymap pivotal-project-mode-map)
  (define-key pivotal-project-mode-map (kbd "R") 'pivotal-get-projects)
  (define-key pivotal-project-mode-map (kbd "n") 'next-line)
  (define-key pivotal-project-mode-map (kbd "p") 'previous-line)
  ;; add some bindings for my vim friends
  (define-key pivotal-project-mode-map (kbd "j") 'next-line)
  (define-key pivotal-project-mode-map (kbd "k") 'previous-line)

  (define-key pivotal-project-mode-map (kbd "o") 'pivotal-open-project-at-point-in-browser)
  (define-key pivotal-project-mode-map (kbd ".") 'pivotal-set-project)
  (define-key pivotal-project-mode-map (kbd "C-m") 'pivotal-set-project))


;;;;;;;;; SUPPORTING FUNS


(defun pivotal-url (&rest parts-of-url)
  (apply 'concat
         pivotal-base-url
         (mapcar (lambda (part) (concat "/" part)) parts-of-url)))

(defun pivotal-v5-url (&rest parts-of-url)
  (let ((v3-url (apply 'pivotal-url parts-of-url)))
   (replace-regexp-in-string "/v3/" "/v5/" v3-url)))

(defun pivotal-api (url method callback &optional xml-data)
  (let ((url-request-method method)
        (url-request-data xml-data)
        (url-request-extra-headers `(("X-TrackerToken" . ,pivotal-api-token)
                                     ("Content-Type" . "application/xml"))))
    (url-retrieve url callback)))

(defun pivotal-clear-headers (buffer)
  (mail-narrow-to-head)
  (delete-region (point-min) (point-max))
  (widen))

(defun pivotal-json-api (url method &optional json-data callback)
  (let ((url-request-method method)
        (url-request-data json-data)
        (url-request-extra-headers `(("X-TrackerToken" . ,pivotal-api-token)
                                     ("Content-Type" . "application/json"))))
    (if callback
        (url-retrieve url callback)
      (url-retrieve-synchronously url))))

(defun pivotal-get-json-from-current-buffer ()
  (let ((json (condition-case nil
                  (json-read-from-string (buffer-substring-no-properties (point-min) (point-max)))
                (error :reissue))))
    (kill-buffer)
    json))

(defun pivotal-get-project-members (project-id)
  (with-current-buffer (pivotal-json-api (pivotal-v5-url "projects" project-id "memberships")
                                         "GET")
    (pivotal-clear-headers (current-buffer))
    (let ((project-members (pivotal-get-json-from-current-buffer)))
      (if (eq :reissue project-members)
          (pivotal-get-project-members project-id)
        project-members))))

(defun pivotal-get-project (project-id)
  (with-current-buffer (pivotal-json-api (pivotal-v5-url "projects" project-id)
                                         "GET")
    (pivotal-clear-headers (current-buffer))
    (let ((project (pivotal-get-json-from-current-buffer)))
      (if (eq :reissue project)
          (pivotal-get-project project-id)
        project))))

(defun pivotal-get-project-url (project-id)
  (replace-regexp-in-string "/services/v3/" "/n/"
                            (pivotal-url
                             "projects" project-id)))

(defun pivotal-get-estimate-scale (project-id)
  (let* ((project             (pivotal-get-project project-id))
         (point-scale-str     (cdr (assoc 'point_scale project)))
         (estimate-scale-strs (split-string point-scale-str ",")))
    estimate-scale-strs))

(defvar pivotal-story-name-history '())

(defvar pivotal-story-description-history '())

(defvar pivotal-story-owner-history '())

(defvar pivotal-story-requester-history '())

(defvar pivotal-story-estimate-history '())

(defun pivotal-project-id-at-point ()
  (save-excursion
    (beginning-of-line)
    (re-search-forward "\\([0-9]+\\)" (point-at-eol))
    (match-string 1)))

(defun pivotal-project-member->member-name-id-association (project-member)
  `(,(cdr (assoc 'name (assoc 'person project-member)))
    .
    ,(cdr (assoc 'id (assoc 'person project-member)))))

(defun pivotal-project->member-name-id-alist (project-id)
  (let ((project-members (pivotal-get-project-members project-id)))
    (mapcar 'pivotal-project-member->member-name-id-association
            (pivotal-get-project-members project-id))))

(defun pivotal-add-story (name description owner-id requester-id estimate)
  (interactive
   (let ((member-name-id-alist (pivotal-project->member-name-id-alist *pivotal-current-project*))
         (estimate-scale       (pivotal-get-estimate-scale *pivotal-current-project*)))
     (list (read-string "Name: " nil 'pivotal-story-name-history)
           (read-string "Description: " nil 'pivotal-story-description-history)
           (cdr (assoc (completing-read "Owner: "
                                        member-name-id-alist
                                        nil
                                        t
                                        nil
                                        'pivotal-story-owner-history)
                       member-name-id-alist))
           (cdr (assoc (completing-read "Requester: "
                                        member-name-id-alist
                                        nil
                                        t
                                        nil
                                        'pivotal-story-requester-history)
                       member-name-id-alist))
           (string-to-number (completing-read "Estimate: "
                                              estimate-scale
                                              nil
                                              t
                                              nil
                                              'pivotal-story-estimate-history)))))
  (kill-buffer (pivotal-json-api (pivotal-v5-url "projects" *pivotal-current-project* "stories")
                                 "POST"
                                 (json-encode (list :name            name
                                                    :description     description
                                                    :owned_by_id     owner-id
                                                    :requested_by_id requester-id
                                                    :estimate        estimate))))
  (message "Story added!"))

(defun assert-pivotal-api-token ()
  (assert (not (string-equal "" pivotal-api-token)) nil "Please set pivotal-api-token: M-x customize-group RET pivotal RET"))

(defun pivotal-get-xml-from-current-buffer ()
  (let ((xml (if (functionp 'xml-parse-fragment)
                 (cdr (xml-parse-fragment))
               (xml-parse-region))))
    (kill-buffer)
    xml))

(defun pivotal-insert-projects (xml)
  "render projects one per line in their own buffer"
  (let ((projects (pivotal-get-project-data xml)))
    (mapc (lambda (project)
            (insert (format "%7.7s %s\n" (car project) (cadr project))))
          projects)))

(defun pivotal-get-project-data (xml)
  "return a list of (id name) pairs"
  (mapcar (lambda (proj)
            (list (pivotal-element-value proj 'id)
                  (pivotal-element-value proj 'name)))
          (xml-get-children (car xml) 'project)))

(defun pivotal-insert-iteration (iteration-xml)
  "extract story information from xml and insert it into current buffer"
  (insert (if (= pivotal-current-iteration-number *pivotal-iteration*)
              (format "- Current Iteration - Ending %s -\n"
                      (pivotal-iteration-date iteration-xml 'finish))
            (format "- Iteration Starting %s -\n"
                    (pivotal-iteration-date iteration-xml 'start))))
  (mapc 'pivotal-insert-story
        (pivotal-extract-stories-from-iteration-xml iteration-xml)))

(defun pivotal-insert-story (story)
  "insert single story into current buffer"
  (let* ((start-point (point))
         (_ (insert (pivotal-format-story-oneline story)))
         (end-of-oneline (point))
         (_ (insert (pivotal-format-story story)))
         (end-of-detail (point)))
    (pivotal-mark-story start-point end-of-detail (pivotal-story-attribute story 'id))
    (pivotal-mark-invisibility end-of-oneline end-of-detail)
    (pivotal-hide end-of-oneline)))

(defun pivotal-append-to-current-story (text)
  (progn
    (pivotal-show)
    (let* ((story-id (pivotal-story-id-at-point (point)))
           (bounds (pivotal-story-boundaries (point)))
           (story-end (second bounds))
           (_ (goto-char story-end))
           (_ (insert text))
           (new-end (point)))
      (pivotal-mark-story story-end new-end story-id)
      (pivotal-mark-invisibility story-end new-end))))

(defun pivotal-append-task-to-current-story (task)
  (progn
    (pivotal-show)
    (let* ((story-id (pivotal-story-id-at-point (point)))
            (bounds (pivotal-story-boundaries (point)))
            (story-beginning (first bounds)))
      (goto-char story-beginning)
      (re-search-forward "--- Comments")
      (forward-line -1)
      (let ((begin-of-task (point)))
        (insert task)
        ;; Mark this new line has belonging to the story
        (pivotal-mark-story begin-of-task (point) story-id)))))



(defun pivotal-invisibility-id (story-id)
  (intern (concat "pivotal-" story-id)))

(defun pivotal-mark-story (min max story-id)
  (put-text-property min max 'pivotal-story-id story-id))

(defun pivotal-mark-invisibility (min max)
  (let ((overlay (make-overlay min max)))
    (overlay-put overlay 'invisible (pivotal-story-at-point min))))

(defun pivotal-hide (&optional position)
  (add-to-invisibility-spec (pivotal-story-at-point position)))

(defun pivotal-show (&optional position)
  (remove-from-invisibility-spec (pivotal-story-at-point position)))

(defun pivotal-story-at-point (&optional position)
  (let* ((buf-point (if position position (point)))
         (story-id (get-text-property buf-point 'pivotal-story-id))
         (invis-id (pivotal-invisibility-id story-id)))
    invis-id))

(defun pivotal-story-id-at-point (&optional position)
  (let* ((story-sym (pivotal-story-at-point position))
         (story-str (symbol-name story-sym)))
    (string-match "pivotal-\\([0-9]+\\)" story-str)
    (match-string 1 story-str)))

(defun pivotal-story-url-at-point (&optional position)
  (replace-regexp-in-string "/services/v3/" "/n/"
                            (pivotal-url
                             "projects" *pivotal-current-project*
                             "stories" (pivotal-story-id-at-point position))))

(defun pivotal-task-id-at-point (&optional position)
  (save-excursion
    (beginning-of-line)
    (forward-char 4)
    (cond ((looking-at "Task")
            (re-search-forward "ID:#\\([0-9]\\)")
            (forward-char 3)
            (number-to-string (number-at-point)))
      (t (beep)
        (message "%s" "Could not find task at point")))))

(defun pivotal-format-story (story)
  (format "%s #%s
Status:       %s
Requested By: %s
Owned By:     %s
Labels:       %s

--- Description
%s

--- Tasks
%s

--- Comments
%s
"
          (pivotal-story-attribute story 'story_type)
          (pivotal-story-attribute story 'id)
          (pivotal-story-attribute story 'current_state)
          (pivotal-story-attribute story 'requested_by)
          (pivotal-story-attribute story 'owned_by)
          (pivotal-story-attribute story 'labels)
          (pivotal-story-attribute story 'description)
          (pivotal-tasks story)
          (pivotal-comments story)))

(defun pivotal-format-story-oneline (story)
  (let ((owner (pivotal-story-attribute story 'owned_by))
        (estimate (pivotal-story-attribute story 'estimate))
        (story-name (pivotal-story-attribute story 'name))
        (status (pivotal-story-attribute story 'current_state)))
    (format "[%4.4s][%1.1s][%9.9s] %.80s\n" owner estimate status story-name)))

(defun pivotal-remove-story-at-point ()
  "delete all characters that belong to the current story. Put point at the first char of the next story."
  (interactive)
  (let* ((bounds (pivotal-story-boundaries (point)))
         (first-point (first bounds))
         (last-point (second bounds)))
    (delete-region first-point last-point)
    (if (< (point) (point-max))
        (forward-char))))

(defun pivotal-story-boundaries (point)
  (let ((story-id (get-text-property (point) 'pivotal-story-id))
        (first-point (point))
        (last-point (point)))
    (while (pivotal-point-has-story-id (- first-point 1) story-id)
      (setq first-point (- first-point 1)))
    (while (pivotal-point-has-story-id (+ last-point 1) story-id)
      (setq last-point (+ last-point 1)))
    (list first-point last-point)))

(defun pivotal-point-has-story-id (point story-id)
  (if (and (<= point (point-max)) (>= point (point-min)))
      (string-equal (get-text-property point 'pivotal-story-id) story-id)
    nil))

(defun pivotal-extract-stories-from-iteration-xml (iteration-xml)
  (pivotal-xml-collection (car iteration-xml) `(iteration stories story)))

(defun pivotal-story-attribute (xml attribute)
  (let*
      ((story (if (eq 'story (car xml))
                  xml
                (car xml)))
       (value (pivotal-element-value story attribute)))
    (if (symbolp value)
        (symbol-name value)
      value)))

(defun pivotal-element-value (xml element)
  (let ((node (xml-get-children xml element)))
    (caddar node)))

(defun pivotal-xml-collection (xml structure)
  "return a collection of nodes found by the given structure"
  (let ((results nil)
        (node xml))
    (mapc (lambda (element)
            (progn
              (setq results (xml-get-children node element))
              (setq node (first results))))
          structure)
    results))

(defun pivotal-iteration-date (xml attr)
  (first (split-string
          (third (first (pivotal-xml-collection (car xml) `(iteration ,attr))))
          " ")))

(defun pivotal-comments (story)
  (let ((notes (pivotal-xml-collection story `(notes note)))
        (comments ""))
    (mapc (lambda (note)
            (setq comments (concat comments (pivotal-format-comment note))))
          notes)
    comments))

(defun pivotal-format-comment (note)
  (let ((text (pivotal-element-value note 'text))
        (author (pivotal-element-value note 'author))
        (created-at (pivotal-element-value note 'noted_at)))
    (if created-at
        (setq created-at (substring created-at 5 10)))
    (format "%s  --  %s on %s\n" text author created-at)))

(defun pivotal-tasks (story)
  (let ((tasks (pivotal-xml-collection story `(tasks task)))
        (tasks-string ""))
    (mapc (lambda (task)
            (setq tasks-string (concat tasks-string (pivotal-format-task task))))
          tasks)
    tasks-string))

(defun pivotal-format-task (task)
  (format "[%s] Task %s (ID:#%s) -- %s\n"
          (if (string= (pivotal-element-value task 'complete) "true")
              "X"
            " ")
          (pivotal-element-value task 'position)
          (pivotal-element-value task 'id)
          (pivotal-element-value task 'description)))

(provide 'pivotal-tracker)

;;; pivotal-tracker.el ends here
