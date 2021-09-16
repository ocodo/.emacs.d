;;; slack-message-reaction.el --- adding, removing reaction from message  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
;; Keywords:

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

;;

;;; Code:

(require 'slack-util)
(require 'slack-message)
(require 'slack-reaction)
(require 'slack-room)
(require 'slack-file)
(require 'slack-request)
(require 'slack-emoji)

(defvar slack-current-buffer)
(defvar slack-completing-read-function)

(defconst slack-message-reaction-add-url "https://slack.com/api/reactions.add")
(defconst slack-message-reaction-remove-url "https://slack.com/api/reactions.remove")
(defcustom slack-invalid-emojis '("^:flag_" "tone[[:digit:]]:$" "-" "^[^:].*[^:]$" "\\Ca")
  "Invalid emoji regex.  Slack server treated some emojis as Invalid."
  :type '(repeat regexp)
  :group 'slack)

(defun slack-get-file-id ()
  (get-text-property 0 'file-id (thing-at-point 'line)))

(defun slack-file-add-reaction (file-id reaction team)
  (slack-message-reaction-add-request (list (cons "name" reaction)
                                            (cons "file" file-id))
                                      team))

(defun slack-file-remove-reaction (file-id team)
  (let* ((file (slack-file-find file-id team))
         (reaction (slack-message-reaction-select
                    (slack-message-reactions file))))
    (slack-message-reaction-remove-request
     (list (cons "file" file-id)
           (cons "name" reaction))
     team)))

(defun slack-message-show-reaction-users ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer)
                  (team (slack-buffer-team buf)))
      (slack-if-let* ((reaction (ignore-errors
                                  (get-text-property (point)
                                                     'reaction))))
          (slack-reaction-help-text reaction
                                    team
                                    #'(lambda (message) (message message)))
        (message "Can't get reaction:"))))

(defun slack-message-reaction-select (reactions)
  (let ((list (mapcar #'(lambda (r)
                          (cons (oref r name)
                                (oref r name)))
                      reactions)))
    (slack-select-from-list
        (list "Select Reaction: ")
        selected)))

(defun slack-message-reaction-input (team)
  (let ((reaction (slack-select-emoji team)))
    (if (and (string-prefix-p ":" reaction)
             (string-suffix-p ":" reaction))
        (substring reaction 1 -1)
      reaction)))

(defun slack-message-reaction-add (reaction ts room team)
  (slack-if-let* ((message (slack-room-find-message room ts)))
      (let ((params (list (cons "channel" (oref room id))
                          (slack-message-get-param-for-reaction message)
                          (cons "name" reaction))))
        (slack-message-reaction-add-request params team))))

(defun slack-message-reaction-add-request (params team)
  (cl-labels ((on-reaction-add
               (&key data &allow-other-keys)
               (slack-request-handle-error
                (data "slack-message-reaction-add-request"))))
    (slack-request
     (slack-request-create
      slack-message-reaction-add-url
      team
      :type "POST"
      :params params
      :success #'on-reaction-add))))

(defun slack-message-reaction-remove (reaction ts room team)
  (slack-if-let* ((message (slack-room-find-message room ts)))
      (let ((params (list (cons "channel" (oref room id))
                          (slack-message-get-param-for-reaction message)
                          (cons "name" reaction))))
        (slack-message-reaction-remove-request params team))))

(defun slack-message-reaction-remove-request (params team)
  (cl-labels ((on-reaction-remove
               (&key data &allow-other-keys)
               (slack-request-handle-error
                (data "slack-message-reaction-remove-request"))))
    (slack-request
     (slack-request-create
      slack-message-reaction-remove-url
      team
      :type "POST"
      :params params
      :success #'on-reaction-remove))))

(provide 'slack-message-reaction)
;;; slack-message-reaction.el ends here
