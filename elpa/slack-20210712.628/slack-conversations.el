;;; slack-conversations.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <yuya373@yuya373>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'slack-util)
(require 'slack-request)
(require 'slack-room)
(require 'slack-counts)
(require 'slack-modeline)
(require 'slack-message)
(require 'slack-create-message)

(defvar slack-completing-read-function)

(defconst slack-conversations-archive-url
  "https://slack.com/api/conversations.archive")
(defconst slack-conversations-unarchive-url
  "https://slack.com/api/conversations.unarchive")
(defconst slack-conversations-invite-url
  "https://slack.com/api/conversations.invite")
(defconst slack-conversations-join-url
  "https://slack.com/api/conversations.join")
(defconst slack-conversations-leave-url
  "https://slack.com/api/conversations.leave")
(defconst slack-conversations-rename-url
  "https://slack.com/api/conversations.rename")
(defconst slack-conversations-set-purpose-url
  "https://slack.com/api/conversations.setPurpose")
(defconst slack-conversations-set-topic-url
  "https://slack.com/api/conversations.setTopic")
(defconst slack-conversations-members-url
  "https://slack.com/api/conversations.members")
(defconst slack-conversations-kick-url
  "https://slack.com/api/conversations.kick")
(defconst slack-conversations-list-url
  "https://slack.com/api/conversations.list")
(defconst slack-conversations-info-url
  "https://slack.com/api/conversations.info")
(defconst slack-conversations-replies-url
  "https://slack.com/api/conversations.replies")
(defconst slack-conversations-close-url
  "https://slack.com/api/conversations.close")
(defconst slack-conversations-create-url
  "https://slack.com/api/conversations.create")
(defconst slack-conversations-history-url
  "https://slack.com/api/conversations.history")
(defconst slack-conversations-open-url
  "https://slack.com/api/conversations.open")
(defconst slack-conversations-view-url
  "https://slack.com/api/conversations.view")
(defconst slack-conversations-mark-url
  "https://slack.com/api/conversations.mark")

(cl-defun slack-conversations-success-handler (team &key on-errors on-success)
  (cl-function
   (lambda (&key data &allow-other-keys)
     (cl-labels
         ((replace-underscore-with-space (s)
                                         (replace-regexp-in-string "_"
                                                                   " "
                                                                   s))
          (log-error
           (_)
           (slack-if-let*
               ((err (plist-get data :error))
                (message (format "%s"
                                 (replace-underscore-with-space
                                  err))))
               (slack-log message team :level 'error))
           (slack-if-let*
               ((errors (plist-get data :errors))
                (has-handler (functionp on-errors)))
               (funcall on-errors errors))))
       (slack-request-handle-error
        (data "conversations" #'log-error)
        (slack-if-let* ((warning (plist-get data :warning)))
            (slack-log (format "%s" (replace-underscore-with-space
                                     warning))
                       team
                       :level 'warn)
          (when (functionp on-success)
            (funcall on-success data))))))))

(defun slack-conversations-archive (room team)
  (let ((id (oref room id)))
    (slack-request
     (slack-request-create
      slack-conversations-archive-url
      team
      :type "POST"
      :params (list (cons "channel" id))
      :success (slack-conversations-success-handler team)))))

(defun slack-conversations-unarchive (room team)
  (let ((channel (oref room id)))
    (slack-request
     (slack-request-create
      slack-conversations-unarchive-url
      team
      :type "POST"
      :params (list (cons "channel" channel))
      :success (slack-conversations-success-handler team)))))

(defun slack-conversations-invite (room team)
  (let* ((channel (oref room id))
         (user-names (slack-user-names
                      team #'(lambda (users)
                               (cl-remove-if #'(lambda (e)
                                                 (or (string= (oref team self-id)
                                                              (plist-get e :id))
                                                     (string= (plist-get e :id)
                                                              "USLACKBOT")
                                                     (cl-find (plist-get e :id)
                                                              (slack-room-members room)
                                                              :test #'string=)))
                                             users))))
         (users nil))
    (cl-labels
        ((already-selected-p
          (user-name)
          (cl-find-if #'(lambda (e)
                          (string= e
                                   (plist-get (cdr user-name)
                                              :id)))
                      users))
         (filter-selected (user-names)
                          (cl-remove-if #'already-selected-p
                                        user-names)))
      (cl-loop for i from 1 upto 30
               as candidates = (filter-selected user-names)
               as selected = (slack-select-from-list
                                 (candidates "Select User: "))
               while selected
               do (push (plist-get selected :id) users)))
    (setq users (mapconcat #'identity users ","))
    (cl-labels
        ((errors-handler
          (errors)
          (let ((message
                 (mapconcat #'(lambda (err)
                                (let ((msg (plist-get err :error))
                                      (user (plist-get err :user)))
                                  (format "%s%s"
                                          (replace-regexp-in-string "_" " " msg)
                                          (or (and user (format ": %s" user))
                                              ""))))
                            errors
                            ", ")))
            (slack-log message team :level 'error))))
      (slack-request
       (slack-request-create
        slack-conversations-invite-url
        team
        :type "POST"
        :params (list (cons "channel" channel)
                      (cons "users" users))
        :success (slack-conversations-success-handler team
                                                      :on-errors
                                                      #'errors-handler))))))

(defun slack-conversations-join (room team &optional on-success)
  (cl-labels
      ((success (data)
                (when (eq 'slack-channel
                          (eieio-object-class-name room))
                  (oset room is-member t))
                (when (functionp on-success)
                  (funcall on-success data))))
    (let ((channel (oref room id)))
      (slack-request
       (slack-request-create
        slack-conversations-join-url
        team
        :type "POST"
        :params (list (cons "channel" channel))
        :success (slack-conversations-success-handler
                  team
                  :on-success #'success))))))

(defun slack-conversations-leave (room team)
  (let ((channel (oref room id)))
    (slack-request
     (slack-request-create
      slack-conversations-leave-url
      team
      :type "POST"
      :params (list (cons "channel" channel))
      :success (slack-conversations-success-handler team)))))

(defun slack-conversations-rename (room team)
  (let ((channel (oref room id))
        (name (read-from-minibuffer "Name: ")))
    (slack-request
     (slack-request-create
      slack-conversations-rename-url
      team
      :type "POST"
      :params (list (cons "channel" channel)
                    (cons "name" name))
      :success (slack-conversations-success-handler team)))))

(defun slack-conversations-set-purpose (room team)
  (let ((channel (oref room id))
        (purpose (read-from-minibuffer "Purpose: ")))
    (cl-labels
        ((on-success (data)
                     (let* ((channel (plist-get data :channel))
                            (purpose (plist-get channel :purpose)))
                       (oset room purpose purpose))))
      (slack-request
       (slack-request-create
        slack-conversations-set-purpose-url
        team
        :type "POST"
        :params (list (cons "channel" channel)
                      (cons "purpose" purpose))
        :success (slack-conversations-success-handler team
                                                      :on-success
                                                      #'on-success))))))

(defun slack-conversations-set-topic (room team)
  (let ((channel (oref room id))
        (topic (read-from-minibuffer "Topic: ")))
    (cl-labels
        ((on-success (data)
                     (let* ((channel (plist-get data :channel))
                            (topic (plist-get channel :topic)))
                       (oset room topic topic))))
      (slack-request
       (slack-request-create
        slack-conversations-set-topic-url
        team
        :type "POST"
        :params (list (cons "channel" channel)
                      (cons "topic" topic))
        :success (slack-conversations-success-handler team
                                                      :on-success
                                                      #'on-success))))))

(defun slack-conversations-kick (room team)
  (let* ((candidates (cl-loop for user in (slack-user-names team)
                              if (cl-find (plist-get (cdr user) :id)
                                          (slack-room-members room)
                                          :test #'string=)
                              collect user))
         (selected (funcall slack-completing-read-function
                            "Select User: "
                            candidates
                            nil t))
         (user (cdr-safe (cl-assoc selected candidates :test #'string=))))
    (when user
      (slack-request
       (slack-request-create
        slack-conversations-kick-url
        team
        :type "POST"
        :params (list (cons "channel" (oref room id))
                      (cons "user" (plist-get user :id)))
        :success (slack-conversations-success-handler team))))))

(defun slack-conversations-list (team success-callback &optional types)
  (let ((cursor nil)
        (channels nil)
        (groups nil)
        (ims nil)
        (types (or types (list "public_channel"
                               "private_channel"
                               "mpim"
                               "im"))))
    (cl-labels
        ((on-success
          (&key data &allow-other-keys)
          (slack-request-handle-error
           (data "slack-conversations-list")
           (cl-loop for c in (plist-get data :channels)
                    do (cond
                        ((eq t (plist-get c :is_channel))
                         (push (slack-room-create c 'slack-channel)
                               channels))
                        ((eq t (plist-get c :is_im))
                         (push (slack-room-create c 'slack-im)
                               ims))
                        ((eq t (plist-get c :is_group))
                         (push (slack-room-create c 'slack-group)
                               groups))))
           (slack-if-let*
               ((meta (plist-get data :response_metadata))
                (next-cursor (plist-get meta :next_cursor))
                (has-cursor (< 0 (length next-cursor))))
               (progn
                 (setq cursor next-cursor)
                 (request))
             (funcall success-callback
                      channels groups ims))))
         (request ()
           (slack-request
            (slack-request-create
             slack-conversations-list-url
             team
             :params (list (cons "types" (mapconcat #'identity types ","))
                           (and cursor (cons "cursor" cursor)))
             :success #'on-success))))
      (request))))

(defun slack-conversations-info (room team &optional after-success)
  (slack-request
   (slack-conversations-info-request room team after-success)))

(defun slack-conversations-info-request (room team &optional after-success)
  (cl-labels
      ((success (&key data &allow-other-keys)
                (slack-request-handle-error
                 (data "slack-conversations-info")
                 (let ((new-room (slack-room-create
                                  (plist-get data :channel)
                                  (eieio-object-class-name room))))
                   (slack-team-set-room team new-room))
                 (when (functionp after-success)
                   (funcall after-success)))))
    (slack-request-create
     slack-conversations-info-url
     team
     :params (list (cons "channel" (oref room id)))
     :success #'success)))

(cl-defun slack-conversations-replies (room ts team &key after-success (cursor nil) (oldest nil))
  (let ((channel (oref room id)))
    (cl-labels
        ((create-message (payload)
                         (slack-message-create payload
                                               team
                                               room))
         (callback (messages next-cursor has-more)
                   (when (functionp after-success)
                     (funcall after-success
                              messages
                              next-cursor
                              has-more)))
         (on-success (&key data &allow-other-keys)
                     (slack-request-handle-error
                      (data "slack-conversations-replies")
                      (let* ((messages (mapcar #'create-message
                                               (plist-get data :messages)))
                             (meta (plist-get data :response_metadata))
                             (next-cursor (and meta (plist-get meta :next_cursor)))
                             (has-more (eq t (plist-get data :has_more)))
                             (user-ids (slack-team-missing-user-ids
                                        team (cl-loop for m in messages
                                                      nconc (slack-message-user-ids m)))))

                        (if (< 0 (length user-ids))
                            (slack-users-info-request
                             user-ids team
                             :after-success #'(lambda ()
                                                (callback messages next-cursor has-more)))
                          (callback messages next-cursor has-more))))))
      (slack-request
       (slack-request-create
        slack-conversations-replies-url
        team
        :params (list (cons "channel" channel)
                      (cons "ts" ts)
                      (if cursor (cons "cursor" cursor)
                        (cons "oldest" oldest)))
        :success #'on-success)))))

(defun slack-conversations-close (room team &optional after-success)
  (let ((channel (oref room id)))
    (cl-labels
        ((on-success (data)
                     (when (functionp after-success)
                       (funcall after-success data))))
      (slack-request
       (slack-request-create
        slack-conversations-close-url
        team
        :type "POST"
        :params (list (cons "channel" channel))
        :success (slack-conversations-success-handler
                  team :on-success #'on-success))))))

(cl-defun slack-conversations-create (team &optional (is-private "false"))
  (let ((name (read-from-minibuffer "Name: "))
        (is-private (or is-private
                        (if (y-or-n-p "Private? ")
                            "true" "false"))))
    (slack-request
     (slack-request-create
      slack-conversations-create-url
      team
      :type "POST"
      :params (list (cons "name" name)
                    (cons "is_private" is-private))
      :success (slack-conversations-success-handler team)))))

(cl-defun slack-conversations-history (room team &key
                                            (after-success nil)
                                            (cursor nil)
                                            (latest nil)
                                            (oldest nil)
                                            (inclusive nil)
                                            (limit "100"))
  (let ((channel (oref room id)))
    (cl-labels
        ((callback (messages next-cursor)
                   (when (functionp after-success)
                     (funcall after-success
                              messages
                              next-cursor)))
         (success (data)
                  (let* ((meta (plist-get data :response_metadata))
                         (next-cursor (or (plist-get meta :next_cursor) ""))
                         (messages (cl-loop for e in (plist-get data :messages)
                                            collect (slack-message-create e team room)))
                         (user-ids (slack-team-missing-user-ids
                                    team (cl-loop for m in messages
                                                  nconc (slack-message-user-ids m)))))
                    (if (< 0 (length user-ids))
                        (slack-user-info-request
                         user-ids team
                         :after-success #'(lambda ()
                                            (callback messages next-cursor)))
                      (callback messages next-cursor)))))
      (slack-request
       (slack-request-create
        slack-conversations-history-url
        team
        :params (list (cons "channel" channel)
                      (cons "limit" limit)
                      (and cursor (cons "cursor" cursor))
                      (and latest (cons "latest" latest))
                      (and oldest (cons "oldest" oldest))
                      (and inclusive (cons "inclusive" inclusive)))
        :success (slack-conversations-success-handler
                  team :on-success #'success))))))

(defun slack-conversations-members (room team &optional cursor after-success)
  (if (slack-room-members-loaded-p room)
      (when (functionp after-success)
        (funcall after-success (slack-room-members room) ""))
    (cl-labels
        ((callback (members next-cursor)
                   (when (< (length next-cursor) 1)
                     (slack-room-members-loaded room))
                   (when (functionp after-success)
                     (funcall after-success members next-cursor)))
         (success (data)
                  (let* ((meta (plist-get data :response_metadata))
                         (next-cursor (or (and meta (plist-get meta :next_cursor)) ""))
                         (members (plist-get data :members))
                         (missing-user-ids (slack-team-missing-user-ids team members)))
                    (slack-room-set-members room members)


                    (if (< 0 (length missing-user-ids))
                        (slack-user-info-request
                         missing-user-ids
                         team
                         :after-success #'(lambda ()
                                            (callback members next-cursor)))
                      (callback members next-cursor)))))
      (slack-request
       (slack-request-create
        slack-conversations-members-url
        team
        :params (list (cons "channel" (oref room id))
                      (cons "limit" "100")
                      (and cursor (cons "cursor" cursor)))
        :success (slack-conversations-success-handler
                  team :on-success #'success))))))

(cl-defun slack-conversations-open (team &key room user-ids)
  (let ((channel (or (and room (oref room id))
                     ""))
        (users (mapconcat #'identity user-ids ",")))
    (slack-request
     (slack-request-create
      slack-conversations-open-url
      team
      :type "POST"
      :params (list (if (< 0 (length users))
                        (cons "users" users)
                      (cons "channel" channel)))
      :success (slack-conversations-success-handler team)))))

(cl-defun slack-conversations-view (room team &key
                                         (after-success nil)
                                         (cursor nil)
                                         (latest nil)
                                         (oldest nil)
                                         (inclusive nil)
                                         (count "100"))
  (cl-labels
      ((success (&key data &allow-other-keys)
                (slack-request-handle-error
                 (data "slack-conversations-view")
                 (let* ((key (cl-case (eieio-object-class-name room)
                               (slack-channel :channel)
                               (slack-im :im)
                               (slack-group :group)))
                        (new-room (slack-room-create
                                   (plist-get data key)
                                   (eieio-object-class-name room)))
                        (bots (plist-get data :bots))
                        (users (plist-get data :users))
                        (history (plist-get data :history))
                        (messages (cl-loop for e in (plist-get history :messages)
                                           collect (slack-message-create e
                                                                         team
                                                                         room)))
                        (meta (plist-get data :response_metadata))
                        (next-cursor (or (and meta (plist-get meta :next_cursor))
                                         "")))
                   (slack-team-set-room team new-room)
                   (slack-team-set-users team users)
                   (slack-team-set-bots team bots)
                   (when (functionp after-success)
                     (funcall after-success messages next-cursor))))))
    (slack-request
     (slack-request-create
      slack-conversations-view-url
      team
      :type "POST"
      :params (list (cons "name" (oref room id))
                    (cons "include_pin_count" "true")
                    (cons "include_full_users" "true")
                    (cons "count" count)
                    (and cursor (cons "cursor" cursor))
                    (and latest (cons "latest" latest))
                    (and oldest (cons "oldest" oldest))
                    (and inclusive (cons "inclusive" inclusive)))
      :success #'success))))

(defun slack-conversations-mark (room team ts &optional after-success)
  (cl-labels ((on-success (&rest _ignore)
                          (when (functionp after-success)
                            (funcall after-success))))
    (slack-request
     (slack-request-create
      slack-conversations-mark-url
      team
      :type "POST"
      :params (list (cons "channel"  (oref room id))
                    (cons "ts"  ts))
      :success (slack-conversations-success-handler team :on-success #'on-success)))))

(provide 'slack-conversations)
;;; slack-conversations.el ends here
