;;; slack-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "slack" "slack.el" (0 0 0 0))
;;; Generated autoloads from slack.el

(autoload 'slack-start "slack" "\


\(fn &optional TEAM)" t nil)

(autoload 'slack-register-team "slack" "\
PLIST must contain :name and :token.
Available options (property name, type, default value)
:subscribed-channels [ list symbol ] '()
  notified when new message arrived in these channels.
:default [boolean] nil
  if `slack-prefer-current-team' is t,
  some functions use this team without asking.
:full-and-display-names [boolean] nil
  if t, use full name to display user name.
:mark-as-read-immediately [boolean] these
  if t, mark messages as read when open channel.
  if nil, mark messages as read when cursor hovered.
:modeline-enabled [boolean] nil
  if t, display mention count and has unread in modeline.
:modeline-name [or nil string] nil
  use this value in modeline.
  if nil, use team name.
:visible-threads [boolean] nil
  if t, thread replies are also displayed in channel buffer.
:websocket-event-log-enabled [boolean] nil
  if t, websocket event is logged.
  use `slack-log-open-event-buffer' to open the buffer.
:animate-image [boolean] nil
  if t, animate gif images.

\(fn &rest PLIST)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack" '("slack-")))

;;;***

;;;### (autoloads nil "slack-action" "slack-action.el" (0 0 0 0))
;;; Generated autoloads from slack-action.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-action" '("slack-")))

;;;***

;;;### (autoloads nil "slack-all-threads-buffer" "slack-all-threads-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-all-threads-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-all-threads-buffer" '("slack-")))

;;;***

;;;### (autoloads nil "slack-attachment" "slack-attachment.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from slack-attachment.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-attachment" '("slack-")))

;;;***

;;;### (autoloads nil "slack-block" "slack-block.el" (0 0 0 0))
;;; Generated autoloads from slack-block.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-block" '("slack-")))

;;;***

;;;### (autoloads nil "slack-bot" "slack-bot.el" (0 0 0 0))
;;; Generated autoloads from slack-bot.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-bot" '("slack-")))

;;;***

;;;### (autoloads nil "slack-bot-message" "slack-bot-message.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-bot-message.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-bot-message" '("slack-bot-")))

;;;***

;;;### (autoloads nil "slack-buffer" "slack-buffer.el" (0 0 0 0))
;;; Generated autoloads from slack-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-buffer" '("lui-prompt-string" "slack-")))

;;;***

;;;### (autoloads nil "slack-channel" "slack-channel.el" (0 0 0 0))
;;; Generated autoloads from slack-channel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-channel" '("slack-c")))

;;;***

;;;### (autoloads nil "slack-company" "slack-company.el" (0 0 0 0))
;;; Generated autoloads from slack-company.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-company" '("company-slack-backend")))

;;;***

;;;### (autoloads nil "slack-conversations" "slack-conversations.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-conversations.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-conversations" '("slack-conversations-")))

;;;***

;;;### (autoloads nil "slack-counts" "slack-counts.el" (0 0 0 0))
;;; Generated autoloads from slack-counts.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-counts" '("slack-c")))

;;;***

;;;### (autoloads nil "slack-create-message" "slack-create-message.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-create-message.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-create-message" '("slack-")))

;;;***

;;;### (autoloads nil "slack-dialog" "slack-dialog.el" (0 0 0 0))
;;; Generated autoloads from slack-dialog.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-dialog" '("slack-dialog")))

;;;***

;;;### (autoloads nil "slack-dialog-buffer" "slack-dialog-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-dialog-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-dialog-buffer" '("slack-")))

;;;***

;;;### (autoloads nil "slack-dialog-edit-element-buffer" "slack-dialog-edit-element-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-dialog-edit-element-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-dialog-edit-element-buffer" '("slack-dialog-edit-")))

;;;***

;;;### (autoloads nil "slack-dnd-status" "slack-dnd-status.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from slack-dnd-status.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-dnd-status" '("slack-")))

;;;***

;;;### (autoloads nil "slack-emoji" "slack-emoji.el" (0 0 0 0))
;;; Generated autoloads from slack-emoji.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-emoji" '("slack-")))

;;;***

;;;### (autoloads nil "slack-event" "slack-event.el" (0 0 0 0))
;;; Generated autoloads from slack-event.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-event" '("slack-")))

;;;***

;;;### (autoloads nil "slack-file" "slack-file.el" (0 0 0 0))
;;; Generated autoloads from slack-file.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-file" '("slack-file")))

;;;***

;;;### (autoloads nil "slack-file-info-buffer" "slack-file-info-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-file-info-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-file-info-buffer" '("slack-")))

;;;***

;;;### (autoloads nil "slack-file-list-buffer" "slack-file-list-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-file-list-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-file-list-buffer" '("slack-")))

;;;***

;;;### (autoloads nil "slack-group" "slack-group.el" (0 0 0 0))
;;; Generated autoloads from slack-group.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-group" '("slack-")))

;;;***

;;;### (autoloads nil "slack-im" "slack-im.el" (0 0 0 0))
;;; Generated autoloads from slack-im.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-im" '("slack-im")))

;;;***

;;;### (autoloads nil "slack-image" "slack-image.el" (0 0 0 0))
;;; Generated autoloads from slack-image.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-image" '("slack-")))

;;;***

;;;### (autoloads nil "slack-log" "slack-log.el" (0 0 0 0))
;;; Generated autoloads from slack-log.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-log" '("slack-")))

;;;***

;;;### (autoloads nil "slack-message" "slack-message.el" (0 0 0 0))
;;; Generated autoloads from slack-message.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message" '("slack-")))

;;;***

;;;### (autoloads nil "slack-message-attachment-preview-buffer" "slack-message-attachment-preview-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-message-attachment-preview-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message-attachment-preview-buffer" '("slack-")))

;;;***

;;;### (autoloads nil "slack-message-buffer" "slack-message-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-message-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message-buffer" '("slack-")))

;;;***

;;;### (autoloads nil "slack-message-compose-buffer" "slack-message-compose-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-message-compose-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message-compose-buffer" '("slack-message-")))

;;;***

;;;### (autoloads nil "slack-message-edit-buffer" "slack-message-edit-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-message-edit-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message-edit-buffer" '("slack-")))

;;;***

;;;### (autoloads nil "slack-message-editor" "slack-message-editor.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-message-editor.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message-editor" '("slack-")))

;;;***

;;;### (autoloads nil "slack-message-event" "slack-message-event.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-message-event.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message-event" '("slack-")))

;;;***

;;;### (autoloads nil "slack-message-faces" "slack-message-faces.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-message-faces.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message-faces" '("slack-")))

;;;***

;;;### (autoloads nil "slack-message-formatter" "slack-message-formatter.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-message-formatter.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message-formatter" '("slack-")))

;;;***

;;;### (autoloads nil "slack-message-notification" "slack-message-notification.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-message-notification.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message-notification" '("slack-")))

;;;***

;;;### (autoloads nil "slack-message-reaction" "slack-message-reaction.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-message-reaction.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message-reaction" '("slack-")))

;;;***

;;;### (autoloads nil "slack-message-sender" "slack-message-sender.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-message-sender.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message-sender" '("slack-")))

;;;***

;;;### (autoloads nil "slack-message-share-buffer" "slack-message-share-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-message-share-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message-share-buffer" '("slack-")))

;;;***

;;;### (autoloads nil "slack-modeline" "slack-modeline.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from slack-modeline.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-modeline" '("slack-")))

;;;***

;;;### (autoloads nil "slack-mrkdwn" "slack-mrkdwn.el" (0 0 0 0))
;;; Generated autoloads from slack-mrkdwn.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-mrkdwn" '("slack-mrkdwn-")))

;;;***

;;;### (autoloads nil "slack-pinned-item" "slack-pinned-item.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-pinned-item.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-pinned-item" '("slack-")))

;;;***

;;;### (autoloads nil "slack-pinned-items-buffer" "slack-pinned-items-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-pinned-items-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-pinned-items-buffer" '("slack-")))

;;;***

;;;### (autoloads nil "slack-reaction" "slack-reaction.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from slack-reaction.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-reaction" '("slack-reaction")))

;;;***

;;;### (autoloads nil "slack-reaction-event" "slack-reaction-event.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-reaction-event.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-reaction-event" '("slack-")))

;;;***

;;;### (autoloads nil "slack-reminder" "slack-reminder.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from slack-reminder.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-reminder" '("slack-re")))

;;;***

;;;### (autoloads nil "slack-reply-event" "slack-reply-event.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-reply-event.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-reply-event" '("slack-")))

;;;***

;;;### (autoloads nil "slack-request" "slack-request.el" (0 0 0 0))
;;; Generated autoloads from slack-request.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-request" '("slack-")))

;;;***

;;;### (autoloads nil "slack-room" "slack-room.el" (0 0 0 0))
;;; Generated autoloads from slack-room.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-room" '("slack-room")))

;;;***

;;;### (autoloads nil "slack-room-buffer" "slack-room-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-room-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-room-buffer" '("slack-")))

;;;***

;;;### (autoloads nil "slack-room-event" "slack-room-event.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from slack-room-event.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-room-event" '("slack-")))

;;;***

;;;### (autoloads nil "slack-room-info-buffer" "slack-room-info-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-room-info-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-room-info-buffer" '("slack-room-info-buffer")))

;;;***

;;;### (autoloads nil "slack-room-message-compose-buffer" "slack-room-message-compose-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-room-message-compose-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-room-message-compose-buffer" '("slack-")))

;;;***

;;;### (autoloads nil "slack-search" "slack-search.el" (0 0 0 0))
;;; Generated autoloads from slack-search.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-search" '("slack-")))

;;;***

;;;### (autoloads nil "slack-search-result-buffer" "slack-search-result-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-search-result-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-search-result-buffer" '("slack-")))

;;;***

;;;### (autoloads nil "slack-selectable" "slack-selectable.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from slack-selectable.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-selectable" '("slack-selectable")))

;;;***

;;;### (autoloads nil "slack-slash-commands" "slack-slash-commands.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-slash-commands.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-slash-commands" '("slack-")))

;;;***

;;;### (autoloads nil "slack-star" "slack-star.el" (0 0 0 0))
;;; Generated autoloads from slack-star.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-star" '("slack-")))

;;;***

;;;### (autoloads nil "slack-star-event" "slack-star-event.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from slack-star-event.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-star-event" '("slack-")))

;;;***

;;;### (autoloads nil "slack-stars-buffer" "slack-stars-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-stars-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-stars-buffer" '("slack-")))

;;;***

;;;### (autoloads nil "slack-team" "slack-team.el" (0 0 0 0))
;;; Generated autoloads from slack-team.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-team" '("slack-")))

;;;***

;;;### (autoloads nil "slack-team-ws" "slack-team-ws.el" (0 0 0 0))
;;; Generated autoloads from slack-team-ws.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-team-ws" '("slack-team-ws")))

;;;***

;;;### (autoloads nil "slack-thread" "slack-thread.el" (0 0 0 0))
;;; Generated autoloads from slack-thread.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-thread" '("slack-")))

;;;***

;;;### (autoloads nil "slack-thread-event" "slack-thread-event.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-thread-event.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-thread-event" '("slack-")))

;;;***

;;;### (autoloads nil "slack-thread-message-buffer" "slack-thread-message-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-thread-message-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-thread-message-buffer" '("slack-")))

;;;***

;;;### (autoloads nil "slack-thread-message-compose-buffer" "slack-thread-message-compose-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-thread-message-compose-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-thread-message-compose-buffer" '("slack-")))

;;;***

;;;### (autoloads nil "slack-typing" "slack-typing.el" (0 0 0 0))
;;; Generated autoloads from slack-typing.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-typing" '("slack-typing")))

;;;***

;;;### (autoloads nil "slack-unescape" "slack-unescape.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from slack-unescape.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-unescape" '("slack-")))

;;;***

;;;### (autoloads nil "slack-unread" "slack-unread.el" (0 0 0 0))
;;; Generated autoloads from slack-unread.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-unread" '("slack-")))

;;;***

;;;### (autoloads nil "slack-user" "slack-user.el" (0 0 0 0))
;;; Generated autoloads from slack-user.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-user" '("slack-")))

;;;***

;;;### (autoloads nil "slack-user-message" "slack-user-message.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-user-message.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-user-message" '("slack-")))

;;;***

;;;### (autoloads nil "slack-user-profile-buffer" "slack-user-profile-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-user-profile-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-user-profile-buffer" '("slack-")))

;;;***

;;;### (autoloads nil "slack-usergroup" "slack-usergroup.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from slack-usergroup.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-usergroup" '("slack-usergroup")))

;;;***

;;;### (autoloads nil "slack-util" "slack-util.el" (0 0 0 0))
;;; Generated autoloads from slack-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-util" '("slack-")))

;;;***

;;;### (autoloads nil "slack-websocket" "slack-websocket.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from slack-websocket.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-websocket" '("slack-")))

;;;***

;;;### (autoloads nil nil ("slack-authorize.el" "slack-pkg.el") (0
;;;;;;  0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; slack-autoloads.el ends here
