;;; slack-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "slack" "slack.el" (23278 48047 0 0))
;;; Generated autoloads from slack.el

(autoload 'slack-start "slack" "\


\(fn &optional TEAM)" t nil)

;;;***

;;;### (autoloads nil "slack-team" "slack-team.el" (23278 48047 0
;;;;;;  0))
;;; Generated autoloads from slack-team.el

(autoload 'slack-register-team "slack-team" "\
PLIST must contain :name :client-id :client-secret with value.
setting :token will reduce your configuration step.
you will notified when receive message with channel included in subscribed-channels.
if :default is t and `slack-prefer-current-team' is t, skip selecting team when channels listed.
you can change current-team with `slack-change-current-team'

\(fn &rest PLIST)" t nil)

;;;***

;;;### (autoloads nil nil ("slack-attachment.el" "slack-bot-message.el"
;;;;;;  "slack-buffer.el" "slack-channel.el" "slack-edit-file-comment-buffer.el"
;;;;;;  "slack-emoji.el" "slack-file-comment-compose-buffer.el" "slack-file-comment.el"
;;;;;;  "slack-file-info-buffer.el" "slack-file-list-buffer.el" "slack-file-share-message.el"
;;;;;;  "slack-file.el" "slack-group.el" "slack-im.el" "slack-message-buffer.el"
;;;;;;  "slack-message-compose-buffer.el" "slack-message-edit-buffer.el"
;;;;;;  "slack-message-editor.el" "slack-message-formatter.el" "slack-message-notification.el"
;;;;;;  "slack-message-reaction.el" "slack-message-sender.el" "slack-message-share-buffer.el"
;;;;;;  "slack-message.el" "slack-pinned-item.el" "slack-pinned-items-buffer.el"
;;;;;;  "slack-pkg.el" "slack-reaction.el" "slack-reminder.el" "slack-reply.el"
;;;;;;  "slack-request-worker.el" "slack-request.el" "slack-room-buffer.el"
;;;;;;  "slack-room-message-compose-buffer.el" "slack-room.el" "slack-search-result-buffer.el"
;;;;;;  "slack-search.el" "slack-slash-commands.el" "slack-star.el"
;;;;;;  "slack-stars-buffer.el" "slack-thread-message-buffer.el"
;;;;;;  "slack-thread-message-compose-buffer.el" "slack-thread.el"
;;;;;;  "slack-user-message.el" "slack-user-profile-buffer.el" "slack-user.el"
;;;;;;  "slack-util.el" "slack-websocket.el") (23278 48047 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; slack-autoloads.el ends here
