;;; slack-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "slack" "slack.el" (0 0 0 0))
;;; Generated autoloads from slack.el

(autoload 'slack-start "slack" "\


\(fn &optional TEAM)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack" '("slack-")))

;;;***

;;;### (autoloads nil "slack-bot-message" "slack-bot-message.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-bot-message.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-bot-message" '("slack-")))

;;;***

;;;### (autoloads nil "slack-buffer" "slack-buffer.el" (0 0 0 0))
;;; Generated autoloads from slack-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-buffer" '("slack-" "lui-prompt-string")))

;;;***

;;;### (autoloads nil "slack-channel" "slack-channel.el" (0 0 0 0))
;;; Generated autoloads from slack-channel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-channel" '("slack-")))

;;;***

;;;### (autoloads nil "slack-file" "slack-file.el" (0 0 0 0))
;;; Generated autoloads from slack-file.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-file" '("slack-")))

;;;***

;;;### (autoloads nil "slack-group" "slack-group.el" (0 0 0 0))
;;; Generated autoloads from slack-group.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-group" '("slack-")))

;;;***

;;;### (autoloads nil "slack-im" "slack-im.el" (0 0 0 0))
;;; Generated autoloads from slack-im.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-im" '("slack-")))

;;;***

;;;### (autoloads nil "slack-message" "slack-message.el" (0 0 0 0))
;;; Generated autoloads from slack-message.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message" '("slack-")))

;;;***

;;;### (autoloads nil "slack-message-editor" "slack-message-editor.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-message-editor.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message-editor" '("slack-")))

;;;***

;;;### (autoloads nil "slack-message-formatter" "slack-message-formatter.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-message-formatter.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message-formatter" '("slack-")))

;;;***

;;;### (autoloads nil "slack-message-notification" "slack-message-notification.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-message-notification.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-message-notification" '("slack-message-")))

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

;;;### (autoloads nil "slack-reaction" "slack-reaction.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from slack-reaction.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-reaction" '("slack-reaction")))

;;;***

;;;### (autoloads nil "slack-reminder" "slack-reminder.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from slack-reminder.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-reminder" '("slack-")))

;;;***

;;;### (autoloads nil "slack-reply" "slack-reply.el" (0 0 0 0))
;;; Generated autoloads from slack-reply.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-reply" '("slack-message-")))

;;;***

;;;### (autoloads nil "slack-request" "slack-request.el" (0 0 0 0))
;;; Generated autoloads from slack-request.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-request" '("slack-")))

;;;***

;;;### (autoloads nil "slack-room" "slack-room.el" (0 0 0 0))
;;; Generated autoloads from slack-room.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-room" '("slack-")))

;;;***

;;;### (autoloads nil "slack-search" "slack-search.el" (0 0 0 0))
;;; Generated autoloads from slack-search.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-search" '("slack-")))

;;;***

;;;### (autoloads nil "slack-team" "slack-team.el" (0 0 0 0))
;;; Generated autoloads from slack-team.el

(autoload 'slack-register-team "slack-team" "\
PLIST must contain :name :client-id :client-secret with value.
setting :token will reduce your configuration step.
you will notified when receive message with channel included in subscribed-chennels.
if :default is t and `slack-prefer-current-team' is t, skip selecting team when channels listed.
you can change current-team with `slack-change-current-team'

\(fn &rest PLIST)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-team" '("slack-")))

;;;***

;;;### (autoloads nil "slack-user" "slack-user.el" (0 0 0 0))
;;; Generated autoloads from slack-user.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-user" '("slack-user-")))

;;;***

;;;### (autoloads nil "slack-user-message" "slack-user-message.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from slack-user-message.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-user-message" '("slack-")))

;;;***

;;;### (autoloads nil "slack-util" "slack-util.el" (0 0 0 0))
;;; Generated autoloads from slack-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-util" '("company-slack-backend" "slack-")))

;;;***

;;;### (autoloads nil "slack-websocket" "slack-websocket.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from slack-websocket.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slack-websocket" '("slack-")))

;;;***

;;;### (autoloads nil nil ("slack-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; slack-autoloads.el ends here
