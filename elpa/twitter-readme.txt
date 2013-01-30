;;; Commentary:

;; A Twitter client for emacs that can view your friends timeline and
;; publish new statuses.

;;; Your should add the following to your Emacs configuration file:

;; (autoload 'twitter-get-friends-timeline "twitter" nil t)
;; (autoload 'twitter-status-edit "twitter" nil t)
;; (global-set-key "\C-xt" 'twitter-get-friends-timeline)
;; (add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

;; Tell it your username and password by customizing the group
;; "twitter".

;; You can view the statuses by pressing C-x t. While in the timeline
;; buffer you can press C-c C-s to post a new status or C-c C-r to
;; reply to the status at point. Once the message is finished press
;; C-c C-c to publish.

