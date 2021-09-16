;;; slack-room-info-buffer.el ---                    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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
(require 'eieio)
(require 'slack-util)
(require 'slack-room)
(require 'slack-group)
(require 'slack-room-buffer)
(require 'slack-team)
(require 'slack-message-formatter)

(defclass slack-room-info-buffer (slack-room-buffer) ())

(define-derived-mode slack-room-info-buffer-mode
  fundamental-mode
  "Slack Room Info"
  (setq-local default-directory slack-default-directory)
  (setq-local buffer-read-only t))

(cl-defmethod slack-buffer-name ((this slack-room-info-buffer))
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (room-name (slack-room-name room team)))
      (format "*Slack Room Info - %s : %s"
              (slack-team-name team)
              room-name)))

(cl-defmethod slack-buffer-key ((_class (subclass slack-room-info-buffer)) room)
  (oref room id))

(cl-defmethod slack-buffer-key ((this slack-room-info-buffer))
  (slack-buffer-key 'slack-room-info-buffer (slack-buffer-room this)))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-room-info-buffer)))
  'slack-room-info-buffer)

(cl-defmethod slack-buffer-init-buffer ((this slack-room-info-buffer))
  (let* ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-room-info-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (slack-buffer-insert this)
      (goto-char (point-min)))
    buf))

(defface slack-room-info-title-face
  '((t (:weight bold :height 1.5)))
  "Used to room info title."
  :group 'slack)

(defface slack-room-info-title-room-name-face
  '((t (:inherit slack-room-info-title-face :foreground "#FFA000")))
  "Used to room info title."
  :group 'slack)

(defface slack-room-info-section-title-face
  '((t (:weight bold :height 1.2)))
  "Used to room info section title."
  :group 'slack)

(defface slack-room-info-section-label-face
  '((t (:weight bold)))
  "Used to room info section title."
  :group 'slack)

(cl-defmethod slack-buffer-insert ((this slack-room-info-buffer))
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this))
        (inhibit-read-only t))
    (insert (propertize "About "
                        'face 'slack-room-info-title-face))
    (insert (propertize (slack-room-name room team)
                        'face 'slack-room-info-title-room-name-face))
    (insert "\n")
    (insert "\n")
    (insert (propertize "Channel Details"
                        'face 'slack-room-info-section-title-face))
    (insert "\n")

    (slack-buffer-insert-purpose room)
    (slack-buffer-insert-topic room)
    (slack-buffer-insert-created room team)))

(cl-defmethod slack-buffer-insert-created ((room slack-room) _team)
  (with-slots (created) room
    (when created
      (insert (propertize "Created"
                          'face 'slack-room-info-section-label-face))
      (insert ":  ")
      (insert (slack-format-ts
               (number-to-string created))))))

(cl-defmethod slack-buffer-insert-created ((room slack-group) team)
  (cl-call-next-method)
  (with-slots (creator) room
    (when creator
      (let ((user (slack-user--find creator team)))
        (insert (format " by %s" (plist-get user :real_name)))))
    ))

(cl-defmethod slack-buffer-insert-purpose ((room slack-group))
  (with-slots (purpose) room
    (when purpose
      (insert (propertize "Purpose"
                          'face 'slack-room-info-section-label-face))
      (insert ":  ")
      (slack-if-let*
          ((purpose-value (plist-get purpose :value))
           (not-blank-p (and purpose-value
                             (< 0 (length purpose-value)))))
          (insert (format "%s\n" purpose-value))
        (insert (propertize "Set purpose"
                            'face '(:box (:line-width
                                          1
                                          :style
                                          released-button))))
        (insert "\n")))))

(cl-defmethod slack-buffer-insert-purpose ((_room slack-room)))

(cl-defmethod slack-buffer-insert-topic ((room slack-group))
  (with-slots (topic) room
    (when topic
      (insert (propertize "Topic"
                          'face 'slack-room-info-section-label-face))
      (insert ":  ")
      (slack-if-let*
          ((topic-value (plist-get topic :value))
           (not-blank-p (and topic-value
                             (< 0 (length topic-value)))))
          (insert (format "%s\n" topic-value))
        (insert (propertize "Set Topic"
                            'face '(:box (:line-width
                                          1
                                          :style
                                          released-button))))
        (insert "\n")))))

(cl-defmethod slack-buffer-insert-topic ((_room slack-room)))

(cl-defmethod slack-create-room-info-buffer ((room slack-room) team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-room-info-buffer team room)))
      buffer
    (slack-room-info-buffer :room-id (oref room id) :team-id (oref team id))))

(provide 'slack-room-info-buffer)
;;; slack-room-info-buffer.el ends here
