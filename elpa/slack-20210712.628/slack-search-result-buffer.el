;;; slack-search-result-buffer.el ---                -*- lexical-binding: t; -*-

;; Copyright (C) 2017

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

(require 'eieio)
(require 'slack-util)
(require 'slack-buffer)
(require 'slack-search)

(define-derived-mode slack-search-result-buffer-mode slack-buffer-mode "Slack Search Result"
  (remove-hook 'lui-post-output-hook 'slack-display-image t))

(defclass slack-search-result-buffer (slack-buffer)
  ((search-result :initarg :search-result :type slack-search-result)))

(cl-defmethod slack-buffer-name ((_class (subclass slack-search-result-buffer)) search-result team)
  (with-slots (query sort sort-dir) search-result
    (format "*Slack - %s : %s Search Result - QUERY: %s, ORDER BY: %s %s"
            (oref team name)
            (if (slack-file-search-result-p search-result)
                "File"
              "Message")
            query
            sort
            (upcase sort-dir))))

(cl-defmethod slack-buffer-name ((this slack-search-result-buffer))
  (with-slots (search-result) this
    (with-slots (query sort sort-dir) search-result
      (format "*Slack - %s : %s Search Result - QUERY: %s, ORDER BY: %s %s"
              (slack-team-name (slack-buffer-team this))
              (if (slack-file-search-result-p search-result)
                  "File"
                "Message")
              query
              sort
              (upcase sort-dir)))))

(cl-defmethod slack-buffer-key ((_class (subclass slack-search-result-buffer)) search-result)
  (with-slots (query sort sort-dir) search-result
    (concat query
            ":"
            (if (slack-file-search-result-p search-result)
                "File"
              "Message")
            ":"
            sort
            ":"
            sort-dir)))

(cl-defmethod slack-buffer-key ((this slack-search-result-buffer))
  (slack-buffer-key 'slack-search-result-buffer (oref this search-result)))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-search-result-buffer)))
  'slack-search-result-buffer)

(defun slack-create-search-result-buffer (search-result team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-search-result-buffer team search-result)))
      buffer
    (make-instance 'slack-search-result-buffer
                   :team-id (oref team id)
                   :search-result search-result)))
(cl-defmethod slack-buffer-file-search-result-to-string ((this slack-search-result-buffer) file)
  (let ((title (slack-file-title file))
        (type (slack-file-type file))
        (user-name (slack-user-name (oref file user)
                                    (slack-buffer-team this)))
        (id (oref file id)))
    (format "%s\n%s"
            (slack-file-link-info id title)
            (propertize (format "%s %s" user-name type)
                        'face 'slack-attachment-footer))))

(cl-defmethod slack-buffer-insert ((this slack-search-result-buffer) match)
  (let* ((team (slack-buffer-team this))
         (time (slack-ts-to-time (slack-ts match)))
         (lui-time-stamp-time time)
         (lui-time-stamp-format "[%Y-%m-%d %H:%M] "))
    (if (slack-file-p match)
        (lui-insert (slack-buffer-file-search-result-to-string this match) t)
      (lui-insert (slack-message-to-string match team) t))
    (lui-insert "" t)))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-search-result-buffer))
  (with-slots (search-result) this
    (slack-search-has-next-page-p search-result)))

(cl-defmethod slack-buffer-insert-history ((this slack-search-result-buffer))
  (let* ((search-result (oref this search-result))
         (pagination (oref search-result pagination))
         (first (oref pagination first))
         (last (oref pagination last))
         (matches (last (oref search-result matches) (1+ (- last first))))
         (cur-point (point)))
    (cl-loop for match in matches
             do (slack-buffer-insert this match))
    (goto-char cur-point)))

(cl-defmethod slack-buffer-request-history ((this slack-search-result-buffer) after-success)
  (with-slots (search-result) this
    (slack-search-request search-result after-success (slack-buffer-team this)
                          (slack-search-paging-next-page
                           (oref search-result pagination)))))

(cl-defmethod slack-buffer-init-buffer ((this slack-search-result-buffer))
  (let ((buffer (cl-call-next-method)))
    (with-current-buffer buffer
      (slack-search-result-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (with-slots (search-result) this
        (let* ((messages (oref search-result matches)))
          (cl-loop for m in messages
                   do (slack-buffer-insert this m)))
        (let ((lui-time-stamp-position nil))
          (if (slack-search-has-next-page-p search-result)
              (slack-buffer-insert-load-more this)))))
    buffer))

(cl-defmethod slack-buffer-loading-message-end-point ((_this slack-search-result-buffer))
  (previous-single-property-change (point-max)
                                   'loading-message))

(cl-defmethod slack-buffer-delete-load-more-string ((this slack-search-result-buffer))
  (let* ((inhibit-read-only t)
         (loading-message-end
          (slack-buffer-loading-message-end-point this))
         (loading-message-start
          (previous-single-property-change loading-message-end
                                           'loading-message)))
    (delete-region loading-message-start
                   loading-message-end)))

(cl-defmethod slack-buffer-prepare-marker-for-history ((_this slack-search-result-buffer)))

(cl-defmethod slack-buffer-insert--history ((this slack-search-result-buffer))
  (slack-buffer-insert-history this)
  (if (slack-buffer-has-next-page-p this)
      (slack-buffer-insert-load-more this)
    (let ((lui-time-stamp-position nil))
      (lui-insert "(no more messages)\n" t))))

(defun slack-search-from-messages ()
  (interactive)
  (cl-destructuring-bind (team query sort sort-dir) (slack-search-query-params)
    (let ((instance (make-instance 'slack-search-result
                                   :sort sort
                                   :sort-dir sort-dir
                                   :query query)))
      (cl-labels
          ((after-success ()
                          (let ((buffer (slack-create-search-result-buffer instance team)))
                            (slack-buffer-display buffer))))
        (slack-search-request instance #'after-success team)))))

(defun slack-search-from-files ()
  (interactive)
  (cl-destructuring-bind (team query sort sort-dir) (slack-search-query-params)
    (let ((instance (make-instance 'slack-file-search-result
                                   :sort sort
                                   :sort-dir sort-dir
                                   :query query)))
      (cl-labels
          ((after-success ()
                          (let ((buffer (slack-create-search-result-buffer instance team)))
                            (slack-buffer-display buffer))))
        (slack-search-request instance #'after-success team)))))

(provide 'slack-search-result-buffer)
;;; slack-search-result-buffer.el ends here
