;; -*- lexical-binding: t -*-
;;; butler.el --- Client for Jenkins

;; Copyright © 2012-2013 Ashton Kemerling
;;
;; Author: Ashton Kemerling <ashtonkemerling@gmail.com>
;; URL: http://www.github.com/AshtonKem/Butler.git
;; Version: 0.2.5
;; Keywords: Jenkins, Hudson, CI
;; Package-Requires: ((deferred "0.3.2") (json "1.2") (emacs "24"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides an interface to connect to the Jenkins CI server


;;; Code:
(eval-when-compile (require 'cl))

(require 'json)
(require 'deferred)
(require 'url)
(require 'butler-servers)
(require 'butler-util)

(defcustom butler-auto-refresh t
  "Set to non-nil to auto-refresh the buffer.  When this is non-nil, the butler status buffer is refreshed at regular intervals specified by `butler-auto-refresh-interval'.  The buffer is never refreshed when it is in the background."
  :type 'boolean
  :group 'butler
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (functionp 'butler-manage-refresh-timer)
           (butler-manage-refresh-timer))))

(defcustom butler-auto-refresh-interval 5
  "Specifies the number of seconds to wait between refreshing the butler status buffer.  Setting this to any number less than 1 will be treated as a 1 second interval.  Auto-refresh can be turned on or off with the `butler-auto-refresh' variable.  Refresh can be toggled by pressing 'a' in the butler status buffer."
  :type 'integer
  :group 'butler
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (functionp 'butler-manage-refresh-timer)
           (butler-manage-refresh-timer))))

(defun butler-buffer-name ()
  "*butler-status*")

(defun butler-buffer ()
  (get-buffer-create (butler-buffer-name)))

(defvar butler-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "a") 'butler-toggle-auto-refresh)
    (define-key map (kbd "g") 'butler-refresh)
    (define-key map (kbd "t") 'trigger-butler-job)
    (define-key map (kbd "h") 'hide-butler-job)
    (define-key map (kbd "q") 'butler-quit)
    map))


(define-derived-mode butler-mode fundamental-mode "Butler"
  "A major mode for interacting with various CI servers"
  (use-local-map butler-mode-map)
  (butler-manage-refresh-timer))

(defun butler-toggle-auto-refresh ()
  "Toggles whether the butler status buffer refreshes automatically.  This is driven by the `butler-auto-refresh' and `butler-auto-refresh-interval' variables, which can be customized with M-x customize-group RET butler RET"
  (interactive)
  (setq butler-auto-refresh (not butler-auto-refresh))
  (message (concat "Auto-refresh " (if butler-auto-refresh "enabled." "disabled.")))
  (butler-manage-refresh-timer))

(defun butler-manage-refresh-timer ()
  (cancel-function-timers 'butler-timer-refresh)
  (when (and butler-auto-refresh
             (get-buffer (butler-buffer-name)))
    (butler-start-refresh-timer)))

(defun butler-start-refresh-timer ()
  (run-with-timer (max butler-auto-refresh-interval 1)
                  (max butler-auto-refresh-interval 1)
                  'butler-timer-refresh))

(defun butler-timer-refresh ()
  (with-local-quit
    (let ((buffer (get-buffer (butler-buffer-name))))
      (if buffer
          (when (get-buffer-window buffer) ; only refresh if visible
            (butler-refresh))
        (cancel-function-timers 'butler-timer-refresh))))) ; cancel timer when buffer no longer exists

(defun butler-quit ()
  "Kills the butler status buffer"
  (interactive)
  (kill-buffer (butler-buffer) ))

(defun refresh-butler-status (callback)
  (prepare-servers)
  (let ((count 0))
    (maphash (lambda (_server-name server)
               (let* ((url-request-method "GET")
                      (base-url (gethash 'url server))
                      (auth (gethash 'auth server))
                      (url-request-extra-headers
                       `(("Authorization" . ,auth)
                         ("Content-Type" . "application/json"))))
                 (incf count)
                 (if (not (gethash 'jobs server))
                     (puthash 'jobs (make-hash-table :test #'equal) server))
                           (deferred:$
                             (deferred:url-retrieve (concat
                                                     (if (string= "/" (substring base-url (- (length base-url) 1)))
                                                         (substring base-url 0 (- (length base-url) 1))
                                                       base-url)
                                                     "/api/json?tree=jobs[name,inQueue,color,url,lastBuild[building,duration,estimatedDuration,timestamp,executor[likelyStuck]]]"))
                             (deferred:nextc it
                               (lambda (buf)
                                 (with-current-buffer buf
                                   (goto-char (point-min))
                                   (search-forward "{")
                                   (let* ((data (buffer-substring (- (point) 1) (point-max)))
                                          (parsed (json-read-from-string data)))
                                     (mapc (lambda (job)
                                             (let* ((hash (or (gethash (cdr (assoc 'name job))
                                                                       (gethash 'jobs server))
                                                              (make-hash-table :test #'equal)))
                                                    (last-build (cdr (assoc 'lastBuild job)))
                                                    (executor (cdr (assoc 'likelyStuck last-build))))
                                               (puthash 'color (cdr (assoc 'color job))
                                                        hash)
                                               (puthash 'name (cdr (assoc 'name job))
                                                        hash)
                                               (puthash 'in-queue (equal t (cdr (assoc 'inQueue job)))
                                                        hash)
                                               (puthash 'url (cdr (assoc 'url job))
                                                        hash)
                                               (puthash 'building (equal t (cdr (assoc 'building last-build)))
                                                        hash)
                                               (puthash 'likely-stuck (equal t (cdr (assoc 'likelyStuck executor)))
                                                        hash)
                                               (puthash 'timestamp (cdr (assoc 'timestamp last-build))
                                                        hash)
                                               (puthash 'expected-duration (cdr (assoc 'estimatedDuration last-build))
                                                        hash)
                                               (puthash (cdr (assoc 'name job))
                                                        hash
                                                        (gethash 'jobs server))))
                                           (cdr (assoc 'jobs parsed)))))
                                 (if (= count (hash-table-count butler-hash))
                                     (funcall callback))
                                 (kill-buffer buf))))))
             butler-hash)))


(defun parse-jobs (data)
  (let* ((parsed (json-read-from-string data))
	 (jobs (cdr (assoc 'jobs parsed))))
    jobs))


(defun find-current-job ()
  (with-current-buffer (butler-buffer)
    (condition-case nil
        (let* ((line-start (line-beginning-position))
               (line-end (line-end-position))
               (line (substring-no-properties (buffer-substring line-start line-end))))
          (if (string-match "^    ●\\(    Waiting   \\|              \\| |\\(\\+\\| \\)\\{10\\}| \\)\\(.*\\)$" line)
              (match-string 3 line))))))

(defun find-current-server (job-name)
  (let ((matches nil)
        (distances nil))
    (maphash (lambda (name server)
               (if (gethash job-name (gethash 'jobs server))
                   (push name matches)))
             butler-hash)
    (if (= 1 (length matches))
        (car matches)
      (with-current-buffer (butler-buffer)
        (setq distances
              (mapcar (lambda (name)
                        (let ((location (save-excursion
                                          (search-backward-regexp
                                           (concat "^" name) nil t))))
                          (if location
                              (- (point) location)
                            -1)))
                      matches))
        (let ((current-index 0)
              (best-index nil)
              (best-value nil))
          (mapc (lambda (value)
                  (if (and (> value 0)
                           (or (not best-value)
                               (< value best-value)))
                      (progn
                        (setq best-index current-index)
                        (setq best-value value)))
                  (incf current-index))
                distances)
          (if (integerp best-index)
              (nth best-index matches)))))))



(defun trigger-butler-job ()
  "Starts the job identified by the cursor position"
  (interactive)
  (with-current-buffer (butler-buffer)
    (let* ((job-name (find-current-job))
           (server-name (find-current-server job-name))
           (server (get-server server-name))
           (job (get-job server job-name))
           (url (gethash 'url job))
           (auth (gethash 'auth server))
           (url-request-extra-headers `(("Authorization" . ,auth))))
      (if (and url auth)
          (deferred:$
            (deferred:url-retrieve (concat url "build/"))
            (deferred:nextc it
              (lambda (buf)
                (kill-buffer buf))))))))


(defun hide-butler-job ()
  "Hides the job identified by the cursor position"
  (interactive)
  (with-current-buffer (butler-buffer)
    (let* ((job-name (find-current-job))
           (server-name (find-current-server job-name))
           (server (get-server server-name))
           (job (get-job server job-name)))
      (if job
          (progn (puthash 'hidden t job)
                 (butler-refresh))))))

(defun generate-progress-string (timestamp expected)
  (let* ((current-time (string-to-number (format-time-string "%s")))
         (milliseconds (* current-time 1000))
         (duration (- milliseconds timestamp))
         (percentage (min
                      (/ (float duration)
                         expected)
                      1.0))
         (rounded (floor (* 10 percentage))))
    (concat " |"
            (make-string rounded ?+)
            (make-string (- 10 rounded) ?\ )
            "| ")))



(defun draw-jobs (jobs target-buffer callback)
  (with-current-buffer target-buffer
    (maphash (lambda (name job)
            (let* ((inhibit-read-only t)
                   (color (gethash 'color job))
                   (building (gethash 'building job nil))
                   (likely-stuck (gethash 'likely-stuck job nil))
                   (in-queue (gethash 'in-queue job nil))
                   (timestamp (gethash 'timestamp job))
                   (expected-duration (gethash 'expected-duration job))
                   (hidden (gethash 'hidden job)))
              (unless hidden
                (insert "    ")
                (insert (colorize-dot color) )
                (if building
                    (if likely-stuck
                        (insert (propertize (generate-progress-string timestamp expected-duration)
                                            'face '(:foreground "res")))
                      (insert (generate-progress-string timestamp expected-duration) ))
                  (if in-queue
                      (insert "    Waiting   ")
                    (insert "              ")))
                (insert name)
                (insert "\n"))))
          jobs)
    (funcall callback)))






(defun draw-butler (buffer callback)
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (count 0)
          (total-size (- (hash-table-count butler-hash) 1)))
      (maphash (lambda (_server-name server)
                 (let* ((name (gethash 'name server))
                        (inhibit-read-only t)
                        (address (gethash 'url server))
                        (auth (gethash 'auth server))
                        (jobs (gethash 'jobs server)))
                   (goto-char (point-max))
                   (insert (concat name " (" (url-unhex-string address) "): "))
                   (insert (propertize (concat "auth: "
                                               auth)
                                       'invisible t))
                   (insert "\n")
                   (draw-jobs jobs buffer
                              (if (= count total-size)
                                  callback
                                (lambda ())))
                   (incf count)))
               butler-hash))))


;;;###autoload
(defun butler-status ()
  "Shows the butler status buffer which displays the status of the configured CI server"
  (interactive)
  (butler-refresh)
  (switch-to-buffer (butler-buffer))
  (butler-mode))

(defun butler-refresh ()
  "Refreshes the contents of the butler status buffer from the server"
  (interactive)
  (refresh-butler-status
   (lambda ()
     (let ((target-point nil)
           (target-buffer (generate-new-buffer "temp")))
       (with-current-buffer (butler-buffer)
         (setq target-point (or (point) 0)))
       (draw-butler target-buffer (lambda ()
                                    (let ((results (buffer-string))
                                          (inhibit-read-only t))
                                      (with-current-buffer (butler-buffer)
                                        (erase-buffer)
                                        (insert results)
                                        (goto-char target-point)
                                        (setq buffer-read-only t))
                                      (kill-buffer target-buffer))))))))


(provide 'butler)


;;; butler.el ends here
