;;; jenkins.el --- Minimalistic Jenkins client for Emacs

;; Copyright (C) 2015  Rustem Muslimov

;; Author: Rustem Muslimov <r.muslimov@gmail.com>
;; Keywords: jenkins, convenience
;; Package-Version: 20160903.1556
;; Package-Requires: ((dash "2.12") (emacs "24.3") (json "1.4"))

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

;; To set up, please use "M-x customize-group" to customize the
;; "jenkins" options, or just directly define variables as shown below:
;;
;; (setq jenkins-api-token "<api token can be found on user's configure page>")
;; (setq jenkins-url "<jenkins url>")
;; (setq jenkins-username "<your user name>")
;; (setq jenkins-viewname "<viewname>")

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'json)

(defconst jenkins-buffer-name
  "*jenkins-status*"
  "Name of jenkins buffer.")

(defvar jenkins-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") 'jenkins--call-build-job-from-main-screen)
    (define-key map (kbd "v") 'jenkins--visit-job-from-main-screen)
    (define-key map (kbd "RET") 'jenkins-enter-job)
    map)
  "Jenkins main screen status mode keymap.")

(defvar jenkins-job-view-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "1") 'jenkins-job-details-toggle)
    (define-key keymap (kbd "g") 'jenkins--refresh-job-from-job-screen)
    (define-key keymap (kbd "b") 'jenkins--call-build-job-from-job-screen)
    (define-key keymap (kbd "v") 'jenkins--visit-job-from-job-screen)
    (define-key keymap (kbd "$") 'jenkins--show-console-output-from-job-screen)
    keymap)
  "Jenkins jobs status mode keymap.")

(defgroup jenkins nil
  "Interact with a Jenkins CI server."
  :prefix "jenkins-"
  :group 'jenkins)

;; Set up these variables get proper working jenkins.el
(defcustom jenkins-api-token nil
  "API token on user's configure page."
  :type 'string
  :group 'jenkins)

(defcustom jenkins-url nil
  "Jenkins URL. Example http://jenkins.company.com:80/ "
  :type 'string
  :group 'jenkins)

(defcustom jenkins-hostname nil
  "DEPRECATED. Please use jenkins-url instead."
  :type 'string
  :group 'jenkins)

(defcustom jenkins-username nil
  "Username for Jenkins."
  :type 'string
  :group 'jenkins)

(defcustom jenkins-viewname nil
  "View name."
  :type 'string
  :group 'jenkins)

(defcustom jenkins-colwidth-id 3
  "Id column's width on main view."
  :type 'integer
  :group 'jenkins)

(defcustom jenkins-colwidth-name 35
  "Name column's width on main view."
  :type 'integer
  :group 'jenkins)

(defcustom jenkins-colwidth-last-status 20
  "Status column's width on main view."
  :type 'integer
  :group 'jenkins)

(defun jenkins-list-format ()
  "List of columns for main jenkins jobs screen."
  (apply 'vector
  `(("#" ,jenkins-colwidth-id f :pad-right 2 :right-align t :col-source jenkins--render-indicator)
	("Name" ,jenkins-colwidth-name t :col-source jenkins--render-name)
	("Last success" ,jenkins-colwidth-last-status f :col-source :last-success)
	("Last failed" ,jenkins-colwidth-last-status f :col-source :last-failed))
 ))

(defun get-jenkins-url ()
  "This function is for backward compatibility."
  (or jenkins-url jenkins-hostname))


(defvar *jenkins-jobs-list*
  nil
  "Data retrieved from jenkins for main jenkins screen.")

(defvar jenkins-local-jobname)
(defvar jenkins-local-jobs-shown nil)

(defun jenkins--render-name (item)
  "Render jobname for main jenkins job ITEM screen."
  (let ((jobname (plist-get item :name))
        (progress (plist-get item :progress)))
    (if progress
        (format "%s %s"
                (propertize (format "%s%%" progress) 'font-lock-face 'warning)
                jobname)
      (format "%s" jobname))))

(defun jenkins-jobs-view-url ()
  "Jenkins url for get list of jobs in queue and their summaries."
  (format (concat
           "%s"
           (if jenkins-viewname "view/%s/" jenkins-viewname "")
           "api/json?depth=2&tree=name,jobs[name,"
           "lastSuccessfulBuild[result,timestamp,duration,id],"
           "lastFailedBuild[result,timestamp,duration,id],"
           "lastBuild[result,executor[progress]],"
           "lastCompletedBuild[result]]"
           )
          (get-jenkins-url) jenkins-viewname))

(defun jenkins-job-url (jobname)
  "JOBNAME url in jenkins."
  (format (concat
           "%sjob/%s/"
           "api/json?depth=1&tree=builds"
           "[number,timestamp,result,url,building,"
           "culprits[fullName]]")
          (get-jenkins-url) jobname))

(defun jenkins--setup-variables ()
  "Ask from user required variables if they not defined yet."
  (unless (or jenkins-hostname jenkins-url)
    (setq jenkins-url (read-from-minibuffer "Jenkins URL: ")))
  (unless jenkins-username
    (setq jenkins-username (read-from-minibuffer "Jenkins username: ")))
  (unless jenkins-api-token
    (setq jenkins-api-token (read-from-minibuffer "Jenkins API Token: "))))

;; models

(defun jenkins--make-job (name result progress last-success last-failed)
  "Define regular jenkins job here."
  (list :name name
        :result result
        :progress progress
        :last-success last-success
        :last-failed last-failed))

(defun jenkins--get-proper-face-for-result (result)
  "Simple function returning proper 'face for jenkins RESULT."
  (let ((facemap (list '("SUCCESS" . 'success)
                       '("FAILURE" . 'error)
                       '("ABORTED" . 'warning))))
    (cdr (assoc result facemap))))

(defun jenkins--render-indicator (job)
  "Special indicator for each JOB on main jenkins window."
  (propertize
   "●" 'font-lock-face
   (jenkins--get-proper-face-for-result
    (plist-get job :result))))

(defun jenkins--convert-jobs-to-tabulated-format ()
  "Use global jenkins-jobs-list prepare data from table."
  (--map
   (list
    (plist-get it :name)
    (apply 'vector
           (-map
            (lambda (column)
              (let* ((args (nthcdr 3 column))
                     (col-source (plist-get args :col-source)))
                (if (functionp col-source)
                    (funcall col-source it)
                  (plist-get it col-source))))
            (jenkins-list-format))))
   (mapcar 'cdr *jenkins-jobs-list*)))

;;; actions

(defun jenkins-enter-job (&optional jobindex)
  "Open each job detalization page, using JOBINDEX."
  (interactive)
  (let ((jobindex (or jobindex (tabulated-list-get-id))))
    (jenkins-job-view jobindex)))

(defun jenkins--parse-time-from (time-since timeitems)
  (let* ((timeitem (car timeitems))
         (extracted-time (mod time-since (cdr timeitem)))
         (rest-time (/ (- time-since extracted-time) (cdr timeitem)))
         )
    (if (cdr timeitems)
        (apply 'list
               (list extracted-time (car timeitem))
               (jenkins--parse-time-from rest-time (cdr timeitems)))
      (list (list time-since (car timeitem)))
      )))

(defun jenkins--time-since-to-text (timestamp)
  "Return beautiful string presenting TIMESTAMP since event."
  (let* ((timeitems
          '(("s" . 60) ("m" . 60)
            ("h" . 24) ("d" . 1)))
         (seconds-since (- (float-time) timestamp))
         (time-pairs (jenkins--parse-time-from seconds-since timeitems))
         )
    (mapconcat
     (lambda (values) (apply 'format "%d%s" values))
     (-take 3 (reverse (--filter (not (= (car it) 0)) time-pairs)))
     ":")))

(defun jenkins--refresh-jobs-list ()
  "Force loading reloading jobs from jenkins and return them formatter for table."
  (jenkins-get-jobs-list)
  (jenkins--convert-jobs-to-tabulated-format))

(defun jenkins--get-auth-headers ()
  "Helper function to setup auth header for jenkins url call."
  `(("Content-Type" . "application/x-www-form-urlencoded")
    ("Authorization" .
     ,(concat
       "Basic "
       (base64-encode-string
        (concat jenkins-username ":" jenkins-api-token))))))

(defun jenkins--retrieve-page-as-json (url)
  "Shortcut for jenkins api URL to return valid json."
  (let ((url-request-extra-headers (jenkins--get-auth-headers)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (json-read-from-string (buffer-string)))
    ))

(defun jenkins--extract-time-of-build (x buildname)
  "Helper defun to render timestamps."
  (let ((val (cdr (assoc 'timestamp (assoc buildname x)))))
    (if val (jenkins--time-since-to-text (/ val 1000)) "")))

(defun jenkins-get-jobs-list ()
  "Get list of jobs from jenkins server."
  (setq
   *jenkins-jobs-list*
   (let* ((jobs-url (jenkins-jobs-view-url))
          (raw-data (jenkins--retrieve-page-as-json jobs-url))
          (jobs (cdr (assoc 'jobs raw-data))))
     (--map
      (apply 'list (cdr (assoc 'name it))
             (jenkins--make-job
              (cdr (assoc 'name it))
              (cdr (assoc 'result (assoc 'lastCompletedBuild it)))
              (cdr (assoc 'progress (assoc 'executor (assoc 'lastBuild it))))
              (jenkins--extract-time-of-build it 'lastSuccessfulBuild)
              (jenkins--extract-time-of-build it 'lastFailedBuild)))
      jobs))))

(defun jenkins-get-job-details (jobname)
  "Make to particular JOBNAME call."
  (cl-labels ((retrieve (attr item)
                        (cdr (assoc attr item)))
              (convert-item (item)
                  (list
                   (retrieve 'number item)
                   :author (let ((culprits (cdr (assoc 'culprits values))))
                             (if (> (length culprits) 0)
                                 (cdar (aref culprits 0)) "---"))
                   :url (retrieve 'url item)
                   :timestring (jenkins--time-since-to-text (/ (retrieve 'timestamp item) 1000))
                   :building (retrieve 'building item)
                   :result (retrieve 'result item)))
              (vector-take (N vec)
                (--map
                 (aref vec it)
                 (number-sequence 0 (1- (min  N (length vec)))))))
    (let* (
         (job-url (jenkins-job-url jobname))
         (raw-data (jenkins--retrieve-page-as-json job-url))
         (builds (-map #'convert-item (vector-take 25 (cdar raw-data))))
         (latestSuccessful
          (caar (--filter (equal (plist-get (cdr it) :result) "SUCCESS") builds)))
         (latestFailed
          (caar (--filter (equal (plist-get (cdr it) :result) "FAILURE") builds)))
         (latestFinished
          (caar (--filter (equal (plist-get (cdr it) :building) :json-false) builds)))
         )
    (list :name jobname
          :builds builds
          :latestSuccessful latestSuccessful
          :latestFailed latestFailed
          :latestFinished latestFinished
          ))))

;; helpers
(defun jenkins-visit-jenkins-web-page ()
  "Open main jenkins web page using predefined variables."
  (interactive)
  (browse-url (get-jenkins-url)))

(defun jenkins-visit-job (jobname)
  "Open job's webpage using JOBNAME."
  (interactive)
  (browse-url (format "%s/job/%s/" (get-jenkins-url) jobname)))

(defun jenkins-get-console-output (jobname build)
  "Show the console output for the current job"
  (let ((url-request-extra-headers (jenkins--get-auth-headers))
        (console-buffer (get-buffer-create (format "*jenkins-console-%s-%s*" jobname build)))
        (url (format "%sjob/%s/%s/consoleText" (get-jenkins-url) jobname build)))
    (with-current-buffer console-buffer
      (erase-buffer)
      (with-current-buffer (url-retrieve-synchronously url)
        (copy-to-buffer console-buffer (point-min) (point-max))))
    (pop-to-buffer console-buffer)))

(defun jenkins--visit-job-from-main-screen ()
  "Open browser for current job."
  (interactive)
  (jenkins-visit-job (tabulated-list-get-id)))

(defun jenkins--visit-job-from-job-screen ()
  "Open browser for current job."
  (interactive)
  (jenkins-visit-job jenkins-local-jobname))

(defun jenkins--show-console-output-from-job-screen ()
  "Show the console output for the currently selected build"
  (interactive)
  (let* ((props (text-properties-at (point) (current-buffer)))
         (jenkins-tag (member 'jenkins-build-number props))
         (build-number (and jenkins-tag
                          (cadr jenkins-tag))))
    (if build-number
        (jenkins-get-console-output jenkins-local-jobname build-number)
      (error "Not on a Jenkins build line"))))

;; emacs major mode funcs and variables
(define-derived-mode jenkins-mode tabulated-list-mode "Jenkins"
  "Special mode for jenkins status buffer."
  (setq truncate-lines t)
  (kill-all-local-variables)
  (setq mode-name "Jenkins")
  (setq major-mode 'jenkins-mode)
  (use-local-map jenkins-mode-map)
  (hl-line-mode 1)
  (setq tabulated-list-format (jenkins-list-format))
  (setq tabulated-list-entries 'jenkins--refresh-jobs-list)
  (tabulated-list-init-header)
  (tabulated-list-print))

(define-derived-mode jenkins-job-view-mode special-mode "jenkins-job"
  "Mode for viewing jenkins job details"
  ;; buffer defaults
  (setq-local jenkins-local-jobname jobname))

(defun jenkins-job-render (jobname)
  "Render details buffer for JOBNAME."
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((job (cdr (assoc jobname *jenkins-jobs-list*))))
    (insert
     (jenkins-job-details-screen jobname)
     ))
  (setq buffer-read-only t))

(defun jenkins-job-view (jobname)
  "Open JOBNAME details screen."
  (interactive)
  (setq jenkins-local-jobs-shown t)
  (let ((details-buffer-name (format "*%s details*" jobname)))
    (switch-to-buffer details-buffer-name)
    (jenkins-job-render jobname)
    (jenkins-job-view-mode)))

(defun jenkins-job-details-toggle ()
  "Toggle builds list."
  (interactive)
  (setq-local jenkins-local-jobs-shown (not jenkins-local-jobs-shown))
  (jenkins-job-render jenkins-local-jobname)
  (goto-line 4))

(defun jenkins-job-call-build (jobname)
  "Call jenkins build JOBNAME function."
  (let ((url-request-extra-headers (jenkins--get-auth-headers))
        (url-request-method "POST")
        (build-url (format "%sjob/%s/build" (get-jenkins-url) jobname)))
    (when (y-or-n-p (format "Ready to start %s?" jobname))
      (with-current-buffer (url-retrieve-synchronously build-url)
        (message (format "Building %s job started!" jobname))))))

(defun jenkins--call-build-job-from-main-screen ()
  "Build job from main screen."
  (interactive)
  (jenkins-job-call-build (tabulated-list-get-id)))

(defun jenkins--call-build-job-from-job-screen ()
  "Call building job from job details in jenkins."
  (interactive)
  (jenkins-job-call-build jenkins-local-jobname))

(defun jenkins--refresh-job-from-job-screen ()
  "Refresh the current job"
  (interactive)
  (jenkins-job-render jenkins-local-jobname))

(defun jenkins-job-details-screen (jobname)
  "Jenkins job detailization screen, JOBNAME."
  (let* ((job-details (jenkins-get-job-details jobname))
         (jobname (plist-get job-details :name))
         (builds (plist-get job-details :builds))
         (latest (assoc (plist-get job-details :latestFinished) builds))
         (latest-result (plist-get (cdr latest) :result))
         (latestSuccessful
          (cdr (assoc (plist-get job-details :latestSuccessful) builds)))
         )
    (concat
     (format "Job name:\t%s\n" jobname)
     "Status:\t\t"
     (propertize
      (format "%s\n\n" latest-result)
      'face (jenkins--get-proper-face-for-result latest-result))
     (propertize
      (concat
       (format
        "Latest %s builds: "
        (length builds))
       (propertize ";; (press 1 to toggle)\n" 'font-lock-face 'italic)
       (if jenkins-local-jobs-shown
           (apply 'concat
                  (--map
                   (propertize
                    (format "- Job #%s, %s %s\n"
                            (car it)
                            (plist-get (cdr it) :author)
                            (plist-get (cdr it) :timestring)
                            )
                    'jenkins-build-number
                    (car it)
                    'face
                    (jenkins--get-proper-face-for-result
                     (plist-get (cdr it) :result)
                     ))
                   builds)))))
     "\nBuild now! "
     (propertize ";; (press b to Build)\n" 'font-lock-face 'italic)
     "View job's page "
     (propertize ";; (press v to open browser)\n" 'font-lock-face 'italic)
     )))

;;;###autoload
(defun jenkins ()
  "Initialize jenkins buffer."
  (interactive)
  (jenkins--setup-variables)
  (switch-to-buffer-other-window jenkins-buffer-name)
  (erase-buffer)
  (setq buffer-read-only t)
  (jenkins-mode))


(provide 'jenkins)
;;; jenkins.el ends here
