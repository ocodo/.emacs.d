;;; transmission.el --- Interface to a Transmission session -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Version: 0.5
;; Package-Version: 20151011.1502
;; Package-Requires: ((emacs "24.4") (let-alist "1.0.3") (seq "1.5"))
;; Keywords: comm, tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Interface to a Transmission session.

;; Based on the JSON RPC library written by Christopher Wellons,
;; available online here: <https://github.com/skeeto/elisp-json-rpc>

;; Entry points are the `transmission' and `transmission-add'
;; commands.  A variety of commands are available for manipulating
;; torrents and their contents, some of which can be applied over
;; multiple items by selecting them within a region.

;; "M-x transmission RET" pops up a torrent list.  One can add,
;; start/stop, verify, remove torrents, set speed limits, ratio
;; limits, bandwidth priorities, trackers, etc.  Also, one can
;; navigate to either a file list or torrent info context.  In the
;; file list, individual files can be toggled for download, and their
;; priorities set.

;; Customize-able are the session address components, RPC credentials,
;; the display of dates, file sizes and transfer rates, and the
;; refreshing of the torrent list.  See the `transmission'
;; customization group.

;; The design draws from a number of sources, including the
;; "transmission-remote" command line utility and the
;; "transmission-remote-cli" ncurses interface.  These can be found
;; respectively at the following:
;; <https://trac.transmissionbt.com/browser/trunk/daemon/remote.c>
;; <https://github.com/fagga/transmission-remote-cli>

;;; Code:

(require 'calc-bin)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'tabulated-list)

(eval-when-compile
  (require 'let-alist)
  (require 'subr-x))

(defgroup transmission nil
  "Interface to a Transmission session."
  :link '(url-link "https://trac.transmissionbt.com/")
  :group 'external)

(defcustom transmission-host "localhost"
  "Host name or IP address of the Transmission session."
  :type 'string
  :group 'transmission)

(defcustom transmission-service 9091
  "Port or name of the service for the Transmission session."
  :type '(choice (integer :tag "Default" 9091)
                 (string :tag "Service")
                 (integer :tag "Port"))
  :group 'transmission)

(defcustom transmission-rpc-path "/transmission/rpc"
  "Path to the Transmission session RPC interface."
  :type '(choice (string :tag "Default" "/transmission/rpc")
                 (string :tag "Other path"))
  :group 'transmission)

(defcustom transmission-rpc-auth nil
  "Authorization (username, password) for using the RPC interface."
  :type '(choice (const :tag "None" nil)
                 (plist :tag "Username/password"
                        :options ((:username string)
                                  (:password string))))
  :group 'transmission)

(defcustom transmission-units nil
  "The flavor of units used to display file sizes.

See `file-size-human-readable'."
  :type '(choice (const :tag "Default" nil)
                 (symbol :tag "SI" si)
                 (symbol :tag "IEC" iec))
  :link '(function-link file-size-human-readable)
  :group 'transmission)

(defcustom transmission-timer-p nil
  "Transmission buffer refreshes automatically?"
  :type 'boolean
  :group 'transmission)

(defcustom transmission-timer-interval 2
  "Period in seconds of the refresh timer."
  :type '(number :validate (lambda (w)
                             (unless (> (widget-value w) 0)
                               (widget-put w :error "Value must be positive")
                               w)))
  :group 'transmission)

(defcustom transmission-time-format "%a %b %e %T %Y"
  "Format string used to display dates.

See `format-time-string'."
  :type 'string
  :link '(function-link format-time-string)
  :group 'transmission)

(defconst transmission-priority-alist
  '((low . -1)
    (normal . 0)
    (high . 1))
  "Alist of names to priority values.")

(defconst transmission-status-plist
  '(0 "stopped"
    1 "verifywait"
    2 "verifying"
    3 "downwait"
    4 "downloading"
    5 "seedwait"
    6 "seeding")
  "Plist of possible Transmission torrent statuses.")

(defconst transmission-torrent-get-fields
  '("id" "name" "status" "eta"
    "rateDownload" "rateUpload"
    "percentDone" "sizeWhenDone"
    "uploadRatio"))

(defconst transmission-files-fields
  '("name" "files" "fileStats" "downloadDir"))

(defconst transmission-info-fields
  '("name" "hashString" "magnetLink" "activityDate" "addedDate"
    "dateCreated" "doneDate" "startDate" "peers" "pieces" "pieceCount"
    "pieceSize" "trackers" "trackerStats" "peersConnected" "peersGettingFromUs"
    "peersSendingToUs" "sizeWhenDone" "error" "errorString" "wanted" "files"
    "downloadedEver" "corruptEver" "haveValid" "totalSize" "percentDone"
    "seedRatioLimit" "seedRatioMode" "bandwidthPriority"))

(defconst transmission-session-header "X-Transmission-Session-Id"
  "The \"X-Transmission-Session-Id\" header key.")

(defvar transmission-session-id nil
  "The \"X-Transmission-Session-Id\" header value.")

(defvar-local transmission-torrent-vector nil
  "Vector of Transmission torrent data.")

(defvar-local transmission-torrent-id nil
  "The Transmission torrent ID integer.")

(defvar-local transmission-refresh-function nil
  "The name of the function applied to `transmission-draw'.")

(define-error 'transmission-conflict
  "Wrong or missing header \"X-Transmission-Session-Id\"" 'error)

(define-error 'transmission-unauthorized
  "Unauthorized user.  Check `transmission-rpc-auth'" 'error)

(define-error 'transmission-wrong-rpc-path
  "Bad RPC path.  Check `transmission-rpc-path'" 'error)

(defvar transmission-timer nil
  "Timer for repeating `revert-buffer' in a visible Transmission buffer.")


;; JSON RPC

(defun transmission--move-to-content ()
  "Move the point to beginning of content after the headers."
  (setf (point) (point-min))
  (re-search-forward "\r?\n\r?\n" nil t))

(defun transmission--content-finished-p ()
  "Return non-nil if all of the content has arrived."
  (setf (point) (point-min))
  (when (search-forward "Content-Length: " nil t)
    (let ((length (read (current-buffer))))
      (and (transmission--move-to-content)
           (<= length (- (position-bytes (point-max))
                         (position-bytes (point))))))))

(defun transmission--status ()
  "Check the HTTP status code.
A 409 response from a Transmission session includes the
\"X-Transmission-Session-Id\" header.  If a 409 is received,
update `transmission-session-id' and signal the error."
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward "HTTP/")
    (skip-chars-forward "[0-9].")
    (let* ((buffer (current-buffer))
           (status (read buffer)))
      (pcase status
        ((or 301 404 405) (signal 'transmission-wrong-rpc-path status))
        (401 (signal 'transmission-unauthorized status))
        (409 (when (search-forward (format "%s: " transmission-session-header))
               (setq transmission-session-id (read buffer))
               (signal 'transmission-conflict status)))))))

(defun transmission--auth-string ()
  "HTTP \"Authorization\" header value if `transmission-rpc-auth' is populated."
  (when transmission-rpc-auth
    (let ((auth (concat (plist-get transmission-rpc-auth :username) ":"
                        (plist-get transmission-rpc-auth :password))))
      (concat "Basic " (base64-encode-string auth)))))

(defun transmission-http-post (process content)
  "Send to PROCESS an HTTP POST request containing CONTENT."
  (with-current-buffer (process-buffer process)
    (erase-buffer))
  (let ((path transmission-rpc-path)
        (headers (list (cons transmission-session-header transmission-session-id)
                       (cons "Content-length" (string-bytes content)))))
    (let ((auth (transmission--auth-string)))
      (if auth (push (cons "Authorization" auth) headers)))
    (with-temp-buffer
      (insert (format "POST %s HTTP/1.1\r\n" path))
      (mapc (lambda (elt)
              (insert (format "%s: %s\r\n" (car elt) (cdr elt))))
            headers)
      (insert "\r\n")
      (insert content)
      (process-send-string process (buffer-string)))))

(defun transmission-wait (process)
  "Wait to receive HTTP response from PROCESS.
Return JSON object parsed from content."
  (with-current-buffer (process-buffer process)
    (while (and (not (transmission--content-finished-p))
                (process-live-p process))
      (accept-process-output process 1))
    (transmission--status)
    (transmission--move-to-content)
    (json-read)))

(defun transmission-send (process content)
  (transmission-http-post process content)
  (transmission-wait process))

(defun transmission-ensure-process ()
  (let* ((name "transmission")
         (process (get-process name))
         (local (string-prefix-p "/" transmission-host)))
    (if (process-live-p process)
        process
      ;; I believe
      ;; https://trac.transmissionbt.com/ticket/5265
      (make-network-process
       :name name :buffer (format " *%s*" name)
       :host transmission-host
       :service (if local transmission-host transmission-service)
       :family (if local 'local)))))

(defun transmission-request (method &optional arguments tag)
  "Send a request to Transmission.

METHOD is a string.
ARGUMENTS is a plist having keys corresponding to METHOD.
TAG is an integer and ignored.

Details regarding the Transmission RPC can be found here:
<https://trac.transmissionbt.com/browser/trunk/extras/rpc-spec.txt>"
  (let ((process (transmission-ensure-process))
        (content (json-encode `(:method ,method :arguments ,arguments :tag ,tag))))
    (unwind-protect
        (condition-case nil
            (transmission-send process content)
          (transmission-conflict
           (transmission-send process content)))
      (when (process-live-p process)
        (delete-process process)
        (kill-buffer (process-buffer process))))))


;; Response parsing

(defun transmission-torrents (arguments)
  "Return a \"torrents\" vector of objects from a \"torrent-get\" request.
Each object is an alist containing key-value pairs matching the
\"fields\" value in ARGUMENTS."
  (cdr (cadr (assq 'arguments (transmission-request "torrent-get" arguments)))))

(defun transmission-torrent-value (torrent field)
  "Return value in vector TORRENT of key FIELD.
TORRENT is the \"torrents\" vector returned by `transmission-torrents'."
  (cdr (assq field (elt torrent 0))))


;; Timer management

(defun transmission-timer-check ()
  "Check if current buffer should run a refresh timer."
  (let ((buffer (get-buffer "*transmission*")))
    (when (and buffer (eq buffer (current-buffer)))
      (transmission-timer-run))))

(defun transmission-timer-run ()
  (when transmission-timer-p
    (when transmission-timer (cancel-timer transmission-timer))
    (setq
     transmission-timer
     (run-at-time t transmission-timer-interval #'transmission-timer-revert))))

(defun transmission-timer-revert ()
  (let ((buffer (get-buffer "*transmission*")))
    (if (and buffer (eq buffer (current-buffer)))
        (revert-buffer)
      (cancel-timer transmission-timer))))


;; Other

(defun transmission-status (status up down)
  "Return a propertized string describing torrent status.
STATUS is a key of `transmission-status-plist'.  UP and DOWN are
transmission rates."
  (let ((state (plist-get transmission-status-plist status))
        (idle (propertize "idle" 'font-lock-face 'shadow)))
    (pcase status
      (0 (propertize state 'font-lock-face 'warning))
      ((or 1 3 5) (propertize state 'font-lock-face '(bold shadow)))
      (2 (propertize state 'font-lock-face 'font-lock-function-name-face))
      (4 (if (> down 0) (propertize state 'font-lock-face 'highlight) idle))
      (6 (if (> up 0) (propertize state 'font-lock-face 'success) idle))
      (_ state))))

(defun transmission-size (bytes)
  "Return string showing size BYTES in human-readable form."
  (file-size-human-readable bytes transmission-units))

(defun transmission-percent (have total)
  "Return floor of the percentage of HAVE by TOTAL."
  (condition-case nil
      (/ (* 100 have) total)
    (arith-error 0)))

(defun transmission-files-directory-base (filename)
  "Return the top-most parent directory in string FILENAME."
  (let ((index (and (stringp filename)
                    (string-match "/" filename))))
    (if index (substring filename 0 (1+ index)))))

(defun transmission-files-directory-prefix-p (title files)
  "Return t if TITLE is a prefix to every element in FILES, otherwise nil."
  (seq-every-p (lambda (f) (string-prefix-p title (cdr-safe (assq 'name f))))
               files))

(defun transmission-prop-values-in-region (prop)
  "Return a list of truthy values of text property PROP in region or at point.
If none are found, return nil."
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end))
            (list '()))
        (save-excursion
          (goto-char beg)
          (while (> end (point))
            (push (get-text-property (point) prop) list)
            (let ((pos (text-property-not-all (point) end prop (car-safe list))))
              (goto-char (or pos end)))))
        (and (car-safe list) list))
    (let ((value (get-text-property (point) prop)))
      (if value (list value)))))

(defun transmission-eta (seconds percent)
  "Return a string showing SECONDS in human-readable form;
otherwise some other estimate indicated by SECONDS and PERCENT."
  (if (<= seconds 0)
      (pcase percent
        (1 "Done")
        (_ (if (char-displayable-p ?∞) (string ?∞) "Inf")))
    (let* ((minute (float 60))
           (hour (float 3600))
           (day (float 86400))
           (month (* 29.53 day))
           (year (* 365.25 day)))
      (apply #'format "%3.0f%s"
             (pcase seconds
               ((pred (> minute)) (list seconds "s"))
               ((pred (> hour)) (list (/ seconds minute) "m"))
               ((pred (> day)) (list (/ seconds hour) "h"))
               ((pred (> month)) (list (/ seconds day) "d"))
               ((pred (> year)) (list (/ seconds month) "M"))
               (_ (list (/ seconds year) "y")))))))

(defun transmission-rate (bytes)
  "Return a rate in units kilobytes per second.
The rate is calculated from BYTES according to `transmission-units'."
  (/ bytes
     (if (eq 'iec transmission-units) 1024 1000)))

(defun transmission-prompt-speed-limit (upload)
  "Make a prompt to set transfer speed limit.
If UPLOAD is non-nil, make a prompt for upload rate, otherwise
for download rate."
  (let-alist (transmission-request "session-get")
    (let ((limit (if upload .arguments.speed-limit-up
                   .arguments.speed-limit-down))
          (enabled (eq t (if upload .arguments.speed-limit-up-enabled
                           .arguments.speed-limit-down-enabled))))
      (list (read-number (concat "Set global " (if upload "up" "down") "load limit ("
                                 (if enabled (format "%d KB/s" limit) "disabled")
                                 "): "))))))

(defun transmission-prompt-ratio-limit ()
  "Make a prompt to set global seed ratio limit."
  (let-alist (transmission-request "session-get")
    (let ((limit .arguments.seedRatioLimit)
          (enabled (eq t .arguments.seedRatioLimited)))
      (list (read-number (concat "Set global seed ratio limit ("
                                 (if enabled (format "%.1f" limit) "disabled")
                                 "): "))))))

(defun transmission-prompt-read-repeatedly (prompt &optional collection)
  "Read strings until an input is blank, with optional completion.
PROMPT is a string to prompt with.
COLLECTION can be a list among other things.  See `completing-read'.
Returns a list of non-blank inputs."
  (let ((list '())
        entry)
   (catch :finished
     (while t
       (setq entry (if (not collection) (read-string prompt)
                     (let ((completion-cycle-threshold t))
                       (completing-read prompt collection nil t))))
       (if (and (not (string-empty-p entry))
                (not (string-blank-p entry)))
           (push entry list)
         (throw :finished list))))))

(defun transmission-list-trackers (id)
  "Return the \"trackers\" array for torrent id ID."
  (let ((torrent (transmission-torrents `(:ids ,id :fields ("trackers")))))
    (transmission-torrent-value torrent 'trackers)))

(defun transmission-files-do (action)
  "Apply ACTION to files in `transmission-files-mode' buffers."
  (unless (memq action (list :files-wanted :files-unwanted
                             :priority-high :priority-low
                             :priority-normal))
    (error "Invalid field %s" action))
  (let ((id transmission-torrent-id)
        (indices (mapcar (lambda (id) (cdr (assq 'index id)))
                         (transmission-prop-values-in-region 'tabulated-list-id))))
    (if (and id indices)
        (let ((arguments (list :ids id action indices)))
          (transmission-request "torrent-set" arguments))
      (user-error "No files selected or at point"))))

(defun transmission-files-file-at-point ()
  "Return the absolute path of the torrent file at point, or nil.
If the file named \"foo\" does not exist, try \"foo.part\" before returning."
  (let* ((dir (file-name-as-directory
               (transmission-torrent-value transmission-torrent-vector 'downloadDir)))
         (base (cdr (assq 'name (tabulated-list-get-id))))
         (full (and dir base (concat dir base))))
    (or (and (file-exists-p full) full)
        (and (file-exists-p (concat full ".part"))
             (concat full ".part")))))

(defun transmission-files-sort (torrent)
  "Return the .files and .fileStats vectors in TORRENT.
The two are spliced together with indices for each file, sorted by file name."
  (let* ((files (cl-map 'vector #'append
                        (transmission-torrent-value torrent 'files)
                        (transmission-torrent-value torrent 'fileStats)))
         (len (length files))
         (indices (cl-map 'vector (lambda (a b) (list (cons a b)))
                          (make-vector len 'index)
                          (number-sequence 0 len))))
    (cl-sort (cl-map 'vector #'append files indices)
             (lambda (a b)
               (string< (cdr (assq 'name a))
                        (cdr (assq 'name b)))))))

(defun transmission-time (seconds)
  "Format a time string, given SECONDS from the epoch."
  (if (= 0 seconds)
      (format "Never")
    (format-time-string transmission-time-format (seconds-to-time seconds))))

(defun transmission-hamming-weight (x)
  "Calculate the Hamming weight of X."
  (let ((m1 #x555555555555555)
        (m2 #x333333333333333)
        (m4 #x0f0f0f0f0f0f0f0f)
        (h01 #x0101010101010101))
    (setq x (- x (logand (lsh x -1) m1)))
    (setq x (+ (logand x m2) (logand (lsh x -2) m2)))
    (setq x (logand (+ x (lsh x -4)) m4))
    (lsh (* x h01) -56)))

(defun transmission-byte->string (byte)
  "Format integer BYTE into a string."
  (let* ((calc-number-radix 2)
         (string (math-format-radix byte)))
    (concat (make-string (- 8 (length string)) ?0) string)))

(defun transmission-torrent-seed-ratio (tlimit mode)
  "String showing a torrent's seed ratio limit."
  (pcase mode
    (0 "Session limit")
    (1 (format "%d (torrent-specific limit)" tlimit))
    (2 "Unlimited")))

(defmacro transmission-tabulated-list-pred (key)
  "Return a sorting predicate comparing values of KEY.
KEY should be a key in an element of `tabulated-list-entries'."
  (declare (debug t))
  `(lambda (a b)
     (> (cdr (assq ,key (car a)))
        (cdr (assq ,key (car b))))))

(defmacro transmission-let-ids (bindings &rest body)
  "Like `when-let', except call `user-error' if BINDINGS are not truthy.
Execute BODY, binding list `ids' of torrent IDs at point or in region."
  (declare (indent 1) (debug t))
  `(let* ((ids (or (and transmission-torrent-id (list transmission-torrent-id))
                   (mapcar (lambda (id) (cdr (assq 'id id)))
                           (transmission-prop-values-in-region 'tabulated-list-id))))
          ,@bindings)
     (if ids
         (progn ,@body)
       (user-error "No torrent selected"))))


;; Interactive

;;;###autoload
(defun transmission-add (torrent &optional directory)
  "Add TORRENT by filename, URL, magnet link, or info hash.
When called with a prefix, prompt for DIRECTORY."
  (interactive (list (read-file-name "Add torrent: ")
                     (if current-prefix-arg
                         (read-directory-name "Target directory: "))))
  (let ((arguments
         (append (if (file-readable-p torrent)
                     `(:metainfo ,(with-temp-buffer
                                    (insert-file-contents torrent)
                                    (base64-encode-string (buffer-string))))
                   `(:filename ,(if (string-match "\\`[[:xdigit:]]\\{40\\}\\'" torrent)
                                    (format "magnet:?xt=urn:btih:%s" torrent)
                                  torrent)))
                 (list :download-dir directory))))
    (let-alist (transmission-request "torrent-add" arguments)
      (pcase .result
        ("success"
         (or (and .arguments.torrent-added.name
                  (message "Added %s" .arguments.torrent-added.name))
             (and .arguments.torrent-duplicate.name
                  (user-error "Already added %s" .arguments.torrent-duplicate.name))))
        (_ (user-error .result))))))

(defun transmission-move (location)
  "Move torrent at point or in region to a new LOCATION."
  (interactive (list (read-directory-name "New directory: ")))
  (transmission-let-ids ((arguments (list :ids ids :move t
                                          :location (expand-file-name location))))
    (when (y-or-n-p (format "Move torrent%s to %s? "
                            (if (cdr ids) "s" "")
                            location))
     (transmission-request "torrent-set-location" arguments))))

(defun transmission-reannounce ()
  "Reannounce torrent at point or in region."
  (interactive)
  (transmission-let-ids nil
    (transmission-request "torrent-reannounce" (list :ids ids))))

(defun transmission-remove (&optional unlink)
  "Prompt to remove torrent at point or torrents in region.
When called with a prefix UNLINK, also unlink torrent data on disk."
  (interactive "P")
  (transmission-let-ids ((arguments `(:ids ,ids :delete-local-data ,(and unlink t))))
    (when (yes-or-no-p (concat "Remove " (and unlink "and unlink ")
                               "torrent" (and (< 1 (length ids)) "s") "?"))
      (transmission-request "torrent-remove" arguments))))

(defun transmission-set-bandwidth-priority (priority)
  "Set bandwidth PRIORITY of torrent(s) at point or in region."
  (interactive
   (let ((completion-cycle-threshold t)
         (prompt (format "Set bandwidth priority %s: "
                         (mapcar #'car transmission-priority-alist))))
     (list (completing-read prompt transmission-priority-alist nil t))))
  (transmission-let-ids ((number (cdr (assoc-string priority transmission-priority-alist)))
                         (arguments `(:ids ,ids :bandwidthPriority ,number)))
    (transmission-request "torrent-set" arguments)))

(defun transmission-set-download (limit)
  "Set global download speed LIMIT in KB/s."
  (interactive (transmission-prompt-speed-limit nil))
  (let ((arguments (if (<= limit 0) '(:speed-limit-down-enabled :json-false)
                     `(:speed-limit-down-enabled t :speed-limit-down ,limit))))
    (transmission-request "session-set" arguments)))

(defun transmission-set-upload (limit)
  "Set global upload speed LIMIT in KB/s."
  (interactive (transmission-prompt-speed-limit t))
  (let ((arguments (if (<= limit 0) '(:speed-limit-up-enabled :json-false)
                     `(:speed-limit-up-enabled t :speed-limit-up ,limit))))
    (transmission-request "session-set" arguments)))

(defun transmission-set-ratio (limit)
  "Set global seed ratio LIMIT."
  (interactive (transmission-prompt-ratio-limit))
  (let ((arguments (if (<= limit 0) '(:seedRatioLimited :json-false)
                     `(:seedRatioLimited t :seedRatioLimit ,limit))))
    (transmission-request "session-set" arguments)))

(defun transmission-toggle ()
  "Toggle torrent between started and stopped."
  (interactive)
  (transmission-let-ids nil
    (let* ((torrent (transmission-torrents (list :ids ids :fields '("status"))))
           (status (transmission-torrent-value torrent 'status)))
      (pcase status
        (0 (transmission-request "torrent-start" (list :ids ids)))
        ((or 4 6) (transmission-request "torrent-stop" (list :ids ids)))))))

(defun transmission-trackers-add ()
  "Add announce URLs to torrent or torrents."
  (interactive)
  (transmission-let-ids nil
    (let* ((urls (transmission-prompt-read-repeatedly "Add announce URLs: "))
           (trackers (mapcar (lambda (elt) (cdr (assq 'announce elt)))
                             (transmission-list-trackers ids)))
           (arguments (list :ids ids :trackerAdd
                            ;; Don't add trackers that are already there
                            (cl-set-difference urls trackers))))
      (let-alist (transmission-request "torrent-set" arguments)
        (message .result)))))

(defun transmission-trackers-remove ()
  "Prompt for trackers to remove by ID from torrent at point."
  (interactive)
  (let ((id transmission-torrent-id))
    (if id
        (let* ((trackers (mapcar (lambda (elt) (number-to-string (cdr (assq 'id elt))))
                                 (transmission-list-trackers id)))
               (len (length trackers))
               (prompt (concat "Remove tracker by ID"
                               (if (> len 1) (format " (%d trackers): " len) ": ")))
               (tids (transmission-prompt-read-repeatedly prompt trackers))
               (arguments (list :ids id :trackerRemove (mapcar #'string-to-number tids))))
          (let-alist (transmission-request "torrent-set" arguments)
            (pcase .result
              ("success" (message "success!"))
              (_ (user-error .result)))))
      (user-error "No torrent selected"))))

(defun transmission-verify ()
  "Verify torrent at point or in region."
  (interactive)
  (transmission-let-ids nil
    (transmission-request "torrent-verify" (list :ids ids))))

(defun transmission-quit ()
  "Quit and bury the buffer."
  (interactive)
  (let ((cur (current-buffer)))
    (if (cl-some (lambda (b) (not (eq cur b)))
                 (mapcar #'car (window-prev-buffers)))
        (quit-window)
      (if (one-window-p)
          (bury-buffer)
        (delete-window)))))

(defun transmission-files-unwant ()
  "Mark file(s) at point or in region as unwanted."
  (interactive)
  (transmission-files-do :files-unwanted))

(defun transmission-files-want ()
  "Mark file(s) at point or in region as wanted."
  (interactive)
  (transmission-files-do :files-wanted))

(defun transmission-files-priority (priority)
  "Set bandwidth PRIORITY on file(s) at point or in region."
  (interactive
   (let* ((completion-cycle-threshold t)
          (collection '(high low normal))
          (prompt (format "Set priority (%s): "
                          (mapconcat #'symbol-name collection " "))))
     (list (completing-read prompt collection nil t))))
  (when (not (string= priority ""))
    (transmission-files-do (intern (concat ":priority-" priority)))))

(defun transmission-files-command (command file)
  "Run a command COMMAND on the FILE at point."
  (interactive
   (let ((fap (transmission-files-file-at-point)))
     (if (not fap)
         (user-error "File does not exist")
       (list
        (read-shell-command (format "! on %s: " (file-name-nondirectory fap)))
        fap))))
  (let* ((args (nconc (split-string command) (list file)))
         (prog (car args)))
    (apply #'start-process prog nil args)))

(defun transmission-find-file ()
  "Visit the file at point with `find-file-read-only'."
  (interactive)
  (let ((file (transmission-files-file-at-point)))
    (if file
        (find-file-read-only file)
      (user-error "File does not exist"))))


;; Drawing

(defun transmission-tabulated-list-format (&optional _arg _noconfirm)
  "Initialize tabulated-list header or update `tabulated-list-format'."
  (let ((idx (cl-some (lambda (e) (if (plist-get (cdr e) :transmission-size) e))
                      tabulated-list-format)))
    (if (eq (cadr idx) (if (eq 'iec transmission-units) 9 7))
        (or header-line-format (tabulated-list-init-header))
      (setf (cadr idx) (if (eq 'iec transmission-units) 9 7))
      (tabulated-list-init-header))))

(defun transmission-format-pieces (pieces count)
  "Format into a string the bitfield PIECES holding COUNT boolean flags."
  (let* ((bytes (base64-decode-string pieces))
         (bits (mapconcat #'transmission-byte->string bytes "")))
    (mapconcat #'identity (seq-partition (substring bits 0 count) 72) "\n")))

(defun transmission-format-trackers (trackers)
  (let ((fmt (concat "Tracker %d: %s (Tier %d)\n"
                     "\t : %d peers, %d seeders, %d leechers, %d downloads")))
    (mapconcat (lambda (e)
                 (let-alist e
                   (format fmt .id .scrape .tier
                           (if (= -1 .lastAnnouncePeerCount) 0 .lastAnnouncePeerCount)
                           (if (= -1 .seederCount) 0 .seederCount)
                           (if (= -1 .leecherCount) 0 .leecherCount)
                           (if (= -1 .downloadCount) 0 .downloadCount))))
               trackers "\n")))

(defmacro transmission-do-entries (seq &rest body)
  "Map over SEQ, pushing each element to `tabulated-list-entries'.
Each form in BODY is a column descriptor."
  (declare (indent 1) (debug t))
  `(mapc (lambda (elt)
           (let-alist elt
             (push (list elt (vector ,@body)) tabulated-list-entries)))
         ,seq))

(defun transmission-draw-torrents ()
  (setq transmission-torrent-vector
        (transmission-torrents `(:fields ,transmission-torrent-get-fields)))
  (setq tabulated-list-entries nil)
  (transmission-do-entries transmission-torrent-vector
    (transmission-eta .eta .percentDone)
    (transmission-size .sizeWhenDone)
    (format "%3d%%" (* 100 .percentDone))
    (format "%d" (transmission-rate .rateDownload))
    (format "%d" (transmission-rate .rateUpload))
    (format "%.1f" (if (> .uploadRatio 0) .uploadRatio 0))
    (transmission-status .status .rateUpload .rateDownload)
    .name)
  (setq tabulated-list-entries (reverse tabulated-list-entries))
  (tabulated-list-print))

(defun transmission-draw-files (id)
  (setq transmission-torrent-vector
        (transmission-torrents `(:ids ,id :fields ,transmission-files-fields)))
  (let* ((files (transmission-files-sort transmission-torrent-vector))
         (file (cdr (assq 'name (unless (zerop (length files)) (elt files 0)))))
         (directory (transmission-files-directory-base file))
         (truncate (if directory (transmission-files-directory-prefix-p directory files))))
    (setq tabulated-list-entries nil)
    (transmission-do-entries files
      (format "%3d%%" (transmission-percent .bytesCompleted .length))
      (symbol-name (car (rassoc .priority transmission-priority-alist)))
      (pcase .wanted (:json-false "no") (_ "yes"))
      (transmission-size .length)
      (if truncate (string-remove-prefix directory .name) .name)))
  (setq tabulated-list-entries (reverse tabulated-list-entries))
  (tabulated-list-print))

(defun transmission-draw-info (id)
  (setq transmission-torrent-vector
        (transmission-torrents `(:ids ,id :fields ,transmission-info-fields)))
  (erase-buffer)
  (let-alist (elt transmission-torrent-vector 0)
    (mapc
     (lambda (elt) (if elt (insert elt "\n")))
     (vector
      (format "ID: %d" id)
      (concat "Name: " .name)
      (concat "Hash: " .hashString)
      (concat "Magnet: " (propertize .magnetLink 'font-lock-face 'link) "\n")
      (format "Percent done: %d%%" (* 100 .percentDone))
      (format "Bandwidth priority: %s"
              (car (rassoc .bandwidthPriority transmission-priority-alist)))
      (concat "Ratio limit: "
              (transmission-torrent-seed-ratio .seedRatioLimit .seedRatioMode))
      (unless (zerop .error)
        (format "Error: %d %s\n" .error
                (propertize .errorString 'font-lock-face 'error)))
      (format "Peers: connected to %d, uploading to %d, downloading from %d\n"
              .peersConnected .peersGettingFromUs .peersSendingToUs)
      (concat "Date created:    " (transmission-time .dateCreated))
      (concat "Date added:      " (transmission-time .addedDate))
      (concat "Date finished:   " (transmission-time .doneDate))
      (concat "Latest Activity: " (transmission-time .activityDate) "\n")
      (concat (transmission-format-trackers .trackerStats) "\n")
      (let ((wanted (apply #'+ (cl-mapcar (lambda (w f)
                                            (if (not (zerop w))
                                                (cdr (assq 'length f)) 0))
                                          .wanted .files))))
        (format "Wanted: %s (%d bytes)" (transmission-size wanted) wanted))
      (format "Downloaded: %s (%d bytes)" (transmission-size .downloadedEver) .downloadedEver)
      (format "Verified: %s (%d bytes)" (transmission-size .haveValid) .haveValid)
      (unless (zerop .corruptEver)
        (format "Corrupt: %s (%d bytes)" (transmission-size .corruptEver) .corruptEver))
      (format "Total size: %s (%d bytes)" (transmission-size .totalSize) .totalSize)
      (format "Piece size: %s (%d bytes) each" (transmission-size .pieceSize) .pieceSize)
      (let ((have (apply #'+ (mapcar #'transmission-hamming-weight
                                     (base64-decode-string .pieces)))))
        (concat
         (format "Piece count: %d / %d (%d%%)" have .pieceCount
                 (transmission-percent have .pieceCount))
         (when (and (not (= have 0)) (< have .pieceCount))
           (format "\nPieces:\n\n%s\n"
                   (transmission-format-pieces .pieces .pieceCount)))))))))

(defun transmission-draw (fun)
  "Draw the buffer with new contents.
FUN should update the buffer contents."
  (with-silent-modifications
    (funcall fun)))

(defun transmission-refresh (&optional _arg _noconfirm)
  "Refresh the current buffer, restoring window position, point, and mark.
Also run the timer for timer object `transmission-timer'."
  (let* ((old-window-start (window-start))
         (old-column (current-column))
         (old-line (line-number-at-pos))
         (old-mark (when (region-active-p)
                     (let ((beg (region-beginning)))
                       (if (= (window-point) beg) (region-end) beg)))))
    (transmission-draw transmission-refresh-function)
    (goto-char (save-excursion
                 (goto-char (point-min))
                 (forward-line (1- old-line))
                 (point)))
    (move-to-column old-column)
    (setf (window-start) old-window-start)
    (and old-mark (set-mark old-mark)))
  (transmission-timer-run))

(defmacro transmission-context (mode)
  "Switch to a context buffer of mode MODE."
  (let ((name (format "*%s*" (replace-regexp-in-string "-mode\\'" ""
                                                       (symbol-name mode)))))
    `(let ((id (or transmission-torrent-id
                   (cdr (assq 'id (tabulated-list-get-id)))))
           (buffer (or (get-buffer ,name)
                       (generate-new-buffer ,name))))
       (if (not id)
           (user-error "No torrent selected")
         (with-current-buffer buffer
           (let ((old-id (or transmission-torrent-id
                             (cdr (assq 'id (tabulated-list-get-id))))))
             (unless (eq major-mode ',mode)
               (funcall #',mode))
             (if (and old-id (eq old-id id))
                 (transmission-refresh)
               (setq transmission-torrent-id id)
               (transmission-draw transmission-refresh-function)
               (goto-char (point-min)))))
         (switch-to-buffer buffer)))))


;; Major mode definitions

(defvar transmission-info-font-lock-keywords
  `(("^\\(.*?:\\)[[:blank:]]*\\(.*\\)$"
     (1 'font-lock-type-face)
     (2 'font-lock-keyword-face)))
  "Default expressions to highlight in `transmission-info-mode' buffers.")

(defvar transmission-info-mode-map
  (let ((map (copy-keymap special-mode-map)))
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "m" 'transmission-move)
    (define-key map "t" 'transmission-trackers-add)
    (define-key map "T" 'transmission-trackers-remove)
    (define-key map "y" 'transmission-set-bandwidth-priority)
    map)
  "Keymap used in `transmission-info-mode' buffers.")

(easy-menu-define transmission-info-mode-menu transmission-info-mode-map
  "Menu used in `transmission-info-mode' buffers."
  '("Transmission-Info"
    ["Add Tracker URLs" transmission-trackers-add]
    ["Remove Trackers" transmission-trackers-remove]
    ["Move Torrent" transmission-move]
    ["Reannounce Torrent" transmission-reannounce]
    ["Set Bandwidth Priority" transmission-set-bandwidth-priority]
    ["Verify Torrent" transmission-verify]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" quit-window]))

(define-derived-mode transmission-info-mode special-mode "Transmission-Info"
  "Major mode for viewing and manipulating torrent attributes in Transmission.

In addition to any hooks its parent mode might have run, this
mode runs the hook `transmission-info-mode-hook' at mode
initialization.

Key bindings:
\\{transmission-info-mode-map}"
  :group 'transmission
  (setq buffer-undo-list t)
  (setq font-lock-defaults '(transmission-info-font-lock-keywords))
  (setq transmission-refresh-function
        (lambda () (transmission-draw-info transmission-torrent-id)))
  (setq-local revert-buffer-function #'transmission-refresh))

(defun transmission-info ()
  "Open a `transmission-info-mode' buffer for torrent at point."
  (interactive)
  (transmission-context transmission-info-mode))

(defvar transmission-files-mode-map
  (let ((map (copy-keymap tabulated-list-mode-map)))
    (define-key map (kbd "RET") 'transmission-find-file)
    (define-key map "!" 'transmission-files-command)
    (define-key map "i" 'transmission-info)
    (define-key map "m" 'transmission-move)
    (define-key map "u" 'transmission-files-unwant)
    (define-key map "w" 'transmission-files-want)
    (define-key map "y" 'transmission-files-priority)
    map)
  "Keymap used in `transmission-files-mode' buffers.")

(easy-menu-define transmission-files-mode-menu transmission-files-mode-map
  "Menu used in `transmission-files-mode' buffers."
  '("Transmission-Files"
    ["Run Command On File" transmission-files-command]
    ["Visit File" transmission-find-file
     "Switch to a read-only buffer visiting file at point"]
    ["Mark Files Unwanted" transmission-files-unwant]
    ["Mark Files Wanted" transmission-files-want]
    ["Set Files' Bandwidth Priority" transmission-files-priority]
    ["View Torrent Info" transmission-info]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" quit-window]))

(define-derived-mode transmission-files-mode tabulated-list-mode "Transmission-Files"
  "Major mode for interacting with torrent files in Transmission.

In addition to any hooks its parent mode might have run, this
mode runs the hook `transmission-files-mode-hook' at mode
initialization.

Key bindings:
\\{transmission-files-mode-map}"
  :group 'transmission
  (setq-local line-move-visual nil)
  (setq tabulated-list-format
        `[("Have" 4 nil :right-align t)
          ("Priority" 8 t)
          ("Want" 4 t :right-align t)
          ("Size" 9 ,(transmission-tabulated-list-pred 'length)
           :right-align t :transmission-size t)
          ("Name" 0 t)])
  (transmission-tabulated-list-format)
  (setq transmission-refresh-function
        (lambda () (transmission-draw-files transmission-torrent-id)))
  (setq-local revert-buffer-function #'transmission-refresh)
  (add-function :before (local 'revert-buffer-function)
                #'transmission-tabulated-list-format))

(defun transmission-files ()
  "Open a `transmission-files-mode' buffer for torrent at point."
  (interactive)
  (transmission-context transmission-files-mode))

(defvar transmission-mode-map
  (let ((map (copy-keymap tabulated-list-mode-map)))
    (define-key map (kbd "RET") 'transmission-files)
    (define-key map "a" 'transmission-add)
    (define-key map "d" 'transmission-set-download)
    (define-key map "i" 'transmission-info)
    (define-key map "l" 'transmission-set-ratio)
    (define-key map "m" 'transmission-move)
    (define-key map "r" 'transmission-remove)
    (define-key map "s" 'transmission-toggle)
    (define-key map "t" 'transmission-trackers-add)
    (define-key map "u" 'transmission-set-upload)
    (define-key map "v" 'transmission-verify)
    (define-key map "q" 'transmission-quit)
    (define-key map "y" 'transmission-set-bandwidth-priority)
    map)
  "Keymap used in `transmission-mode' buffers.")

(easy-menu-define transmission-mode-menu transmission-mode-map
  "Menu used in `transmission-mode' buffers."
  '("Transmission"
    ["Add Torrent" transmission-add]
    ["Start/Stop Torrent" transmission-toggle
     :help "Toggle pause on torrents at point or in region"]
    ["Set Bandwidth Priority" transmission-set-bandwidth-priority]
    ("Set Limits"
     ["Set Global Download Limit" transmission-set-download]
     ["Set Global Upload Limit" transmission-set-upload]
     ["Set Global Seed Ratio Limit" transmission-set-ratio])
    ["Move Torrent" transmission-move]
    ["Reannounce Torrent" transmission-reannounce]
    ["Verify Torrent" transmission-verify]
    "--"
    ["View Torrent Files" transmission-files]
    ["View Torrent Info" transmission-info]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" transmission-quit]))

(define-derived-mode transmission-mode tabulated-list-mode "Transmission"
  "Major mode for interfacing with a Transmission daemon. See
https://trac.transmissionbt.com/ for more information about
Transmission.

In addition to any hooks its parent mode might have run, this
mode runs the hook `transmission-mode-hook' at mode
initialization.

Key bindings:
\\{transmission-mode-map}"
  :group 'transmission
  (setq-local line-move-visual nil)
  (setq tabulated-list-format
        `[("ETA" 4 ,(transmission-tabulated-list-pred 'eta)
           :right-align t)
          ("Size" 9 ,(transmission-tabulated-list-pred 'sizeWhenDone)
           :right-align t :transmission-size t)
          ("Have" 4 nil :right-align t)
          ("Down" 4 nil :right-align t)
          ("Up" 3 nil :right-align t)
          ("Ratio" 5 nil :right-align t)
          ("Status" 11 t)
          ("Name" 0 t)])
  (transmission-tabulated-list-format)
  (setq transmission-refresh-function #'transmission-draw-torrents)
  (setq-local revert-buffer-function #'transmission-refresh)
  (add-hook 'post-command-hook #'transmission-timer-check nil t)
  (add-function :before (local 'revert-buffer-function)
                #'transmission-tabulated-list-format))

;;;###autoload
(defun transmission ()
  "Open a `transmission-mode' buffer."
  (interactive)
  (let* ((name "*transmission*")
         (buffer (or (get-buffer name)
                     (generate-new-buffer name))))
    (unless (eq buffer (current-buffer))
      (with-current-buffer buffer
        (if (eq major-mode 'transmission-mode)
            (transmission-refresh)
          (transmission-mode)
          (transmission-draw transmission-refresh-function)
          (goto-char (point-min))))
      (switch-to-buffer-other-window buffer))))

(provide 'transmission)

;;; transmission.el ends here
