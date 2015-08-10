;;; transmission.el --- Interface to a Transmission session -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Version: 0.4.1
;; Package-Version: 20150808.1414
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
(require 'let-alist)
(require 'seq)
(require 'subr-x)

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

(defcustom transmission-file-size-units nil
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
    "peersSendingToUs"))

(defconst transmission-session-header "X-Transmission-Session-Id"
  "The \"X-Transmission-Session-Id\" header key.")

(defvar transmission-session-id nil
  "The \"X-Transmission-Session-Id\" header value.")

(defvar-local transmission-torrent-id nil
  "The Transmission torrent ID integer corresponding to the current buffer.")

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
  "Check the HTTP status code.  A 409 response from a
Transmission session includes the \"X-Transmission-Session-Id\"
header.  If a 409 is received, update `transmission-session-id'
and signal the error."
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
  (with-current-buffer (process-buffer process)
    (erase-buffer))
  (let ((path transmission-rpc-path)
        (headers (list (cons transmission-session-header transmission-session-id)
                       (cons "Content-length" (string-bytes content)))))
    (let ((auth (transmission--auth-string)))
      (if auth (push (cons "Authorization" auth) headers)))
    (with-temp-buffer
      (insert (format "POST %s HTTP/1.1\r\n" path))
      (dolist (elt headers)
        (insert (format "%s: %s\r\n" (car elt) (cdr elt))))
      (insert "\r\n")
      (insert content)
      (process-send-string process (buffer-string)))))

(defun transmission-wait (process)
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
         (process (get-process name)))
    (if (process-live-p process)
        process
      (open-network-stream name (format "*%s" name)
                           transmission-host
                           transmission-service))))

(defun transmission-request (method &optional arguments tag)
  "Send a request to Transmission.

Details regarding the Transmission RPC can be found here:
<https://trac.transmissionbt.com/browser/trunk/extras/rpc-spec.txt>"
  (let ((process (transmission-ensure-process))
        (content (json-encode `(:method ,method :arguments ,arguments :tag ,tag))))
    (unwind-protect
        (condition-case nil
            (transmission-send process content)
          (transmission-conflict
           (transmission-send process content)))
      (when (and process (process-live-p process))
        (delete-process process)
        (kill-buffer (process-buffer process))))))


;; Response parsing

(defun transmission-torrents (arguments)
  "Return the \"torrents\" vector associated with the response
from a \"torrent-get\" request with arguments ARGUMENTS."
  (let* ((request `("torrent-get" ,arguments))
         (response (apply #'transmission-request request)))
    (cdr (cadr (assq 'arguments response)))))

(defun transmission-torrent-value (torrent field)
  "Return value in FIELD of in TORRENT, the \"torrents\" vector
returned by `transmission-torrents'."
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
  (let ((state (plist-get transmission-status-plist status))
        (idle (propertize "idle" 'font-lock-face 'shadow)))
    (pcase status
      (0 (propertize state 'font-lock-face 'warning))
      ((or 1 3 5) (propertize state 'font-lock-face '(bold shadow)))
      (2 (propertize state 'font-lock-face 'font-lock-function-name-face))
      (4 (if (> down 0) (propertize state 'font-lock-face 'highlight) idle))
      (6 (if (> up 0) (propertize state 'font-lock-face 'success) idle))
      (_ state))))

(defun transmission-have-percent (bytes totalbytes)
  (condition-case nil
      (/ (* 100 bytes) totalbytes)
    (arith-error 0)))

(defun transmission-files-directory-base (filename)
  "Returns the top-most parent directory in string FILENAME"
  (let ((index (and (stringp filename)
                    (string-match "/" filename))))
    (if index (substring filename 0 (1+ index)))))

(defun transmission-files-directory-prefix-p (title files)
  (seq-every-p (lambda (f) (string-prefix-p title (cdr-safe (assq 'name f))))
               files))

(defun transmission-prop-values-in-region (prop)
  "Return a list of values taken by text property PROP in region
or at point, otherwise nil."
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
        (_ " Inf"))
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
  "Return a the rate BYTES per second scaled according to
`transmission-file-size-units'."
  (let ((scale (if (eq 'iec transmission-file-size-units) 1024 1000)))
    (/ bytes scale)))

(defun transmission-prompt-speed-limit (upload)
  "Make a prompt to set transfer speed limit.  If UPLOAD is
non-nil, make a prompt for upload rate, otherwise for download
rate."
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
  "Read strings until an input is blank.  Returns inputs in a list."
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
  "Do stuff to files in `transmission-files-mode' buffers."
  (unless (memq action (list :files-wanted :files-unwanted
                             :priority-high :priority-low
                             :priority-normal))
    (error "Invalid field %s" action))
  (let ((id transmission-torrent-id)
        (indices (transmission-prop-values-in-region 'index)))
    (if (and id indices)
        (let ((arguments (list :ids id action indices)))
          (transmission-request "torrent-set" arguments))
      (user-error "No files selected or at point"))))

(defun transmission-files-file-at-point ()
  "Return the absolute path of the torrent file at point, or nil.
If the file named \"foo\" does not exist, try \"foo.part\" before returning."
  (let* ((dir (file-name-as-directory (get-text-property (point) 'dir)))
         (base (get-text-property (point) 'name))
         (full (and dir base (concat dir base))))
    (or (and (file-exists-p full) full)
        (and (file-exists-p (concat full ".part"))
             (concat full ".part")))))

(defun transmission-files-sort (torrent)
  "Return the .files and .fileStats vectors in TORRENT, spliced
together with indices for each file, and sorted by file name."
  (let* ((files (cl-map 'vector #'append
                        (transmission-torrent-value torrent 'files)
                        (transmission-torrent-value torrent 'fileStats)))
         (len (length files))
         (indices (cl-map 'vector (lambda (a b) (list (cons a b)))
                          (make-vector len 'index)
                          (number-sequence 0 len))))
    (seq-sort (lambda (a b)
                (string-lessp (cdr (assq 'name a))
                              (cdr (assq 'name b))))
              (cl-map 'vector #'append files indices))))

(defun transmission-time (seconds)
  (if (= 0 seconds)
      (format "Never")
    (format-time-string transmission-time-format (seconds-to-time seconds))))

(defun transmission-map-byte-to-string (byte)
  "Map integer BYTE to an 8-bit binary representation as a string."
  (let* ((calc-number-radix 2)
         (string (math-format-radix byte)))
    (concat (make-string (- 8 (length string)) ?0) string)))

(defmacro transmission-let-ids (bindings &rest body)
  "Execute BODY, binding list `ids' with `transmission-prop-values-in-region'.
Similar to `when-let', except calls user-error if bindings are not truthy."
  (declare (indent 1) (debug t))
  `(let* ((ids (or (and transmission-torrent-id (list transmission-torrent-id))
                   (transmission-prop-values-in-region 'id)))
          ,@bindings)
     (if ids
         (progn ,@body)
       (user-error "No torrent selected"))))


;; Interactive

(defun transmission-next-torrent ()
  "Skip to the next torrent."
  (interactive)
  (let* ((id (get-text-property (point) 'id))
         (skip (text-property-any (point) (point-max) 'id id)))
    (if (or (eobp)
            (not (setq skip (text-property-not-all skip (point-max)
                                                   'id id))))
        (message "No next torrent")
      (when (not (get-text-property skip 'id))
        (save-excursion
          (goto-char skip)
          (setq skip (text-property-not-all skip (point-max)
                                            'id nil))))
      (if skip (goto-char skip)
        (message "No next torrent")))))

(defun transmission-previous-torrent ()
  "Skip to the previous torrent."
  (interactive)
  (let ((id (get-text-property (point) 'id))
        (start (point))
        (found nil))
    ;; Skip past the current link.
    (while (and (not (bobp))
                (numberp id)
                (eq id (get-text-property (point) 'id)))
      (forward-char -1))
    ;; Find the previous link.
    (while (and (not (bobp))
                (or (not (numberp (get-text-property (point) 'id)))
                    (not (setq found (= id (get-text-property (point) 'id))))))
      (forward-char -1)
      (setq id (get-text-property (point) 'id)))
    (if (not found)
        (progn
          (message "No previous torrent")
          (goto-char start))
      ;; Put point at the start of the link.
      (while (and (not (bobp))
                  (eq id (get-text-property (point) 'id)))
        (forward-char -1))
      (and (not (bobp)) (forward-char 1)))))

;;;###autoload
(defun transmission-add (torrent &optional _arg)
  "Add a torrent by filename, URL, magnet link, or info hash.
When called with a prefix, treat input as a string."
  (interactive (list (if (consp current-prefix-arg)
                         (read-string "Add link: ")
                       (read-file-name "Add file: "))
                     current-prefix-arg))
  (let ((arguments (if (file-readable-p torrent)
                       `(:metainfo ,(with-temp-buffer
                                      (insert-file-contents torrent)
                                      (base64-encode-string (buffer-string))))
                     `(:filename ,(if (string-match "\\`[[:xdigit:]]\\{40\\}\\'" torrent)
                                      (format "magnet:?xt=urn:btih:%s" torrent)
                                    torrent)))))
    (let-alist (transmission-request "torrent-add" arguments)
      (pcase .result
        ("success"
         (or (and .arguments.torrent-added.name
                  (message "Added %s" .arguments.torrent-added.name))
             (and .arguments.torrent-duplicate.name
                  (user-error "Already added %s" .arguments.torrent-duplicate.name))))
        (_ (user-error .result))))))

(defun transmission-reannounce ()
  "Reannounce torrent at point or in region."
  (interactive)
  (transmission-let-ids nil
    (transmission-request "torrent-reannounce" (list :ids ids))))

(defun transmission-remove (&optional unlink)
  "Prompt to remove torrent at point or torrents in region.
When called with a prefix, also unlink torrent data on disk."
  (interactive "P")
  (transmission-let-ids ((arguments `(:ids ,ids :delete-local-data ,(and unlink t))))
    (when (yes-or-no-p (concat "Remove " (and unlink "and unlink ")
                               "torrent" (and (< 1 (length ids)) "s") "?"))
      (transmission-request "torrent-remove" arguments))))

(defun transmission-set-bandwidth-priority (priority)
  "Set bandwidth priority of torrent(s) at point or in region."
  (interactive
   (let ((completion-cycle-threshold t)
         (prompt (format "Set bandwidth priority %s: "
                         (mapcar #'car transmission-priority-alist))))
     (list (completing-read prompt transmission-priority-alist nil t))))
  (transmission-let-ids ((number (cdr (assoc-string priority transmission-priority-alist)))
                         (arguments `(:ids ,ids :bandwidthPriority ,number)))
    (transmission-request "torrent-set" arguments)))

(defun transmission-set-download (limit)
  "Set global download speed limit in KB/s."
  (interactive (transmission-prompt-speed-limit nil))
  (let ((arguments (if (<= limit 0) '(:speed-limit-down-enabled :json-false)
                     `(:speed-limit-down-enabled t :speed-limit-down ,limit))))
    (transmission-request "session-set" arguments)))

(defun transmission-set-upload (limit)
  "Set global upload speed limit in KB/s."
  (interactive (transmission-prompt-speed-limit t))
  (let ((arguments (if (<= limit 0) '(:speed-limit-up-enabled :json-false)
                     `(:speed-limit-up-enabled t :speed-limit-up ,limit))))
    (transmission-request "session-set" arguments)))

(defun transmission-set-ratio (limit)
  "Set global seed ratio limit."
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
           ;; Don't add trackers that are already there
           (arguments (list :ids ids :trackerAdd (seq-difference urls trackers))))
      (let-alist (transmission-request "torrent-set" arguments)
        (message .result)))))

(defun transmission-trackers-remove ()
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
    (if (seq-filter (lambda (b) (not (eq cur b)))
                    (mapcar #'car (window-prev-buffers)))
        (quit-window)
      (if (one-window-p)
          (bury-buffer)
        (delete-window)))))

(defun transmission-files-unwant ()
  (interactive)
  (transmission-files-do :files-unwanted))

(defun transmission-files-want ()
  (interactive)
  (transmission-files-do :files-wanted))

(defun transmission-files-priority (priority)
  (interactive
   (let* ((completion-cycle-threshold t)
          (collection '(high low normal))
          (prompt (format "Set priority (%s): "
                          (mapconcat #'symbol-name collection " "))))
     (list (completing-read prompt collection nil t))))
  (when (not (string= priority ""))
    (transmission-files-do (intern (concat ":priority-" priority)))))

(defun transmission-files-command (command arg)
  "Run a command COMMAND on the file at point."
  (interactive
   (let ((file (transmission-files-file-at-point)))
     (if (not file)
         (user-error "File does not exist.")
       (list
        (read-shell-command (format "! on %s: " (file-name-nondirectory file)))
        file))))
  (let* ((args (nconc (split-string command) (list arg)))
         (prog (car args)))
    (apply #'start-process prog nil args)))


;; Drawing

(defun transmission-format-pieces (pieces count)
  (let* ((bytes (mapcar #'identity (base64-decode-string pieces)))
         (bits (seq-mapcat #'transmission-map-byte-to-string bytes)))
    (mapconcat #'identity (seq-partition (seq-take bits count) 72) "\n")))

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

(defun transmission-insert-entry (vec props)
  (let* ((entry (mapconcat #'identity vec " "))
         (start (point))
         (end (+ start (length entry))))
    (insert entry)
    (add-text-properties start end props)))

(defun transmission-draw-torrents ()
  (let ((torrents (transmission-torrents `(:fields ,transmission-torrent-get-fields))))
    (erase-buffer)
    (seq-doseq (element torrents)
      (let-alist element
        (let ((vec
               (vector
                (format "%-4s" (transmission-eta .eta .percentDone))
                (format (if (eq 'iec transmission-file-size-units) "%9s" "%7s")
                        (file-size-human-readable .sizeWhenDone transmission-file-size-units))
                (format "%3d%%" (* 100 .percentDone))
                (format "%4d" (transmission-rate .rateDownload))
                (format "%3d" (transmission-rate .rateUpload))
                (format "%4.1f" (if (> .uploadRatio 0) .uploadRatio 0))
                (format "%-11s" (transmission-status .status .rateUpload .rateDownload))
                (concat .name "\n"))))
          (transmission-insert-entry vec (list 'id .id)))))))

(defun transmission-draw-files (id)
  (let* ((torrent (transmission-torrents `(:ids ,id :fields ,transmission-files-fields)))
         (files (transmission-files-sort torrent))
         (file (cdr-safe (assq 'name (and (not (seq-empty-p files)) (elt files 0)))))
         (directory (transmission-files-directory-base file))
         (truncate (if directory (transmission-files-directory-prefix-p directory files))))
    (erase-buffer)
    (seq-doseq (element files)
      (let-alist element
        (let ((vec
               (vector
                (format "%3d%%" (transmission-have-percent .bytesCompleted .length))
                (format "%6s" (car (rassoc .priority transmission-priority-alist)))
                (format "%3s" (pcase .wanted (:json-false "no") (_ "yes")))
                (format (if (eq 'iec transmission-file-size-units) "%9s" "%7s")
                        (file-size-human-readable .length transmission-file-size-units))
                (concat (if truncate (string-remove-prefix directory .name) .name) "\n"))))
          (transmission-insert-entry vec (list 'name .name 'index .index)))))
    (add-text-properties (point-min) (point-max) `(dir ,(transmission-torrent-value torrent 'downloadDir)))))

(defun transmission-draw-info (id)
  (let ((torrents (transmission-torrents `(:ids ,id :fields ,transmission-info-fields))))
    (erase-buffer)
    (let-alist (elt torrents 0)
      (let ((vec
             (vector
              (format "ID: %d" id)
              (concat "Name: " .name)
              (concat "Hash: " .hashString)
              (concat "Magnet: " (propertize .magnetLink 'font-lock-face 'link) "\n")
              (format "Peers: connected to %d, uploading to %d, downloading from %d\n"
                      .peersConnected .peersGettingFromUs .peersSendingToUs)
              (concat "Date created:    " (transmission-time .dateCreated))
              (concat "Date added:      " (transmission-time .addedDate))
              (concat "Date finished:   " (transmission-time .doneDate))
              (concat "Latest Activity: " (transmission-time .activityDate) "\n")
              (concat (transmission-format-trackers .trackerStats) "\n")
              (format "Piece count: %d" .pieceCount)
              (format "Piece size: %s (%d bytes) each"
                      (file-size-human-readable .pieceSize transmission-file-size-units)
                      .pieceSize)
              (format "Pieces:\n\n%s\n" (transmission-format-pieces .pieces .pieceCount)))))
        (insert (mapconcat #'identity vec "\n"))))))

(defun transmission-draw (fun)
  "FUN erases the buffer and draws a new one."
  (setq buffer-read-only nil)
  (funcall fun)
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))

(defun transmission-refresh (&optional _arg _noconfirm)
  (let* ((old-window-start (window-start))
         (old-window-point (window-point))
         (old-mark (when (region-active-p)
                     (let ((beg (region-beginning)))
                       (if (= old-window-point beg) (region-end) beg)))))
    (transmission-draw transmission-refresh-function)
    (goto-char old-window-point)
    (setf (window-start) old-window-start)
    (and old-mark (set-mark old-mark)))
  (transmission-timer-run))

(defmacro transmission-context (mode)
  "Switch to a context buffer of mode MODE."
  (let ((name (format "*%s*" (replace-regexp-in-string "-mode\\'" ""
                                                       (symbol-name mode)))))
    `(let ((id (or transmission-torrent-id
                   (get-char-property (point) 'id)))
           (buffer (or (get-buffer ,name)
                       (generate-new-buffer ,name))))
       (if (not id)
           (user-error "No torrent selected")
         (switch-to-buffer buffer)
         (let ((old-id (or transmission-torrent-id
                           (get-text-property (point-min) 'id))))
           (unless (eq major-mode ',mode)
             (funcall #',mode))
           (if (and old-id (eq old-id id))
               (transmission-refresh)
             (setq transmission-torrent-id id)
             (transmission-draw transmission-refresh-function)
             (goto-char (point-min))))))))


;; Major mode definitions

(defvar transmission-map
  (let ((map (copy-keymap special-mode-map)))
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    map)
  "Common keymap used in Transmission mode buffers.")

(defvar transmission-info-font-lock-keywords
  `(("^\\(.*?:\\)[[:blank:]]*\\(.*\\)$"
     (1 'font-lock-type-face)
     (2 'font-lock-keyword-face)))
  "Default expressions to highlight in `transmission-info-mode' buffers.")

(defvar transmission-info-mode-map
  (let ((map (copy-keymap transmission-map)))
    (define-key map "t" 'transmission-trackers-add)
    (define-key map "T" 'transmission-trackers-remove)
    map)
  "Keymap used in `transmission-info-mode' buffers.")

(define-derived-mode transmission-info-mode special-mode "Transmission-Info"
  "Major mode for viewing and manipulating torrent attributes in Transmission.
The hook `transmission-info-mode-hook' is run at mode
initialization.

Key bindings:
\\{transmission-info-mode-map}"
  :group 'transmission
  (buffer-disable-undo)
  (setq-local font-lock-defaults '(transmission-info-font-lock-keywords))
  (setq-local transmission-refresh-function
              (lambda () (transmission-draw-info transmission-torrent-id)))
  (setq-local revert-buffer-function #'transmission-refresh))

(defun transmission-info ()
  "Open a `transmission-info-mode' buffer for torrent at point."
  (interactive)
  (transmission-context transmission-info-mode))

(defvar transmission-files-mode-map
  (let ((map (copy-keymap transmission-map)))
    (define-key map "!" 'transmission-files-command)
    (define-key map "i" 'transmission-info)
    (define-key map "u" 'transmission-files-unwant)
    (define-key map "w" 'transmission-files-want)
    (define-key map "y" 'transmission-files-priority)
    map)
  "Keymap used in `transmission-files-mode' buffers.")

(define-derived-mode transmission-files-mode special-mode "Transmission-Files"
  "Major mode for interacting with torrent files in Transmission.
The hook `transmission-files-mode-hook' is run at mode
initialization.

Key bindings:
\\{transmission-files-mode-map}"
  :group 'transmission
  (buffer-disable-undo)
  (setq-local transmission-refresh-function
              (lambda () (transmission-draw-files transmission-torrent-id)))
  (setq-local revert-buffer-function #'transmission-refresh))

(defun transmission-files ()
  "Open a `transmission-files-mode' buffer for torrent at point."
  (interactive)
  (transmission-context transmission-files-mode))

(defvar transmission-mode-map
  (let ((map (copy-keymap transmission-map)))
    (define-key map (kbd "RET") 'transmission-files)
    (define-key map "\t" 'transmission-next-torrent)
    (define-key map [backtab] 'transmission-previous-torrent)
    (define-key map "\e\t" 'transmission-previous-torrent)
    (define-key map "a" 'transmission-add)
    (define-key map "d" 'transmission-set-download)
    (define-key map "i" 'transmission-info)
    (define-key map "l" 'transmission-set-ratio)
    (define-key map "r" 'transmission-remove)
    (define-key map "s" 'transmission-toggle)
    (define-key map "t" 'transmission-trackers-add)
    (define-key map "u" 'transmission-set-upload)
    (define-key map "v" 'transmission-verify)
    (define-key map "q" 'transmission-quit)
    (define-key map "y" 'transmission-set-bandwidth-priority)
    map)
  "Keymap used in `transmission-mode' buffers.")

(define-derived-mode transmission-mode special-mode "Transmission"
  "Major mode for interfacing with a Transmission daemon. See
https://trac.transmissionbt.com/ for more information about
Transmission.  The hook `transmission-mode-hook' is run at mode
initialization.

Key bindings:
\\{transmission-mode-map}"
  :group 'transmission
  (buffer-disable-undo)
  (setq-local transmission-refresh-function #'transmission-draw-torrents)
  (setq-local revert-buffer-function #'transmission-refresh)
  (add-hook 'post-command-hook #'transmission-timer-check nil 'local))

;;;###autoload
(defun transmission ()
  "Open a `transmission-mode' buffer."
  (interactive)
  (let* ((name "*transmission*")
         (buffer (or (get-buffer name)
                     (generate-new-buffer name))))
    (switch-to-buffer-other-window buffer)
    (if (eq major-mode 'transmission-mode)
        (transmission-refresh)
      (transmission-mode)
      (transmission-draw transmission-refresh-function)
      (goto-char (point-min)))))

(provide 'transmission)

;;; transmission.el ends here
