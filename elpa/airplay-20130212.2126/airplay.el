;;; airplay.el --- Airplay bindings to Emacs

;; Copyright (C) 2013 by Wataru MIYAGUNI

;; Author: Wataru MIYAGUNI <gonngo@gmail.com>
;; URL: https://github.com/gongo/airplay-el
;; Keywords: appletv airplay
;; Version: 0.1.0
;; Package-Requires: ((request "20130110.2144") (simple-httpd "1.4.1") (deferred "0.3.1"))

;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; A client for AirPlay Server.

;;; Code:

(eval-when-compile (require 'cl))
(require 'xml)
(require 'dns)
(require 'request-deferred)
(require 'find-func)

(defvar airplay->host nil)
(defvar airplay->port 7000)

(defvar airplay/video->server-daemon-name "airplay-server")
(defvar airplay/video->server-port 7070)
(defvar airplay/video->server-lisp-name "airplay-video-server.el")
(defvar airplay/video->server-buffer "*airplay-server*")

(defvar airplay/video->playing? nil)

(defconst airplay->log-buffer "*airplay log*")

(defconst airplay/image->transitions
  '(
    :none        "None"
    :slide_left  "SlideLeft"
    :slide_right "SlideRight"
    :dissolve    "Dissolve"
    ))

(defun airplay/debug-log (fmt &rest args)
  (with-current-buffer (get-buffer-create airplay->log-buffer)
    (insert (apply 'format fmt args))))

(defun airplay/device:browse ()
  "Return IP Address and port of _airplay._tcp service type device.
If not found device, return (nil . nil)."
  (with-temp-buffer
    (let ((process (make-network-process :name "mdns"
                                         :coding 'binary
                                         :buffer (current-buffer)
                                         :host "224.0.0.251"
                                         :service 5353
                                         :type 'datagram))
          (send-text (dns-write
                      `((id ,(random 65000))
                        (opcode query)
                        (queries (("_airplay._tcp.local" (type PTR)))))))
          response)
      (process-send-string process send-text)
      (when (zerop (buffer-size))
        (accept-process-output process 5)) ;; wait 5sec
      (setq response (buffer-string))
      (delete-process process)
      (if (zerop (length response)) (cons nil nil)
        (let* ((dns_response (dns-get 'additionals (dns-read response)))
               (address (dns-get 'data (car dns_response)))
               (port (dns-get 'port (dns-get 'data (car (last dns_response))))))
          `(,address . ,port))))))

(defun airplay/device:--available-my-network-list ()
  "Return an alist of link up network interfaces and their network address
excluded \"127.0.0.1\".
wrapped `network-interface-list'"
  (let (name address flag)
    (remove-if
     (lambda (ifs)
       (setq name (car ifs))
       (setq address (format-network-address (cdr ifs) t))
       (setq flag (nth 4 (network-interface-info (car ifs))))
       (when (and (memq 'up flag)
                  (equal "127.0.0.1" address)) t))
     (network-interface-list))))

(defun airplay/device:client-ip ()
  (format-network-address
   (cdr
    (let* ((ifaces (airplay/device:--available-my-network-list))
           (ifaces-vector (apply 'vector ifaces)))
      (elt (shuffle-vector ifaces-vector) 0)))
   t))

(defun airplay/net:request (method path &rest args)
  (let ((request-backend 'url-retrieve))
    (apply 'request-deferred (airplay/net:--make-url path) :type method args)))

(defun airplay/net:--make-query (args)
  (mapconcat
   (lambda (x)
     (concat (url-hexify-string (car x)) "=" (url-hexify-string (cdr x))))
   args "&"))

(defun airplay/net:--make-url (path)
  (unless airplay->host
    (let ((device (airplay/device:browse)))
      (setq airplay->host (car device))
      (setq airplay->port (cdr device))))
  (format "http://%s:%s/%s" airplay->host airplay->port path))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTTP Method                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun airplay/protocol:get (path &rest args)
  (apply 'airplay/net:request "GET" path args))

(defun airplay/protocol:post (path &rest args)
  (apply 'airplay/net:request "POST" path args))

(defun airplay/protocol:put (path &rest args)
  (apply 'airplay/net:request "PUT" path args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Request/Response Content Type Maker/Parser   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun airplay/protocol:make-text-parameters (args)
  (concat
   (mapconcat (lambda (x) (concat (car x) ": " (cdr x))) args "\n")
   "\n"))

(defun airplay/protocol:parse-text-parameters ()
  "\
Parse string in current buffer.
Returns the text/parameters list.

eg.

  (buffer-string)
  ;; => \"duration: 83.124794\\nposition: 14.467000\\n\"

  (airplay/protocol:parse-text-parameters)
  ;; => ((\"duration\" . \"83.124794\") (\"position\" . \"14.467000\"))"
  (let ((params '()))
    (save-excursion
      (goto-char (point-min))
      (perform-replace "\\`\\(?:\\\s-\\|\n\\)+\\|\\(?:\\\s-\\|\n\\)+\\'" "" nil t nil)
      (goto-char (point-min))
      (while (re-search-forward "^\\([^:\\\s ]+\\)[\\\s ]*:[\\\s ]*\\([^\\\s ]+\\)$" nil t)
        (let ((name (match-string 1)) (body (match-string 2)))
          (setq params (append params `((,name . ,body))))
          (forward-line))))
    params))

(defun airplay/protocol:parse-scrub ()
  "\
Parse string in current buffer.
Returns the scrub plist.

eg.

  (buffer-string)
  ;; => \"duration: 83.124794\\nposition: 14.467000\\n\"

  (airplay/protocol:parse-scrub)
  ;; => '(:duration 83.124794 :position 14.467000)"
  (let* ((params (airplay/protocol:parse-text-parameters))
         (position (string-to-number (cdr (assoc "position" params))))
         (duration (string-to-number (cdr (assoc "duration" params)))))
    `(:position ,position :duration ,duration)))

(defun airplay/protocol:parse-plist-xml ()
  "Parse string in current buffer.
Assumes \"Apple//DTD PLIST\" ( http://www.apple.com/DTDs/PropertyList-1.0.dtd ) format.
Returns the XML list."
  (let ((tree (xml-parse-region (point-min) (point-max))))
    (airplay/protocol:--parse-plist-xml
     (assoc 'plist tree))))

(defun airplay/protocol:--parse-plist-xml (top)
  (let ((kvs (xml-node-children (remove-if 'stringp (assoc 'dict top))))
        key value kset vset (dict '()))
    (while kvs
      (setq kset (pop kvs))
      (setq vset (pop kvs))

      (setq key (car (xml-node-children kset)))
      (setq value
            (if (eq (xml-node-name vset) 'array)
                (airplay/protocol:--parse-plist-xml (xml-node-children vset))
              (car (xml-node-children vset))))
      (setq dict (append dict `((,key . ,value)))))
    dict))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Video server                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun airplay/server:boot (video)
  (airplay/server:shutdown)
  (let ((self (concat invocation-directory invocation-name))
        (load-httpd-el (find-library-name "simple-httpd"))
        (load-server-el (concat (file-name-directory (or
                                                      buffer-file-name
                                                      load-file-name))
                                airplay/video->server-lisp-name))
        (port airplay/video->server-port))
    (call-process self nil nil nil
                  "-Q"
                  (concat "--daemon=" airplay/video->server-daemon-name)
                  "-l" load-httpd-el
                  "-l" load-server-el
                  "--eval" (format  "(setq httpd-port %d)" port))
    (server-eval-at airplay/video->server-daemon-name
                    `(airplay/server:start ,video))))

(defun airplay/server:shutdown ()
  (when (server-running-p airplay/video->server-daemon-name)
    (server-eval-at airplay/video->server-daemon-name '(kill-emacs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User API                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun airplay/image:view (image_file &optional transition)
  (let* ((transition (or transition :none))
         (transition_val (or (plist-get airplay/image->transitions transition)
                             (plist-get airplay/image->transitions :none))))
    (airplay/protocol:put
     "photo"
     :headers `(("X-Apple-Transition" . ,transition_val))
     :data (with-temp-buffer
             (insert-file-contents-literally image_file)
             (buffer-string)))))

;;;###autoload
(defun airplay:stop ()
  (interactive)
  (setq airplay/video->playing? nil)
  (airplay/server:shutdown)
  (airplay/protocol:post "stop"))

;;;###autoload
(defun airplay/video:play (video_location)
  (lexical-let ((location video_location))
    (deferred:$
      (airplay:stop)
      (deferred:wait 2000)
      (deferred:nextc it
        (lambda (x)
          (setq airplay/video->playing? t)
          (airplay/protocol:post
           "play"
           :data (airplay/protocol:make-text-parameters
                  `(("Content-Location" . ,(airplay/video:--video-path location))
                    ("Start-Position"   . "0.0"))))))
      (deferred:nextc it
        (lambda (x)
          (airplay/video:--monitoring-buffering)))
      (deferred:nextc it
        (lambda (x)
          (cond ((null x) (message "timeout...") nil)
                ((listp x) (message "error: %s" (nth 2 x)) nil)
                (x (airplay/video:--monitoring-playback)))))
      (deferred:error it
        (lambda (x)
          (airplay/debug-log "%S" x)))
      (deferred:nextc it
        (lambda (x) (airplay:stop))))))

(defun airplay/video:--monitoring-playback (&optional interval)
  "Monitored every INTERVAL of the video during playback.
INTERVAL is 1 second WHEN nil.

Exit the monitor when meet the following requirements.

1. Position is out of range play time
2. `airplay/video->playing?' is nil
3. Throw error"
  (lexical-let ((interval (or interval 1000)))
    (deferred:next
      (deferred:lambda (x)
        (deferred:nextc (airplay/video:scrub)
          (lambda (data)
            (let* ((response (request-response-data data))
                   (position (plist-get response :position))
                   (duration (plist-get response :duration))
                   (err (request-response-error-thrown data)))
              (message "[Test] Progress: %S/%S" position duration)
              (cond ((or (null position)
                         (<= position 0)
                         (>= (+ 1 position) duration)
                         (not airplay/video->playing?))
                     nil)
                    ((and err (not (eq 'http (nth 1 err))))
                     err)
                    (t
                     (deferred:nextc (deferred:wait interval) self))))))))))

(defun airplay/video:--monitoring-buffering (&optional limit interval)
  "Monitored every INTERVAL of the video during buffering.
INTERVAL is 1 second WHEN nil.

Exit the monitor when meet the following requirements.

1. Timeout LIMIT sec (default 30 sec).
2. Position is in play time.
3. `airplay/video->playing?' is t
4. Throw error

* Memo (sorry, only Japanese) *

動画再生開始中に scrub や playback-info のリクエストを送ると
何故か 405 Method Not Allowed が返ってくる場合があるが、
その直後に同じリクエストを送ると問題無いので、そういう仕様なんだろうという予想で
その場合は無視して監視を続行している。
"
  (lexical-let ((timeout? nil)
                (interval (or interval 1000)))
    (let ((limit (or limit 30000))
          (d (deferred:lambda (x)
               (deferred:nextc (airplay/video:scrub)
                 (lambda (data)
                   (let* ((response (request-response-data data))
                          (position (plist-get response :position))
                          (duration (plist-get response :duration))
                          (err (request-response-error-thrown data)))
                     (cond ((or timeout? (not airplay/video->playing?))
                            nil)
                           ((and err (not (eq 'http (nth 1 err))))
                            err)
                           ((or (null position)
                                (<= position 0)
                                (>= position duration))
                            (deferred:nextc (deferred:wait interval) self))
                           (t t))))))))
      (deferred:$
        (deferred:timeout limit nil
          (deferred:next d))
        (deferred:nextc it
          (lambda (x) (setq timeout? t) x))))))

(defun airplay/video:--video-path (location)
  "Return path that can be played of LOCATION.

If LOCATION is specified local file,
set up a streaming server on the local
and returns its address because to play on Apple TV."
  (when (file-exists-p location)
    (airplay/server:boot location)
    (setq location (format "http://%s:%s/"
                           (airplay/device:client-ip)
                           airplay/video->server-port)))
  location)

;;;###autoload
(defun airplay/video:scrub (&optional cb)
  "Retrieve the current playback position."
  (lexical-let ((cb (or cb (lambda (x y)))))
    (airplay/protocol:get
     "scrub"
     :parser 'airplay/protocol:parse-scrub
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (let ((position (plist-get data :position))
                       (duration (plist-get data :duration)))
                   (funcall cb position duration)))))))

;;;###autoload
(defun airplay/video:seek (position)
  (airplay/protocol:post
   "scrub"
   :params `(("position" . ,(number-to-string position)))))

;;;###autoload
(defun airplay/video:info (&optional callback)
  (lexical-let
      ((callback (or callback
                     (lambda (data)
                       (if (null data)
                           (message "Not playing...")
                         (message "Playing now!"))))))
    (airplay/protocol:get
     "playback-info"
     :parser 'airplay/protocol:parse-plist-xml
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (funcall callback data))))))

;;;###autoload
(defun airplay/video:pause ()
  (interactive)
  (airplay/video:--rate "0"))

;;;###autoload
(defun airplay/video:resume ()
  (interactive)
  (airplay/video:--rate "1"))

(defun airplay/video:--rate (value)
  (airplay/protocol:post
   "rate"
   :params `(("value" . ,value))))

(provide 'airplay)

;;; airplay.el ends here
