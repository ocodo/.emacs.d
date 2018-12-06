;;; timp.el --- Multithreading library  -*- lexical-binding: t; -*- 
;;
;; Copyright (C) 2015-2016 Mola-T
;; Author: Mola-T <Mola@molamola.xyz>
;; URL: https://github.com/mola-T/timp
;; Version: 1.3.0
;; Package-Requires: ((emacs "24.4")(cl-lib "0.5")(fifo-class "1.0")(signal "1.0"))
;; Keywords: internal, lisp, processes, tools
;;
;;; License:
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; Timp is a multithreading library.
;; It provides 'threads' which can manage jobs in the background
;; without blocking the main Emacs ui.
;; With timp, you can even run database or server in the background.
;; See https://github.com/mola-T/timp for more information.
;;
;;; code:

(require 'fifo-class)
(require 'signal)
(require 'timp-packet)
(require 'timp-socket)

(defgroup timp nil
  "Group for timp."
  :group 'internal
  :group 'lisp
  :group 'processes
  :group 'tools)

(defcustom timp-limit 100
  "Maximum number of threads."
  :tag "Maximum number of threads."
  :group 'timp)

(defconst timp--proc "thread"
  ;; Process name for thread server.
  "Private variable. Modifying it may cause serious problem.")

(defvar timp--record
  ;; Save a record of threads whether they are occupied or free
  ;; format '((0 . <process0>) (1 . nil) (2 . <process3>)....)
  (mapcar 'list
          (number-sequence 0 (- timp-limit 1)))
  "Private variable. Modifying it may cause serious problem.")

(defvar timp--outbound-connect
  ;; Initialize a connection to outbound of timp-socket
  (and (signal-connect :signal 'timp-socket--outbound-signal
                       :worker 'timp--process-outbound-data)
       'timp--process-outbound-data)
  "Private variable. Modifying it may cause serious problem.")

(defvar timp--inbound-connect
  ;; Initialize a connection to inbound of timp-socket
  (and (signal-connect :signal 'timp-socket--inbound-signal
                       :worker 'timp--process-inbound-data)
       'timp--process-inbound-data)
  "Private variable. Modifying it may cause serious problem.")

(defconst timp--data-listener-proc-name "thread-listener"
  ;; Process name for thread data listener.
  "Private variable. Modifying it may cause serious problem.")

(defvar timp--data-listener nil
  ;; Store the local network subprocess
  ;; The listener is a loacl server
  ;; that used to receive data from child threads.
  "Private variable. Modifying it may cause serious problem.")

(defvar timp--data-port nil
  ;; Port number of the timp--data-listener.
  "Private variable. Modifying it may cause serious problem.")

(defvar timp--data-buffer '(0)
  ;; Buffer for imcomplete received data
  ;; Minimum need to have a list for nconc 
  "Private variable. Modifying it may cause serious problem.")

(defconst timp--large-data-listener-proc-name "thread-Dlistener"
  ;; Process name for thread large data listener.
  "Private variable. Modifying it may cause serious problem.")

(defvar timp--large-data-listener nil
  ;; Store the local network subprocess for large data
  ;; The listener is a loacl server
  ;; that used to receive large data from child threads.
  "Private variable. Modifying it may cause serious problem.")

(defvar timp--large-data-port nil
  ;; Port number of the timp--large-data-listener.
  "Private variable. Modifying it may cause serious problem.")

(defvar timp--large-data-buffer '(0)
  ;; Buffer for imcomplete received data
  ;; Minimum need to have a list for nconc 
  "Private variable. Modifying it may cause serious problem.")

(defsignal timp--large-data-processed)

(defvar timp--large-data-processed-connection
  ;; Initialize a connection so that large data works keep doing
  ;; after a one has been done
  (and (signal-connect :signal 'timp--large-data-processed
                       :worker 'timp--process-next-large-data)
       'timp--process-next-large-data)
  "Private variable. Modifying it may cause serious problem.")

(defcustom timp-debug-buffer-name "*timp log*"
  "The buffer name for echo from thread.
This is for debug purpose."
  :tag "Name for timp debug buffer."
  :group 'timp)

(defcustom timp-debug-p nil
  ;; It should be nil when release!!!
  "Whether messages from thread are printed to buffer."
  :tag "Enable thread debug?"
  :group 'timp)

(defvar timp--debug-print-inbound-packet nil
  "Whether inbound packet is printed to thread log.")

(defvar timp--debug-print-outbound-packet nil
  "Whether outbound packet is printed to thread log.")

(cl-defun timp--debug-print-packet (packet &key inbound)
  ;; Print inbound and outbound data to thread log
  "Private function. Using it may cause serious problem."
  (when (or (and timp--debug-print-inbound-packet inbound)
            (and timp--debug-print-outbound-packet (null inbound)))
    (timp-debug-print
     (format "thread%d~%s~~ %s"
             (timp-packet-get-source packet)
             (or (and inbound "IN") "OUT")
             (prin1-to-string packet)))))

(defcustom timp-kill-emacs-close-thread-delay 5
  "The time waited for threads to quit safely before closing emacs.
Default value is 5 seconds."
  :tag "Time delayed for killing emacs to close threads."
  :group 'timp)

(defsignal timp--kill-emacs-signal
  ;; Singal to be emitted after kill-emacs has been invoked.
  ;; More accurately, kill-emacs is adviced around by timp--kill-emacs
  ;; and the signal is emitted by timp--kill-emacs.
  "Private signal. Modifying it may cause serious problem.")

(defsubst timp--push-to-outbound-buffer (thread)
  ;; Push the thread to outbound buffer.
  ;; These function invokes iff thread fails to send out job for one time.
  (timp-set-quene thread t)
  (timp-add-to-socket-buffer thread))

(defsubst timp--push-to-outbound (thread)
  ;; Push thread to outbound of the timp-socket
  ;; Make the setQuene and timp-socket-outbound-push atomic
  "Private function. Using it may cause serious problem."
  (timp-set-quene thread t)
  (timp-socket-outbound-push thread))

(defsubst timp--pop-from-outbound ()
  ;; Pop from outbound of timp-socket and return the thread.
  ;; Make the setQuene and timp-socket-outbound-pop atomic
  "Private function. Using it may cause serious problem."
  (let ((thread (timp-socket-outbound-pop)))
    (timp-set-quene thread nil)
    thread))


(cl-defun timp-get (&key name quit-warn persist)

  "Create a new thread and the thread is returned.
NAME specified the name of the thread.
It does nothing but to let you identitfy the thread when calling
`list-process' of `process-list'.
PERSIST stated whether the thread should be persisted.
If it is nil, after a single instruction,
the thread quits automatically.
If it is t, the thread persists and you are responsible
for quiting the thread either by `timp-quit'(better)
or `timp-force-quit'."
  
  ;; Start the local listener
  (unless (and timp--data-listener (process-live-p (get-process timp--data-listener)))
    (unless (setq timp--data-listener
                  (make-network-process :name timp--data-listener-proc-name
                                        :host 'local
                                        :server 10
                                        :service t
                                        :family 'ipv4
                                        :filter 'timp--listener-receive-data))
      (error "Fail to create a data listener thread.")))
  
  ;; Wait until timp--data-listener ready
  (while (null (eq  (process-status timp--data-listener) 'listen)) nil)

  ;; Start the large data listener
  (unless (and timp--large-data-listener (process-live-p (get-process timp--large-data-listener)))
    (unless (setq timp--large-data-listener
                  (make-network-process :name timp--large-data-listener-proc-name
                                        :host 'local
                                        :server t
                                        :service t
                                        :family 'ipv4
                                        :filter 'timp--LDlistener-receive-data))
      (error "Fail to create a large data listener thread.")))
  
  ;; Wait until listeners ready
  (while (null (eq  (process-status timp--data-listener) 'listen)) nil)
  (while (null (eq  (process-status timp--large-data-listener) 'listen)) nil)
  (setq timp--data-port (process-contact timp--data-listener :service))
  (setq timp--large-data-port (process-contact timp--large-data-listener :service))

  (catch 'thread-exceed-limit
    (let* ((thread-num (car (rassoc nil timp--record)))
           thread-name)

      (if thread-num
          (setq thread-name (concat timp--proc
                                    (format "%04d" thread-num)
                                    (and name (concat " - " name))))
        (throw 'thread-exceed-limit nil))

      (unless (process-live-p (get-process thread-name))
        
        ;; Creating the thread
        (start-process thread-name
                       nil
                       (file-truename
                        (expand-file-name invocation-name
                                          invocation-directory))
                       "-Q" "-batch"
                       "-l" (locate-library "signal")
                       "-l" (locate-library "timp-packet")
                       "-l" (locate-library "timp-server")
                       "-f" "timp-server-init")
        
        (when (process-live-p (get-process thread-name))        
          ;; Send the thread name and local port by stdout
          (process-send-string thread-name
                               (concat (prin1-to-string (list thread-num timp--data-port timp--large-data-port)) "\n"))
          
          (let ((thread (make-instance 'timp
                                       :id thread-num
                                       :process (get-process thread-name)
                                       :persist persist
                                       :quit-warn quit-warn)))
            
            ;; Register the thread to the timp--record
            (setf (cdr (assoc thread-num timp--record)) thread)
            thread))))))

(defun timp--listener-receive-data (_proc data)
  
  ;; Data will arrived as string.
  ;; Large data will be split into small data chunks at parent process.
  ;; A newline charater "\n" indicates the end of the chunks.
  ;; One chunk is sent at a time.
  ;; Depends on OS, the max. data size for a data chunk is fixed, say 4kb for my PC.
  ;; So data chunk is put in timp--data-buffer first.
  ;; And combine to form a complete data when the newline character is met.
  "Private function. Using it may cause serious problem."
  
  (if (string-match "\n" data (- (length data) 1))
      (progn
        (nconc timp--data-buffer (list data))
        (timp-socket-inbound-push timp--data-buffer)
        (setq timp--data-buffer (list 0))) ;; Need to use (list 0) instead of '(0)
    (nconc timp--data-buffer (list data))))

(defun timp--LDlistener-receive-data (_proc data)
  
  ;; It is same as timp--listener-receive-data expect
  ;; it only process large data (>4kb).
  "Private function. Using it may cause serious problem."
  (if (string-match "\n" data (- (length data) 1))
      (progn
        (nconc timp--large-data-buffer (list data))
        (timp-socket-inbound-push timp--large-data-buffer) 
        (setq timp--large-data-buffer (list 0)) ;; Need to use (list 0) instead of '(0)
        ;; Emit a singal to process next large data
        (signal-emit 'timp--large-data-processed))
    (nconc timp--large-data-buffer (list data))))


(defun timp--process-next-large-data ()

  ;; If there is large data permission request quening up
  ;; Give permission to the next thread.
  "Private function. Using it may cause serious problem."

  ;; This large data has been processed, pop it.
  (timp-socket-large-data-pop)
  
  (when (timp-socket-large-data-has-next)
    (let* ((thread (timp-socket-large-data-first))
           (sender (timp-get-sender thread))
           packet)
      
      (unless (and sender (process-live-p sender))
        (setq sender (open-network-stream (concat "thread" (number-to-string (timp-get-id thread)) " - sender")
                                          nil
                                          "localhost"
                                          (timp-get-port thread)
                                          'plain)))
      
      (setq packet (make-instance 'timp-packet
                                  :source (timp-get-id thread)
                                  :type 'ldr
                                  :data t))
      
      
      (process-send-string sender (concat (prin1-to-string packet) "\n"))
      
      (timp--debug-print-packet packet :inbound nil)
      
      (if (timp-has-next-job-p thread)
          (timp-set-sender thread sender)
        (ignore-errors (delete-process sender))
        (timp-set-sender thread nil)))))


(defun timp--process-inbound-data ()

  ;; Process a complete inbound data from timp-socket.
  ;; Distribute the job to responsible functions.
  "Private function. Using it may cause serious problem."
  
  (let* ((job (timp-socket-inbound-pop))
         (string (mapconcat 'identity (cdr job) ""))
         packet
         thread
         packet-type)
    
    ;; Cut the last newline char
    (setq string (substring string 0 (- (length string) 1)))
    (setq packet (read string))
    
    (when (timp-packet-p packet)
      (setq thread (cdr
                    (assq (timp-packet-get-source packet) timp--record)))

      ;; debug
      (timp--debug-print-packet packet :inbound t)
      
      (setq packet-type (timp-packet-get-type packet))
      (when (and thread (process-live-p (timp-get-process thread)))
        ;; Distribute jobs
        (cond
         ((eq packet-type 'port)
          (timp--port-packet-handler thread packet))
         ((eq packet-type 'err)
          (timp--err-packet-handler thread packet))
         ((eq packet-type 'msg)
          (timp--msg-packet-handler thread packet))
         ((eq packet-type 'quit)
          (timp--quit-packet-handler thread))
         ((eq packet-type 'rpy)
          (timp--rpy-packet-handler thread packet))
         ((eq packet-type 'tgi)
          (timp--tgi-packet-handler thread packet))
         ((eq packet-type 'ldr)
          (timp--ldr-packet-handler thread packet)))))))

(defun timp--port-packet-handler (thread packet)
  
  ;; Handling replied packet which is of port type.
  ;; Set port number of thread.
  "Private function. Using it may cause serious problem."
  (let ((port (timp-packet-get-data packet)))
    (timp-set-port thread port)
    (timp--do-next-after-process-job thread t)))

(defun timp--err-packet-handler (thread packet)

  ;; Handling replied packet which is of err type.
  ;; Calling appropiate function to handle error.
  "Private function. Using it may cause serious problem."
  
  (let ((error-handler (timp-packet-get-error-handler packet))
        (arg (timp-packet-get-data packet)))
    
    (when error-handler
      (ignore-errors (apply error-handler arg))))
  (timp--do-next-after-process-job thread))

(defun timp--msg-packet-handler (thread packet)
  
  ;; Handling replied packet which is of msg type.
  ;; Output message to *thread log* if it is in debug mode.
  ;; Output message directly if it is not in debug mode.
  "Private function. Using it may cause serious problem."
  
  (let ((data (timp-packet-get-data packet)))
    (when timp-debug-p
      (timp-debug-print (format "thread%d~ %s\n" (timp-get-id thread) data)))
    (message data)))

(defun timp--quit-packet-handler (thread)

  ;; Handling replied packet which is of quit type.
  ;; Kill the process associated with THREAD.
  ;; It can also use to force quit a thread.
  "Private function. Using it may cause serious problem."
  
  (ignore-errors (delete-process (timp-get-sender thread)))
  (ignore-errors (delete-process (timp-get-process thread)))
  (setf (cdr (assoc (timp-get-id thread) timp--record)) nil))

(defun timp--rpy-packet-handler (thread packet)

  ;; Handling replied packet which is of msg type.
  ;; Calling appropiate function to handle the reply.
  "Private function. Using it may cause serious problem."

  (let ((reply-func (timp-packet-get-reply packet))
        (arg (timp-packet-get-data packet)))
    (when reply-func
      (ignore-errors (apply reply-func arg))))
  (timp--do-next-after-process-job thread))

(defun timp--tgi-packet-handler (_thread packet)

  ;; Handle instruction generated by child thread.
  ;; Calling appropiate function to handle the instruction.
  "Private function. Using it may cause serious problem."

  (let ((instruction (timp-packet-get-reply packet))
        (arg (timp-packet-get-data packet)))
    (ignore-errors (apply instruction arg)))
  ;; Instruction generated by child thread does not count as reply
  ;; It should not change either the quene state or ready state of the thread.
  )

(defun timp--ldr-packet-handler (thread packet)
  
  ;; If there is no other large data request, process it.
  ;; Otherewise, quene up in timp-socket.LDquene
  "Private function. Using it may cause serious problem."
  ;; Push the thread to large-data of timp-socket
  (timp-socket-large-data-push thread)
  
  (when (eq (timp-socket-large-data-first) thread)
    (let ((sender (timp-get-sender thread)))
      (unless (and sender (process-live-p sender))
        (while (null
                (setq sender (ignore-errors
                               (open-network-stream (concat "thread" (number-to-string (timp-get-id thread)) " - sender")
                                                    nil
                                                    "localhost"
                                                    (timp-get-port thread)
                                                    'plain))))))
      (process-send-string sender (concat (prin1-to-string packet) "\n"))
      
      (timp--debug-print-packet packet :inbound nil)
      
      (if (timp-has-next-job-p thread)
          (timp-set-sender thread sender)
        (ignore-errors (delete-process sender))
        (timp-set-sender thread nil)))))

(defun timp--do-next-after-process-job (thread &optional notQuit)
  
  ;; Perform the next action after getting a reply from the child thread
  ;; and the reply has been processed.
  "Private function. Using it may cause serious problem."

  (timp-clear-current-job thread)
  (if (timp-has-next-job-p thread)
      (if (timp-socket-job-in-buffer-p thread)
          (progn
            (timp-socket-remove-from-buffer thread)
            (timp--push-to-outbound thread))
        (timp--push-to-outbound thread))
    (unless (or (timp-persist-p thread) notQuit)
      (timp-quit thread))))


(defclass timp (fifo-class)
  ((id
    :initarg :id
    :type integer
    :accessor timp-id
    :protection :private)
   (process
    :initarg :process
    :type process
    :accessor timp-process
    :protection :private)
   (sender
    :type process
    :accessor timp-sender
    :protection :private)
   (port
    :initform nil
    :initarg :port
    ;; port type is checked by setter
    :accessor timp-port
    :protection :private)
   (persist
    :initform nil
    :initarg :persist
    :type boolean
    :accessor timp-perisist
    :protection :private)
   (quit-warn
    :initform nil
    :initarg :quit-warn
    :type (or null string)
    :accessor timp-quit-warn
    :protection :private)
   (job
    :initform nil
    :accessor timp-job
    :protection :private)
   (current-job
    :initform nil
    :accessor timp-current-job
    :protection :private)
   (quene
    :initform nil
    :type boolean
    :accessor timp-quene
    :protection :private)
   (load-path
    :initform nil
    :type boolean
    :accessor timp-load-path
    :protection :private))

  "Timp (Thread) class. `timp-get' is the only vaild
way to create a thread instance.")

(defmethod initialize-instance :before ((_obj timp) &rest args)
  ;; Constructor. Make sure name and process get initialized.
  "Private function. Using it may cause serious problem."
  (unless (plist-get (car args) ':id)
    (error "Slot :name must be initialized."))
  (unless (plist-get (car args) ':process)
    (error "Slot :process must be initialized.")))

(defmethod timp-get-id ((obj timp))
  "Private function.Private function. Using it may cause serious problem."
  ;; Get the thread id (assq (car data) timp--record)
  (timp-id obj))

(defmethod timp-get-process ((obj timp))
  "Private function. Using it may cause serious problem."
  ;; Get the thread process
  (timp-process obj))

(defmethod timp-get-sender ((obj timp))
  "Private function. Using it may cause serious problem."
  ;; Get the thread sender
  (timp-sender obj))

(defmethod timp-set-sender ((obj timp) sender)
  "Private function. Using it may cause serious problem."
  ;; Set the thread sender
  (setf (timp-sender obj) sender))

(defmethod timp-set-port ((obj timp) port)
  "Private function. Using it may cause serious problem."
  ;; Set the thread port
  (unless (and (integerp port) (> port 0) (<= port 65535))
    (error "Invalid port"))
  (setf (timp-port obj) port))

(defmethod timp-get-port ((obj timp))
  "Private function. Using it may cause serious problem."
  ;; Get the thread port
  (timp-port obj))

(defmethod timp-persist-p ((obj timp))
  "Private function. Using it may cause serious problem."
  ;; Return whether the thread should be persist
  (timp-perisist obj))

(defmethod timp-get-quit-warn ((obj timp))
  "Private function. Using it may cause serious problem."
  ;; Return the quit warning of the thread.
  (timp-quit-warn obj))

(defmethod timp-get-job ((obj timp))
  ;; Get job list from thread
  "Private function. Using it may cause serious problem."
  (timp-job obj))

(defmethod timp-has-next-job-p ((obj timp))
  ;; Whether there is job
  "Private function. Using it may cause serious problem."
  (when (timp-get-job obj) t))

(defmethod timp-push-job ((obj timp) packet)
  ;; Push job to thread's job quene
  "Private function. Using it may cause serious problem."
  (fifo-class-push obj 'job packet))

(defmethod timp-pop-job ((obj timp))
  ;; Pop job from thread's job quene and return the job's timp-packet.
  "Private function. Using it may cause serious problem."
  (fifo-class-pop obj 'job))

(defmethod timp-get-current-job ((obj timp))
  ;; Get the current job from the thread
  "Private function. Using it may cause serious problem."
  (timp-current-job obj))

(defmethod timp-set-current-job ((obj timp) packet)
  ;; Set the current job to thread
  "Private function. Using it may cause serious problem."
  (setf (timp-current-job obj) packet))

(defmethod timp-clear-current-job ((obj timp))
  ;; Remove the current job
  "Private function. Using it may cause serious problem."
  (setf (timp-current-job obj) nil))

(defmethod timp-ready-p ((obj timp))
  "Private function. Using it may cause serious problem."
  ;; Whether the thread is ready to send next job.
  ;; No current job means ready
  (unless (timp-current-job obj) t))

(defmethod timp-quened-p ((obj timp))
  ;; Whether thread is quened in timp-socket
  "Private function. Using it may cause serious problem."
  (timp-quene obj))

(defmethod timp-set-quene ((obj timp) quene)
  ;; quene is either t or nil
  "Private function. Using it may cause serious problem."
  (setf (timp-quene obj) quene))

(defmethod timp-load-path-ready-p ((obj timp))
  ;; Return whether load path is set
  "Private function. Using it may cause serious problem."
  (timp-load-path obj))

(defmethod timp-flag-load-path-ready ((obj timp))
  ;; Return whether load path is set
  "Private function. Using it may cause serious problem."
  (setf (timp-load-path obj) t))


(defun timp-validate (object)

  "Validate whether OBJECT is a thread and is valid.
Return t for valid OBJECT."

  (if (and (timp-p object)
           (processp (timp-get-process object))
           (process-live-p (timp-get-process object)))
      t
    (when (timp-p object)
      (timp-force-quit object))
    nil))

(defmethod timp-quit ((obj timp))
  
  "Send a quit signal to child Thread to perform safe quit action."

  (when (process-live-p (timp-get-process obj))
    (timp-push-job obj (make-instance 'timp-packet
                                      :source (timp-get-id obj)
                                      :type 'quit
                                      :data t))
    (unless (timp-quened-p obj)
      (timp--push-to-outbound obj))))

(defmethod timp-force-quit ((obj timp))

  "Forced quit a THREAD without letting the thread to stop its job kindly."

  (timp--quit-packet-handler obj))

(defmethod timp--send-exec ((obj timp) func unique reply-func error-handler quit-warn &rest arg)

  "Not supposed to be called directly. Use `timp-send-exec' instead."
  
  (when (process-live-p (timp-get-process obj))
    (let ((jobs (timp-get-job obj))
          (packet (make-instance 'timp-packet
                                 :source (timp-get-id obj)
                                 :type 'exe
                                 :data (cons func arg)
                                 :reply reply-func
                                 :error-handler error-handler
                                 :quit-warn quit-warn)))
      (unless (and unique (or (member packet jobs) (equal (timp-get-current-job obj) packet)))
        (timp-push-job obj packet)
        
        (unless (timp-quened-p obj)
          (timp--push-to-outbound obj))))))

(cl-defmacro timp-send-exec (timp func &rest arg &key unique reply-func error-handler quit-warn &allow-other-keys)

  ;; Just a wrapper to timp--send-exec
  ;; so that arguments can be supplied in a more elegant way.
  "Send single instruction to child thread.

TIMP is a thread object which can be aquired by `timp-get'.
FUNC is the symbol of function which you want to execute in child thread.
ARGS are the arguments supplied to FUNC.

UNIQUE is to stated whether the job is unique.
Non-nil value states that the job should be unique.
For example, you send a update database job to child thread.
The update database job takes some time, says 3 minutes.
If you send subsequent

REPLY-FUNC is the function to be called when excution of FUNC returned a
result. The result is pass as the argument to the REPLY-FUNC.
You can ignore it if you don't need a reply. 

ERROR-HANDLER is the function to be called when error is encountered
in the child thread during excuting of instruction.
The ERROR-HANDER function will be called with the error message as argument.
You can ignore it if you don't handle the error.

The instruction will be executed by `apply' in the child thread."
  
  (let (key rest)
    (dolist (elt arg)
      (if (memq elt '(:unique :reply-func :error-handler :quit-warn))
          (setq key elt)
        (if key
            (progn
              (cond
               ((eq key ':unique)
                (setq unique elt))
               ((eq key ':reply-func)
                (setq reply-func elt))
               ((eq key ':error-handler)
                (setq error-handler elt))
               ((eq key ':quit-warn)
                (setq quit-warn elt)))
              (setq key nil))
          (push elt rest))))
    (setq rest (nreverse rest))

    `(timp--send-exec ,timp ,func ,unique ,reply-func ,error-handler ,quit-warn ,@rest)))

(defmethod timp--send-code ((obj timp) code unique reply-func error-handler quit-warn)

  "Not supposed to be called directly. Use `timp-send-code' instead."

  (when (process-live-p (timp-get-process obj))
    (let ((jobs (timp-get-job obj))
          (packet (make-instance 'timp-packet
                                 :source (timp-get-id obj)
                                 :type 'code
                                 :data code
                                 :reply reply-func
                                 :error-handler error-handler
                                 :quit-warn quit-warn)))
      (unless (and unique (or (member packet jobs) (equal (timp-get-current-job obj) packet)))
        (timp-push-job obj packet)
        
        (unless (timp-quened-p obj)
          (timp--push-to-outbound obj))))))


(cl-defmacro timp-send-code (timp &key code unique reply-func error-handler quit-warn)

  ;; Just a wrapper to timp--send-code
  ;; so that arguments can be supplied in a more elegant way.
  "Evaluate CODE in child thread.

timp is a thread object which can be aquired by `timp-get'.
CODE is the code being evaluated at child thread.

REPLY-FUNC is the function to be called when excution of FUNC returned a
result. The result is pass as the argument to the REPLY-FUNC.
You can ignore it if you don't need a reply. 

ERROR-HANDLER is the function to be called when error is encountered
in the child thread during excuting of instruction.
The ERROR-HANDER function will be called with the error message as argument.
You can ignore it if you don't handle the error."

  `(timp--send-code ,timp ,code ,unique ,reply-func ,error-handler ,quit-warn))

(defmethod timp-require-package ((obj timp) &rest packets)

  "Require PACKETS in child threads.
This function helps managing load-path in child threads."

  (unless (timp-load-path-ready-p obj)
    (timp-send-exec obj 'timp-server-set-load-path 
                    :error-handler 'timp-debug-print
                    load-path)
    (timp-flag-load-path-ready obj))

  (timp-send-exec obj 'timp-server-require-packet packets
                  :error-handler 'timp-debug-print))

(defmethod timp--send-variable ((obj timp) var-symbol var-value)

  "Not supposed to be called directly. Use `timp-send-variable' instead."
  
  (timp-send-code obj
                  :code `(setq ,var-symbol ',var-value)
                  :error-handler 'timp-debug-print))

(defmacro timp-send-variable (thread &rest variables)

  "Set VARIABLES in child THREAD to value in parent threads.
Note that VARIABLES do not need to be quoted.

For example, in parent thread, a = 10 and b = 20;
in child thread a = 0 and b = 0.

\(timp-send-variable child-thread a b\)

After this operation, a = 10 and b = 20 in child thread."
  
  (let (validated-variable)
    (dolist (var variables)
      (if (boundp var)
          (push var validated-variable)
        ;; Should it raises an error or just print a message?
        (error "%s is not defined." (symbol-name var))))
    (setq validated-variable (nreverse validated-variable))
    `(dolist (var ',validated-variable)
       (timp--send-variable ,thread var (symbol-value var)))))

(defun timp-debug-print (object)

  "Print OBJECT to thread log."

  (unless (stringp object)
    (setq object (prin1-to-string object)))
  
  (with-current-buffer (get-buffer-create timp-debug-buffer-name)
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (insert (format-time-string "%Y%m%d - %I:%M:%S%p $ ")
            (format "%s\n" object))
    (setq buffer-read-only t))
  (when (eq (current-buffer) (get-buffer timp-debug-buffer-name))
    (recenter -3)))


(defun timp--process-outbound-data ()

  ;; Process the first thread in the outbound of timp-socket

  "Private function. Using it may cause serious problem."

  (let ((thread (timp--pop-from-outbound)))
    ;; If ready do job, if not ready requene in outbound of timp-socket
    
    (if (timp-ready-p thread)
        ;; Check if process alive, if not, release if from thread record
        (if (process-live-p (timp-get-process thread))
            (let ((sender (timp-get-sender thread))
                  packet)
              
              ;; Make a network stream
              (unless (and sender (process-live-p sender))
                (setq sender 
                      (ignore-errors
                        (open-network-stream (concat "thread" (number-to-string (timp-get-id thread)) " - sender")
                                             nil
                                             "localhost"
                                             (timp-get-port thread)
                                             'plain))))
              
              (when sender
                ;; Send job
                (setq packet (timp-pop-job thread))
                (process-send-string sender
                                     (concat (prin1-to-string packet) "\n"))
                (timp-set-current-job thread packet)

                ;; debug
                (timp--debug-print-packet packet :inbound nil)
                
                (if (timp-has-next-job-p thread)
                    (progn
                      ;; Still has job, keep the sender, quene the thread again
                      (timp-set-sender thread sender)
                      (timp--push-to-outbound thread))
                  ;; No job, close the network stream
                  (ignore-errors (delete-process sender))
                  (timp-set-sender thread nil))))
          
          ;; Process not alive, delete form thread record
          (timp-force-quit thread))

      ;; Thread is not ready, put to buffer
      (timp--push-to-outbound-buffer thread))))


(defun timp--kill-emacs (orig &optional arg count)

  ;; This function is supposed to be adviced
  ;; :around kill-emacs and save-buffers-kill-emacs.
  ;; When ~kill-emacs~ is invoked, attempt to close all threads safely
  ;; with certain period of time.
  ;; By default, it is 5 seconds defined customly in
  ;; timp-kill-emacs-close-thread-delay.
  "Private function. Using it may cause serious problem."

  (let (threads)
    (dolist (record timp--record)
      (when (cdr record)
        (push (cdr record) threads)))

    (if (and threads
             (or (null count)
                 (< (* count 0.3) timp-kill-emacs-close-thread-delay)))
        (progn
          (when (and count (= (% count 3) 0))
            (message "%d thread%s is running. Try to quit %s safely.
Emacs will be quit within %d seconds."
                     (length threads)
                     (if (> (length threads) 1) "s" "")
                     (if (> (length threads) 1) "them" "it")
                     (or (and count
                              (ceiling
                               (- timp-kill-emacs-close-thread-delay
                                  (* count 0.3))))
                         timp-kill-emacs-close-thread-delay)))
          
          (unless count
            ;; Try to safe quit all threads
            (dolist (thread threads)
              (timp-quit thread))
            
            ;; Don't block the ui when closing threads
            ;; Update the close thread progress in
            ;; timp--kill-emacs-reporter
            (signal-connect :signal 'timp--kill-emacs-signal
                            :worker 'timp--kill-emacs))
          ;; Call itself recusively 
          (signal-emit 'timp--kill-emacs-signal
                       :delay 0.3
                       :arg (list orig arg (or (and (null count) 1) (1+ count)))))

      ;; If no threads, close happily.
      ;; If still has threads, froce close.
      (signal-disconnect 'timp--kill-emacs-signal 'timp--kill-emacs)

      (let (stop-quit)
        ;; Force quit any threads without warning
        ;; Ask for user confirm if there is warning
        (dolist (thread threads)
          (if (or
               (and (timp-get-current-job thread) (timp-packet-get-quit-warn (timp-get-current-job thread)))
               (timp-get-quit-warn thread))
              (if (yes-or-no-p (concat (or
                                        (timp-packet-get-quit-warn (timp-get-current-job thread))
                                        (timp-get-quit-warn thread))
                                       "\nDo you really want to quit?"))
                  (timp-force-quit thread)
                (setq stop-quit t))
            (timp-force-quit thread)))
        
        (if stop-quit
            (message "User cancelled kill-emacs action.")
          (ignore-errors (delete-process timp--data-listener))
          (ignore-errors (delete-process timp--large-data-listener))
          ;; Close the network stream created by child threads.
          (dolist (process (process-list))
            (when (string-prefix-p "thread" (process-name process))
              (ignore-errors (delete-process process))))
          (apply orig (list arg)))))))

(advice-add 'kill-emacs :around 'timp--kill-emacs)
(advice-add 'save-buffers-kill-emacs :around 'timp--kill-emacs)

(provide 'timp)
;;; timp.el ends here
