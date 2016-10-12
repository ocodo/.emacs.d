;;; timp-server.el --- Implementation of child thread  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2015-2016 Mola-T
;; Author: Mola-T <Mola@molamola.xyz>
;; URL: https://github.com/mola-T/timp
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
;; It is the backend or child thread of timp.el
;; Under normal situation, end user may not notice that it is running.
;; Because the only propriate used is called by timp.el
;; and runs in a subprocess.
;;
;;; code:


;; Must not impletment socket in timp-server
;; With previous testing, it will become 3 times slower.

;; In reality, don't need to require these package
;; Because they are controlled in parent thread
;; I put here just for debugging
;; (require 'signal)
;; (require 'timp-packet)

;; (defvar threadS-stop nil
;;   "Thread will stop if it is set to t.")

(defconst timp-server-stream "timp-server-stream"
  "Process name of the data stream.
Implement through localhost.")

(defconst timp-server-sender "timp-server-sender"
  "Process name of the data sender.")

(defconst timp-server-Dsender "timp-server-Dsender"
  "Process name of the large data sender.")

(defvar timp-server-id nil
  "Number of this thread.")

(defvar timp-server-port nil
  "Port of the timp server (this server).")

(defvar timp-server-parent-port nil
  "Port of thre parent thread.")

(defvar timp-server-parent-large-data-port nil
  "Large data port of the parent thread.")

(defvar timp-server-large-data-permission nil
  "It becomes t when it gets permission to send large data.")

(defvar timp-server-buffer '(0)
  ;; Minimum need to have a list for nconc 
  "Buffer for imcomplete received data.")

(defvar timp-server-complete-packet-buffer nil
  "`timp-server-buffer' is transferred here when getting a complete packet.
So `timp-server-buffer' is able to handle one more incoming packet.
Usually there is only one packet to receive at the same time.
However, there is an expectional case that it needs to receive
a large data sending premission.")

(defvar timp-server-inhibit-message nil
  "Non-nil value prevent message from redirecting back to parent thread.")

(defsignal timp-server-quit-signal
  "A block signal to be emitted when it receives
a quit message from parent thread.")

(defun timp-server-init ()
  "Ininialize and listen to main process for instruction."
  
  ;; Get parent port
  (let ((info (read-from-minibuffer "" nil nil t)))
    (setq timp-server-id (car info)
          timp-server-parent-port (cadr info)
          timp-server-parent-large-data-port (car (cddr info))))
  ;; I tried to use `caddr' and it won't work.
  ;; I used more than an hour to figure out it is `caddr' causing the problem
  ;; because it is in cl-lib....
  ;; ╭∩╮（￣.￣）╭∩╮

  ;; Start stream for listening data
  ;; in terms of network process
  ;; Should be the faster way to get data from another process
  (make-network-process :name timp-server-stream
                        :server t
                        :host 'local
                        :service t
                        :family 'ipv4
                        :filter 'timp-server-receive-data
                        :nowait t)
  
  ;; Store the local server port that need to send back to parent
  (setq timp-server-port (process-contact (get-process timp-server-stream) :service))
  
  ;;! This sleep time is very important
  ;;! In parent thread, it is doing:
  ;; >>>>> Register the thread to the thread--record
  ;; >>>>> (setf (cdr (assoc timp-num timp--record)) timp)
  ;; `timp-server-send-port-data' need to do after parent thread has finished this operation
  ;; accept-process-output seems a `sleep-for' in batch-mode
  ;; Only accept-process-output is blocking the program without using cpu power
  (accept-process-output nil 0.1)
  (timp-server-send-port-data timp-server-port)

  ;; Redirect 'message to parent's log
  (advice-add 'message :around 'timp-server-message)
  (while t (sleep-for 0.5)))

(defun timp-server-receive-data (_proc data)
  
  "Process received data."
  ;; It needs to be very efficient.
  ;; As it fails to do so, parent process will be blocking
  ;; just for waiting it for process receiving data.

  ;; Data will arrived as string.
  ;; Large data will be split into small data chunks at parent process.
  ;; A newline charater "\n" indicates the end of the chunks.
  ;; One chunk is sent at a time.
  ;; Depends on OS, the max. data size for a data chunk is fixed, say 4kb for my PC.
  ;; So data chunk is put in timp-server-buffer first.
  ;; And combine to form a complete data when the newline character is met.

  ;; Check only the last character
  (if (string-match "\n" data (- (length data) 1))
      (progn
        (nconc timp-server-buffer (list data))
        (setq timp-server-complete-packet-buffer timp-server-buffer) ;; Move the completed packet to another buffer
        (setq timp-server-buffer (list 0)) ;; Need to use (list 0) instead of '(0)
        (timp-server-process-data)) 
    ;; nconc is 100 times faster than concat a string
    (nconc timp-server-buffer (list data))))

(defun timp-server-process-data ()
  "Process a complete data from `timp-server-buffer'."
  ;; It won't block the parent process.
  ;; Efficiency is not care. XD
  ;; Combine the list
  (let ((string (mapconcat 'identity (cdr timp-server-complete-packet-buffer) ""))
        packet
        packet-type)
    
    ;; Cut the last newline char
    (setq string (substring string 0 (- (length string) 1)))
    (setq packet (read string))
    
    ;; Distrubute jobs
    (when (timp-packet-p packet)
      (setq packet-type (timp-packet-get-type packet))
      (cond
       ((eq packet-type 'exe)
        (timp-server-exe-packet-handler packet))
       ((eq packet-type 'code)
        (timp-server-code-packet-handler packet))
       ((eq packet-type 'quit)
        (timp-server-quit))
       ((eq packet-type 'ldr)
        (setq timp-server-large-data-permission t))))))

(defun timp-server-process-data-maybe ()
  "Process parent thread request if existed.
This function is designed to be inserted in a long running function
so that parent thread request could still be handled ASAP."
  (accept-process-output))

(defun timp-server-send-data (packet)
  
  "Send out data through the network stream."
  ;; There is a need to seperate large datas
  ;; Because the main thread can't handle large data at the same time
  ;; resulting a packet (the real ip packet) buffer overflow
  ;; I am not sure it is due to too many retry counts or buffer overflow
  ;; Anyway, there will be packet loss if large data is not handled specially

  ;; To make it more effieicent, small packets (<4kb) can go out directly
  ;; while large packets (>4kb) will need to do a handshake will the main thread
  ;; The main thread holds a quene to let only one large data to be sent at one time

  (let ((data (concat (prin1-to-string packet) "\n"))
        sender)
    (if (> (length data) 4000)
        (progn
          (while (null (setq sender (ignore-errors
                                      (open-network-stream timp-server-sender
                                                           nil
                                                           "localhost"
                                                           timp-server-parent-port
                                                           'plain))))
            (accept-process-output nil 0.05))
          
          ;; Send a handshake packet in the normal data stream
          (process-send-string sender
                               (concat
                                (prin1-to-string (make-instance 'timp-packet
                                                                :source timp-server-id
                                                                :type 'ldr
                                                                :data (length data)))
                                "\n"))
          (ignore-errors (delete-process sender))
          
          ;; Wait until premission got
          (while (null timp-server-large-data-permission)
            (accept-process-output nil 0.05))
          
          ;; Send through large data channel
          (while (null (setq sender (ignore-errors
                                      (open-network-stream timp-server-Dsender
                                                           nil
                                                           "localhost"
                                                           timp-server-parent-large-data-port
                                                           'plain)))))
          (process-send-string sender data)
          (delete-process sender)
          (setq timp-server-large-data-permission nil))

      ;; Small data go out directly
      (while (null (setq sender (ignore-errors
                                  (open-network-stream timp-server-sender
                                                       nil
                                                       "localhost"
                                                       timp-server-parent-port
                                                       'plain))))
        (accept-process-output nil 0.05))
      (process-send-string sender data)
      (delete-process sender))))

(defun timp-server-send-port-data (data)
  "Send port number back to parent thread."
  (timp-server-send-data
   (make-instance 'timp-packet
                  :source timp-server-id
                  :type 'port
                  :data data)))

(defun timp-server-send-err-data (&optional error-code error-handler data)
  "Send reply back to parent thread."
  (timp-server-send-data
   (make-instance 'timp-packet
                  :source timp-server-id
                  :type 'err
                  :error-handler error-handler
                  :data (and error-handler (list (cons error-code data))))))

(defun timp-server-send-msg-data (data)
  "Send message back to parent thread."
  (timp-server-send-data
   (make-instance 'timp-packet
                  :source timp-server-id
                  :type 'msg
                  :data data)))

(defun timp-server-send-quit ()
  "Send signal to parent thread that it is safe to quit."
  (timp-server-send-data
   (make-instance 'timp-packet
                  :source timp-server-id
                  :type 'quit
                  :data t)))

(defun timp-server-send-rpy-data (&optional reply-func data)
  "Send reply back to parent thread."
  (timp-server-send-data
   (make-instance 'timp-packet
                  :source timp-server-id
                  :type 'rpy
                  :reply reply-func
                  :data (and reply-func (list data)))))

(defun timp-server-send-tgi-data (function data)
  "Send instruction generated by child thread."
  (timp-server-send-data
   (make-instance 'timp-packet
                  :source timp-server-id
                  :type 'tgi
                  :reply function
                  :data data)))

(defun timp-server-exe-packet-handler (packet)
  
  "Execute the instruction issued from the parent thread.
It replies with the returning result of the execution to the parent thread.
Otherwise, it will reply nil.
If there is any error during the execution of instrustion,
a packet will be sent to notify the error."
  
  (let ((data (timp-packet-get-data packet))
        (reply-func (timp-packet-get-reply packet))
        (error-handler (timp-packet-get-error-handler packet))
        error-info
        result)
    (setq result (condition-case list
                     (apply (car data) (cdr data))
                   (error list
                          (setq error-info list))))
    (if error-info
        (timp-server-send-err-data (car data) error-handler error-info)
      (timp-server-send-rpy-data reply-func result))))

(defun timp-server-code-packet-handler (packet)
  
  "Evaluate the code issued from the parent thread.
It replies with the returning result of the evaluation to the parent thread.
Otherwise, it will reply nil.
If there is any error during the evaluation of code,
a packet will be sent to notify the error."
  
  (let ((code (timp-packet-get-data packet))
        (reply-func (timp-packet-get-reply packet))
        (error-handler (timp-packet-get-error-handler packet))
        error-info
        result)
    (setq result (condition-case list
                     (eval code)
                   (error list
                          (setq error-info list))))
    (if error-info
        (timp-server-send-err-data code error-handler error-info)
      (timp-server-send-rpy-data reply-func result))))

(defmacro with-timp-server-inhibited-message (&rest body)
  "Temporary inbibite message in BODY from redirecting back
to parent thread."
  `(progn
     (setq timp-server-inhibit-message t)
     ,@body
     (setq timp-server-inhibit-message nil)))

(defun timp-server-message (_orig-func &rest args)
  "Message is meaningless in child thread.
  So send it back to parent."
  (unless timp-server-inhibit-message
   (let ((message (ignore-errors (apply 'format args))))
    (when message
      (timp-server-send-msg-data message)))))

(defun timp-server-set-load-path (path)
  "Set load path in `trim-server'."
  (setq load-path path))

(defun timp-server-require-packet (&rest packets)
  "Require package."
  (dolist (packet (car packets))
    (require packet)))

(defun timp-server-quit ()
  "Terminate the thread safely by emit a signal.
Any backend packages should make connection to this signal
if they want to quit safely."
  (signal-emitb 'timp-server-quit-signal)
  (timp-server-send-quit))

(defun timp-server-sleep-for (second)
  "To pause the `timp-server' for number of SECOND.
SECONDS may be a floating-point value, meaning that you can wait for a
fraction of a second."
  (accept-process-output nil second))

(defalias 'timp-sleep-for 'timp-server-sleep-for)

(defun timp-server-do-nothing ()
  "Seriously, this is a function doing nothing.
If you can read this documentation, you are most possibily looking at
the source code. So, this function is just put here to say hi to you!"
  (when (fboundp 'you-are-looking-at-the-source-code)
    'say-hello))


(defun timp-server-debug-write-file (&rest datas)
  
  "These function is for debugging.
When developing this package, if anything goes wrong,
the subprocess just closes directly or does not respond.
I put this function everywhere to find at which moment it goes wrong."
  
  (let (string)
    (dolist (data datas)
      (if (stringp data)
          (setq string (concat string data "\n"))
        (setq string (concat string (prin1-to-string data) "\n"))))
    (write-region string
                  nil
                  (concat (file-name-as-directory user-emacs-directory)
                          (number-to-string (or timp-server-id 9999)))
                  t)))

;; This provide package is just for developer
;; This package is never needed to be require.
(provide 'timp-server)
;;; timp-server.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
