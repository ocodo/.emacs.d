;;; timp-socket.el --- Manage thread data sending  -*- lexical-binding: t; -*- 
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
;; Manage threads data sending.
;; It quenes up inbound and outbound data to prevent data loss
;; when transfering data concurrently across threads.
;;
;;; Code:
(require 'eieio-base)
(require 'fifo-class)
(require 'signal)

(defsignal timp-socket--outbound-signal
  ;; Singal to be emitted when there is outbound job.
  "Private variable. Modifying it may cause serious problem.")

(defsignal timp-socket--inbound-signal
  ;; Singal to be emitted when there is inbound job.
  "Private variable. Modifying it may cause serious problem.")

;; timp-socket is a singleton class
;; It helps quening inbound and outbound jobs.
;; When there is thread jobs or reply from child thread.
;; They are pushed to the socket instead of process directly.
;; Because if the jobs are processed directly,
;; the main thread will be in blocking state.
;; When another job is going to send out or received at the same moment,
;; the main thread cannot process it which cause data/job lost.
;; Quening is a very fast process which can be done almost at negligible time.
;; So, it ensures only one job is processing while it can quening up other jobs.
(defclass timp-socket (fifo-class eieio-singleton)
  ((outbound
    :initform nil
    :accessor timp-socket-outbound
    :protection :private)
   ;; fifo outbound data
   ;; outbound contains a timp object
   ;; which the thread object has a job quene itself
   (inbound
    :initform nil
    :accessor timp-socket-inbound
    :protection :private)
   ;; fifo inbound data
   ;; Inbound contains unprocessed form of timp-packet object
   (buffer
    :initform nil
    :accessor timp-socket-buffer
    :protection :private)
   ;; When timp object fails to send out thread job
   ;; because child thread has not yet reply
   ;; The timp object is stored here instead of requene immediately
   ;; This prevents flooding of timp-socket--outbound-signal
   ;; just for trying to send out job and requening
   (large-data
    :initform nil
    :accessor timp-socket-large-data
    :protection :private)
   ;; It is the large data quene.
   ;; When child threads have large data (>4kb) to send to parent,
   ;; they will ask for permission from the parent thread.
   ;; The parent thread will allow only one large data to be sent at the same time.
   ;; Other large data request will be placed here to quene for the premission.
   )
  "`timp-socket' class.")

(defconst timp--socket-instance
  (make-instance 'timp-socket)
  ;; Initialize a timp-socket instance
  "Private variable. Modifying it may cause serious problem.")


(defun timp-socket-inbound-push (data)
  ;; Push DATA to timp-socket buffer.
  "Private function. Using it may cause serious problem."
  (fifo-class-push timp--socket-instance 'inbound data)
  (signal-emit 'timp-socket--inbound-signal))

(defun timp-socket-inbound-pop ()
  ;; Get DATA from timp-socket buffer.
  "Private function. Using it may cause serious problem."
  (fifo-class-pop timp--socket-instance 'inbound))

(defmethod timp-socket-get-inbound ((obj timp-socket))
  ;; Get inbound buffer from socket
  ;; Only for the hasNext function
  "Private function. Using it may cause serious problem."
  (timp-socket-inbound obj))

(defun timp-socket-inbound-has-next ()
  ;; Whether there is job in buffer
  "Private function. Using it may cause serious problem."
  (when (timp-socket-get-inbound timp--socket-instance) t))


(defun timp-socket-outbound-push (data)
  ;; Push DATA to timp-socket buffer.
  "Private function. Using it may cause serious problem."
  (fifo-class-push timp--socket-instance 'outbound data)
  (signal-emit 'timp-socket--outbound-signal))

(defun timp-socket-outbound-pop ()
  ;; Get DATA from timp-socket buffer.
  "Private function. Using it may cause serious problem."
  (fifo-class-pop timp--socket-instance 'outbound))

(defmethod timp-socket-get-outbound ((obj timp-socket))
  "Private function. Using it may cause serious problem."
  ;; Get Outbound buffer from socket
  ;; Only for the hasNext function
  (timp-socket-outbound obj))

(defun timp-socket-outbound-has-next ()
  ;; Whether there is job in buffer
  "Private function. Using it may cause serious problem."
  (when (timp-socket-get-outbound timp--socket-instance) t))


(defmethod timp-socket-get-buffer ((obj timp-socket))
  ;; Get the socket buffer.
  "Private function. Using it may cause serious problem."
  (timp-socket-buffer obj))

(defmethod timp-socket-add-to-buffer ((obj timp-socket) thread)
  ;; Add the thread to buffer if it is not here.
  "Private function. Using it may cause serious problem."
  (cl-pushnew (timp-socket-buffer obj) thread))

(defmethod timp-socket--remove-from-buffer ((obj timp-socket) thread)
  ;; Get the thread from buffer and remove it the thread form buffer.
  "Private function. Using it may cause serious problem."
  (setf (timp-socket-buffer obj) (remq thread (timp-socket-buffer obj))))

(defun timp-add-to-socket-buffer (thread)
  ;; Add the thread to thread.scoket's buffer if it is not here.
  "Private function. Using it may cause serious problem."
  (timp-socket-add-to-buffer timp--socket-instance thread))

(defun timp-socket-remove-from-buffer (thread)
  ;; Remove the thread from thread.scoket's buffer if it exists.
  "Private function. Using it may cause serious problem."
  (timp-socket--remove-from-buffer timp--socket-instance thread))

(defun timp-socket-job-in-buffer-p (thread)
  ;; Return t if thread is in timp-socket-buffer.
  "Private function. Using it may cause serious problem."
  (when (memq thread (timp-socket-get-buffer timp--socket-instance)) t))


(defun timp-socket-large-data-push (thread)
  ;; When child thread sends large data request
  ;; and it is not ready to handle,
  ;; push the child thread to the large-data-quene.
  "Private function. Using it may cause serious problem."
  (fifo-class-push timp--socket-instance 'large-data thread))

(defun timp-socket-large-data-pop ()
  ;; Pop the first thread in the large-data.
  "Private function. Using it may cause serious problem."
  (fifo-class-pop timp--socket-instance 'large-data))

(defun timp-socket-large-data-first ()
  ;; Get the first thread in the large-data without removing it.
  "Private function. Using it may cause serious problem."
  (fifo-class-first timp--socket-instance 'large-data))

(defun timp-socket-large-data-has-next ()
  ;; Return whether there is job in the large-data
  "Private function. Using it may cause serious problem."
  (when (fifo-class-first timp--socket-instance 'large-data) t))

(provide 'timp-socket)
;;; timp-socket.el ends here
