;;; timp-packet.el --- data packet to transfer between threads  -*- lexical-binding: t; -*- 
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
;; It provides timp-packet class which is used to transfer
;; instrustion or data between parent threads and child threads.
;;
;;; code:
(require 'eieio)

(defconst timp--packet-type
  '(port err msg quit exe code rpy tgi ldr)
  ;; Type of packet.
  ;; Stand for:
  ;; port = port
  ;; err = error
  ;; msg = message
  ;; quit = quit
  ;; exe = single execution instruction
  ;; code = code
  ;; rpy = reply
  ;; tgi = thread generated instruction
  ;; ldr = large data request
  "Private variable. Modifying it may cause serious problem.")

(defun timp--packet-type-p (test)
  ;; Predicate whether TEST is a correct thread type.
  "Private function. Using it may cause serious problem."
  (memq test timp--packet-type))


(defclass timp-packet ()
  ((source :initarg :source
           :type integer
           :accessor timp-packet-source
           :protection :private)
   ;; The id of thread whose send this packet
   ;; For parent thread, it is not really important
   ;; For child thread, it is very important as parent thread
   ;; needs this info to differentiate which child thread is replying
   (type :initarg :type
         :type (satisfies timp--packet-type-p)
         :accessor timp-packet-type
         :protection :private)
   ;; port err msg exe cod rpy
   ;; Stand for:
   ;; port = port
   ;; err = error
   ;; msg = message
   ;; quit = quit
   ;; exe = single execution instruction
   ;; code = code
   ;; rpy = reply
   ;; tgi = thread generated instruction
   ;; ldr = large data request
   (data :initarg :data
         :initform nil
         :accessor timp-packet-data
         :protection :private)
   ;; Store the data
   (reply :initarg :reply
          :initform nil
          :accessor timp-packet-reply
          :protection :private)
   ;; Store the function that will be called when the job is done and repiled.
   ;; The function will be called with argument stored in data
   ;; where data is the return value of the job
   (error-handler :initarg :error-handler
                  :initform nil
                  :accessor timp-packet-error-handler
                  :protection :private)
   ;; The function to be called if error occure in child thread.
   ;; The function need to accept the error infomation will be
   ;; passed to the handler function
   (quit-warn :initarg :quit-warn
              :initform nil
              :accessor timp-packet-quit-warn
              :protection :private)
   ;; The message to be printed and asked for confirmation
   ;; if the thread is tried to force quit by user.
   )
  ;; Once a packet is created, it cannot be modified.
  "A timp packet class.")


(defmethod initialize-instance :before ((_obj timp-packet) &rest args)
  "Constructor. Make sure source and type get initialized."
  (unless (plist-get (car args) ':source)
    (error "Slot :source must be initialized."))
  (unless (plist-get (car args) ':type)
    (error "Slot :type must be initialized.")))


(defmethod timp-packet-get-source ((obj timp-packet))
  ;; Get the source thread of the packet.
  "Private function. Using it may cause serious problem."
  (timp-packet-source obj))

(defmethod timp-packet-get-type ((obj timp-packet))
  ;; Get the type of the packet.
  "Private function. Using it may cause serious problem."
  (timp-packet-type obj))

(defmethod timp-packet-get-data ((obj timp-packet))
  ;; Get the Data of the packet.
  "Private function. Using it may cause serious problem."
  (timp-packet-data obj))

(defmethod timp-packet-get-reply ((obj timp-packet))
  ;; Get the reply form packet
  "Private function. Using it may cause serious problem."
  (timp-packet-reply obj))

(defmethod timp-packet-get-error-handler ((obj timp-packet))
  ;; Get the error handler form packet
  "Private function. Using it may cause serious problem."
  (timp-packet-error-handler obj))

(defmethod timp-packet-get-quit-warn ((obj timp-packet))
  ;; Get the quit warning message from packet
  "Private function. Using it may cause serious problem."
  (timp-packet-quit-warn obj))

;; (defmethod thread.packet.clrQuitWarn ((obj timp-packet))
;;   ;; Get the quit warning message from packet
;;   "Private function. Using it may cause serious problem."
;;   (setf (timp-packet-quit-warn obj) nil))

(provide 'timp-packet)
;;; timp-packet.el ends here
