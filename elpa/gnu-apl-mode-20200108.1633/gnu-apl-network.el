;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'gnu-apl-util)

(defvar *gnu-apl-end-tag* "APL_NATIVE_END_TAG")
(defvar *gnu-apl-notification-start* "APL_NATIVE_NOTIFICATION_START")
(defvar *gnu-apl-notification-end* "APL_NATIVE_NOTIFICATION_END")
(defvar *gnu-apl-protocol* "1.5")
(defvar *gnu-apl-remote-protocol* nil
  "The received version of a protocol on GNU APL side")


;;; We really should be using define-error here, but that function is
;;; new in 24.4 and thus is not generally available yet. This should
;;; be changed once 24.4 is more common place.
;;;(define-error 'gnu-apl-network-proto-error "Network connection error")
(put 'gnu-apl-network-proto-error 'error-conditions '(error gnu-apl-network-proto-error))
(put 'gnu-apl-network-proto-error 'error-message "Network common error")

(defun gnu-apl--connect-to-remote (connect-mode addr)
  "Connect to the remote apl interpreter.
CONNECT-MODE is a string describing how to connect. ADDR is the
address to connect to. The address is interpreted based on the
connect mode in use."
  (cond ((string= connect-mode "tcp")
         (open-network-stream "*gnu-apl-connection*" nil "localhost" (string-to-number addr)
                              :type 'plain
                              :return-list nil
                              :end-of-command "\n"))
        ((string= connect-mode "unix")
         (make-network-process :name "gnu-apl-native"
                               :buffer nil
                               :family 'local
                               :type nil
                               :service addr
                               :coding 'utf-8))
        (t
         (error "Unexpected connect mode: %s" connect-mode))))

(defun gnu-apl--protocol-acceptable-p (version)
  (not (version< version *gnu-apl-protocol*)))

(defun gnu-apl--connect (connect-mode addr)
  (with-current-buffer (gnu-apl--get-interactive-session)
    (when (and (boundp 'gnu-apl--connection)
               (process-live-p gnu-apl--connection))
      (error "Connection is already established"))
    (condition-case err
        (let ((proc (gnu-apl--connect-to-remote connect-mode addr)))
          (set-process-coding-system proc 'utf-8 'utf-8)
          (setq-local gnu-apl--connection proc)
          (setq-local gnu-apl--current-incoming "")
          (setq-local gnu-apl--results nil)
          (setq-local gnu-apl--notifications nil)
          (setq-local gnu-apl--incoming-state 'normal)
          (set-process-filter proc 'gnu-apl--filter-network))
      ;; TODO: Error handling is pretty poor right now
      ('file-error (error "err:%S type:%S" err (type-of err))))
    (condition-case err
        (let ((version (gnu-apl--send-network-command-and-read "proto")))
          (unless (gnu-apl--protocol-acceptable-p (car version))
            (error "GNU APL version too old (%s). Please upgrade to at least %s" (car version) *gnu-apl-protocol*))
          (setq-local *gnu-apl-remote-protocol* (car version)))
      (gnu-apl-network-proto-error (error "GNU APL version too old (<1.3). Please upgrade to at least %s" *gnu-apl-protocol*)))))

(defun gnu-apl--process-notification (lines)
  (let ((type (car lines)))
    (cond ((string= type "symbol_update")
           (gnu-apl--trace-symbol-updated (cdr lines)))
          ((string= type "sev_erased")
           (gnu-apl--trace-symbol-erased (cadr lines)))
          (t
           (error "Unexpected notificationt type: %s" type)))))

(defun gnu-apl--filter-network (proc output)
  (with-current-buffer (gnu-apl--get-interactive-session)
    (setq gnu-apl--current-incoming (concat gnu-apl--current-incoming output))
    (loop with start = 0
          for pos = (cl-position ?\n gnu-apl--current-incoming :start start)
          while pos
          do (let ((s (subseq gnu-apl--current-incoming start pos)))
               (setq start (1+ pos))

               (cond ((string= s *gnu-apl-notification-start*)
                      (unless (eq gnu-apl--incoming-state 'normal)
                        (error "Attempt to enter notification state while in notification"))
                      (setq gnu-apl--incoming-state 'override))
                     ((string= s *gnu-apl-notification-end*)
                      (unless (eq gnu-apl--incoming-state 'override)
                        (error "Attempt to exit notification state while in normal state"))
                      (setq gnu-apl--incoming-state 'normal)
                      (gnu-apl--process-notification gnu-apl--notifications)
                      (setq gnu-apl--notifications nil))
                     ((eq gnu-apl--incoming-state 'normal)
                      (setq gnu-apl--results (nconc gnu-apl--results (list s))))
                     ((eq gnu-apl--incoming-state 'override)
                      (setq gnu-apl--notifications (nconc gnu-apl--notifications (list s))))
                     (t
                      (error "Illegal state"))))

          finally (when (plusp start)
                    (setq gnu-apl--current-incoming (subseq gnu-apl--current-incoming start))))))

(defun gnu-apl--send-network-command-and-read (command)
  (gnu-apl--send-network-command command)
  (gnu-apl--read-network-reply-block))

(defun gnu-apl--send-network-command (command)
  (with-current-buffer (gnu-apl--get-interactive-session)
    (process-send-string gnu-apl--connection (concat command "\n"))))

(defun gnu-apl--send-block (lines)
  (dolist (line lines)
    (gnu-apl--send-network-command line))
  (gnu-apl--send-network-command *gnu-apl-end-tag*))

(defun gnu-apl--read-network-reply ()
  (with-current-buffer (gnu-apl--get-interactive-session)
    (loop while (and (null gnu-apl--results) (process-live-p gnu-apl--connection))
          do (accept-process-output gnu-apl--connection 3))
    (unless gnu-apl--results
      (signal 'gnu-apl-network-proto-error 'disconnected))
    (let ((value (pop gnu-apl--results)))
      value)))

(defun gnu-apl--read-network-reply-block ()
  (loop for line = (gnu-apl--read-network-reply)
        while (not (string= line *gnu-apl-end-tag*))
        collect line))

(provide 'gnu-apl-network)
