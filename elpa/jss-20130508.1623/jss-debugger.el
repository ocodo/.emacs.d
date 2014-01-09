;;; jss-debugger.el -- jss mode for handling a browser exception
;;
;; Copyright (C) 2013 Edward Marco Baringer
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(require 'eieio)
(require 'jss-browser-api)
(require 'jss-remote-value)
(require 'url)
(require 'jss-console)
(require 'jss-prompt)

(defvar jss-debugger nil
  "Dummy variable used to pass the debugger to the function jss-debugger-mode.")

(make-variable-buffer-local
 (defvar jss-current-debugger-instance))

(defun jss-current-debugger ()
  jss-current-debugger-instance)

(make-variable-buffer-local
 (defvar jss-debugger-num-frames nil))

(define-derived-mode jss-debugger-mode jss-super-mode "JSS Debugger"
  "The jss-debugger serves to signal exceptions, provide any
neccessary context and give the tools required to debug the
current state of the tab.

The first item in a debugger buffer is always the exception
object itself, represented as a normal jss-remote-vaule.

The debugger buffer then the current frames on the call stack and
provides, for each frame, a link to the frame's source code and a
prompt running in the frame's execution context.

Use n (jss-frame-next) and p (jss-frame-previous) to move between
frames, hit <return> on a frame to see its source location and
its prompt (each frame has its own prompt which evaluates within
the dynamic context of that frame). On a frame label hit
s (jss-frame-goto-source) to visit the source code of the frame
in a jss-script buffer.

By default jss will create a new buffer, not attached to any
file, containing the frame's source code, but see
`jss-script-source-original-location-functions` for ways to map
source urls to real files. Note: when using the source-location
mapping jss will assume that the contents of the local file are
what the server sees, but it will not check; it is the user's
task to know out if the current contents of the buffer match what
the server was given for that url at page/script load time.

Most of the debugger keybindings are on single letter keys and
are _not_ available when point is in a frame's prompt. Inside the
prompt for a specific frame use jss-frame-previous (bound to C-c
C-c in the prompt's map) to jump out of the prompt and back to
the current frame's top line."
  (setf jss-current-debugger-instance jss-debugger
        jss-current-tab-instance (jss-debugger-tab jss-debugger))
  (add-hook 'kill-buffer-hook 'jss-debugger-kill nil t)
  (widen)
  (delete-region (point-min) (point-max))
  (let ((buffer (current-buffer)))
    (jss-debugger-call-auto-resumes (jss-current-debugger))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (jss-wrap-with-text-properties (list 'jss-debugger-exception t)
          (jss-debugger-insert-message (jss-current-debugger))
          (unless (bolp) (insert "\n"))
          (insert "Paused on ")
          (jss-insert-remote-value (jss-debugger-exception (jss-current-debugger))))
        (insert "\n\n")
        (loop
         initially (setf jss-debugger-num-frames 0)
         for frame in (jss-debugger-stack-frames jss-debugger)
         do (incf jss-debugger-num-frames)
         do (jss-debugger-insert-frame frame (1- jss-debugger-num-frames)))
        (goto-char (car (jss-find-property-block 'jss-debugger-exception t)))))
    t))

(easy-menu-define jss-debugger-menu jss-debugger-mode-map
  "Menu for JSS Debugger buffers."
  `("JSS Debugger"
    [ "Resume" jss-debugger-stepper-resume t ]
    [ "Step Into" jss-debugger-stepper-step-into t ]
    [ "Step Over" jss-debugger-stepper-step-over t ]
    [ "Step Out" jss-debugger-stepper-step-out t ]
    [ "Jump to Next Frame" jss-frame-next t ]
    [ "Jump to Previous Frame" jss-frame-previous t ]
    [ "Jump to Exception" jss-frame-goto-exception t]
    [ "Jump to Frame Source" jss-frame-goto-source :active (get-text-property (point) 'jss-frame) ]))

(defmethod jss-debugger-mode* ((dbg jss-generic-debugger))
  (let ((jss-debugger dbg))
    (jss-debugger-mode)))

(defvar jss-debugger-mode-map (make-sparse-keymap))

(define-key jss-debugger-mode-map (kbd "n") 'jss-frame-next)
(define-key jss-debugger-mode-map (kbd "p") 'jss-frame-previous)
(define-key jss-debugger-mode-map (kbd "e") 'jss-frame-goto-exception)
(define-key jss-debugger-mode-map (kbd "s") 'jss-frame-goto-source)

(define-key jss-debugger-mode-map (kbd "g") 'jss-debugger-stepper-frame-restart)

(define-key jss-debugger-mode-map (kbd "r") 'jss-debugger-stepper-resume)
(define-key jss-debugger-mode-map (kbd "q") 'jss-debugger-stepper-resume)
(define-key jss-debugger-mode-map (kbd "i") 'jss-debugger-stepper-step-into)
(define-key jss-debugger-mode-map (kbd "v") 'jss-debugger-stepper-step-over)
(define-key jss-debugger-mode-map (kbd "o") 'jss-debugger-stepper-step-out)

(defvar jss-debugger-resume-points '())

(defun jss-debugger-set-resume-point-here ()
  "Records the source location of the top frame of the current
debugger in the jss-debugger-resume-points global variable (so
that jss-debugger-resume-point can test for it)"
  (interactive)
  
  (block nil
    (unless (jss-current-debugger)
      (error "Not currently in a debugger."))
    (let ((here-frame (first (jss-debugger-stack-frames (jss-current-debugger)))))
      (unless here-frame
        (warn "No top frame in current debugger, can't get source.")
        (return))
      (jss-deferred-then
       (jss-frame-get-source-location here-frame)
       (lambda (location)
         (block nil
           (destructuring-bind (script line column)
               location
             (unless script
               (warn "No script location for top frame of debugger.")
               (return))
             (unless (and line column)
               (warn "No line or column info for top frame of debugger."))
             (let ((url (jss-script-url script)))
               (unless url
                 (warn "No url for script of top frame of debugger.")
                 (return))
               (pushnew (list url line column) jss-debugger-resume-points
                        :test 'equal)))))))))

;;; TODO: jss-debugger-resume-points (the opposite of breakpoints)

(defcustom jss-debugger-auto-resume-functions '()
  "List of functions to call on a new exception, if any of them
  return true the exception is automatically resumed."
  :group 'jss)

;(pushnew 'jss-is-jquery-exception jss-ignorable-exception-functions)
;(pushnew 'jss-is-3rd-party-exception jss-ignorable-exception-functions)

(defun jss-with-first-stack-frame-url (jss-debugger thunk)
  "Calls thunk passing in the url of the top frame in
jss-debugger (when/if we get that information from the serevr and
the url is a real url)."
  (lexical-let ((thunk thunk)
                (jss-debugger jss-debugger))
    (let ((frames (jss-debugger-stack-frames jss-debugger)))
      (if (first frames)
          (jss-deferred-then
           (jss-frame-get-source-location (first frames))
           (lambda (location)
             (let* ((script (first location)))
               (when script
                 (let ((url (jss-script-url script)))
                   (when url
                     (funcall thunk url jss-debugger)))))))
        (warn "No first stack frame.")))))

(defun jss-is-jquery-exception (jss-debugger)
  "Tests if the url of the topmost frame of jss-debugger is a
version of jquery."
  (jss-with-first-stack-frame-url
   jss-debugger
   (lambda (url debugger)
     (save-match-data
       (if (string-match ".*/jquery[-.0-9]*\\(\\.min\\)?\\.js$" url)
           (format "%s looks like a jquery library." url)
         nil)))))

(defun jss-is-3rd-party-exception (jss-debugger)
  "Tests if the url of the topmost frame of jss-debugger is on a
different host that the current tab."
  (jss-with-first-stack-frame-url
   jss-debugger
   (lambda (script-url debugger)
     (let ((tab-url (jss-tab-url (jss-debugger-tab debugger))))
       (when (and script-url tab-url
                  (not (string= "" script-url))
                  (not (string= "" tab-url)))
         (let ((script-parsed (url-generic-parse-url script-url))
               (tab-parsed (url-generic-parse-url tab-url)))
           (if (string= (url-host tab-parsed)
                        (url-host script-parsed))
               nil
             (format "%s is external to %s" script-url tab-url))))))))

(setf jss-debugger-auto-resume-functions (list 'jss-is-jquery-exception
                                               'jss-is-3rd-party-exception))

(defun jss-debugger-call-auto-resumes (jss-debugger)
  "Call the auto-resume functions to see if we should just abort this debugger.

NB: May kill the current buffer. Callers should check the state
of the debugger after this function returns."
  ;; nb: some of the async code run to compute 'should ve resume' may
  ;; change buffers (it shouldn't, but i haven't yet figured out why),
  ;; so grab the buffer here before running that other code.
  (lexical-let ((buffer (current-buffer))
                (jss-debugger jss-debugger))
    (dolist (func jss-debugger-auto-resume-functions)
      (lexical-let* ((func func)
                     (cond (funcall func jss-debugger))
                     (handler (lambda (value)
                                (when value
                                  (when (buffer-live-p buffer)
                                    (with-current-buffer buffer
                                      (if (stringp value)
                                          (jss-console-warn-message (jss-tab-console (jss-debugger-tab jss-debugger))
                                                                    "%s triggered auto-resume: %s"
                                                                    func
                                                                    value)
                                        (jss-console-warn-message (jss-tab-console (jss-debugger-tab jss-debugger))
                                                                  "%s triggered auto-resume: %s"
                                                                  func
                                                                  value))
                                      (jss-debugger-stepper-resume)))))))      
        ;; this code may actually trigger the handler immediately, so
        ;; the current buffer and debugger state may change.
        (save-excursion
          (if (jss-deferred-p cond)
              (jss-deferred-then cond handler)
            (prog1
                (funcall handler cond)
              ;; no need to keep checking other conditions
              (return))))))))

(defvar jss-debugger-prompt-map
  (let ((map (make-sparse-keymap)))
    (define-key map "C-c C-c" 'jss-frame-previous)
    map)
  "Extra keymap used when point is inside a prompt within a
jss-debugger buffer. Just provides an simple shortcut to jump out
of the prompt and back to the current frame's label (where the
normal debugger navigation commands are available).")

(defmethod jss-debugger-insert-frame ((frame jss-generic-stack-frame) count)
  "Inserts the data and overlays and keymaps for `frame`. `count`
is the current number of the frame (assumed to be unique to each
to each frame without a debugger buffer)."
  (lexical-let ((frame frame))
    (jss-wrap-with-text-properties (list 'jss-frame frame 'jss-frame-count count)
      (jss-toggling-visibility
       (lambda ()
         (jss-wrap-with-text-properties (list 'jss-frame-label t)
           (insert (format "Frame %d: " count))
           (jss-insert-with-highlighted-whitespace (jss-frame-function-name frame))
           (insert "\n")))
       (lambda ()
         (when (jss-frame-source-hint frame)
           (jss-wrap-with-text-properties (list 'jss-frame-source-position t)
             (insert "Source location: ")
             (jss-insert-with-highlighted-whitespace (jss-frame-source-hint frame))
             (insert "\n")))
         (jss-insert-prompt (lambda (text)
                              (jss-evaluate frame text))
                            :local-map jss-debugger-prompt-map)
         (jss-section-marker))))))

(defmacro define-jss-debugger-step-function (name method)
  `(defun ,name ()
     (interactive)
     (unless (jss-current-debugger)
       (error "No debugger in %s." (current-buffer)))
     
     (,method (jss-current-debugger))
     
     (when (buffer-live-p (current-buffer))
       (kill-buffer (current-buffer)))))

(define-jss-debugger-step-function jss-debugger-stepper-resume    jss-debugger-resume)
(define-jss-debugger-step-function jss-debugger-stepper-step-into jss-debugger-step-into)
(define-jss-debugger-step-function jss-debugger-stepper-step-over jss-debugger-step-over)
(define-jss-debugger-step-function jss-debugger-stepper-step-out  jss-debugger-step-out)

(defun jss-frame-goto-source (force-server-side-js)
  "Open up a JSS Script buffer containing the source code for the current frame.

force-server-side-js will be passed to
jss-script-display-at-position (where it controls whether or not
we want to look for the local file containg the, editable, source
code."
  (interactive "P")
  (let ((frame (get-text-property (point) 'jss-frame)))
    (unless frame
      (error "No frame at point."))
    (jss-deferred-add-backs
     (jss-frame-get-source-location frame)
     (lambda (location)
       (apply 'jss-script-display-at-position (append location (list force-server-side-js)))))))

(defun jss-frame-parts-locations (point)
  (let ((frame (get-text-property (point) 'jss-frame)))
    (unless frame
      (error "No frame at point."))
    (save-excursion
      (destructuring-bind (start . end)
          (jss-find-property-block 'jss-frame frame :test 'eq)
        (goto-char start)
        (list frame
              :start start :end end
              :frame-label-start (save-excursion (jss-start-of-next-property-block 'jss-frame-label))
              :frame-label-end   (save-excursion (jss-end-of-current-property-block 'jss-frame-label))))))) 

(defmacro* with-frame-at-point ((frame &rest location-args) &body body)
  (declare (indent 1))
  `(destructuring-bind (,frame ,@location-args)
       (jss-frame-parts-locations (point))
     ,@body))

(defun jss-frame-next ()
  "Move point to the next frame in the buffer."
  (interactive)
  (let* ((current-count (get-text-property (point) 'jss-frame-count))
         (next-id (if current-count
                      (mod (1+ current-count) jss-debugger-num-frames)
                    0)))
    (goto-char (car (jss-find-property-block 'jss-frame-count next-id)))))

(defun jss-frame-previous ()
  "Move point to the previous frame in the buffer."
  (interactive)
  (let* ((current-count (get-text-property (point) 'jss-frame-count))
         (next-id (if current-count
                      (if (= current-count 0)
                          (1- jss-debugger-num-frames)
                        (1- current-count))
                    0)))
    (goto-char (car (jss-find-property-block 'jss-frame-count next-id)))))

(defun jss-frame-goto-exception ()
  (interactive)
  (goto-char
   (car (jss-find-property-block 'jss-debugger-exception t :test 'eq))))

(defun jss-debugger-stepper-frame-restart ()
  (interactive)
  (lexical-let ((frame (get-text-property (point) 'jss-frame))
                (current-buffer (current-buffer)))
    (unless frame
      (error "No frame at point."))
    (jss-deferred-then
     (jss-frame-restart frame)
     (lambda (result)
       (with-current-buffer current-buffer
         (message "Killing debugger buffer.")
         (jss-debugger-kill))))))

(defun jss-debugger-kill ()
  (interactive)
  (jss-debugger-cleanup (jss-current-debugger)))

(defun jss-debugger-frame-goto-prompt ()
  (interactive)
  t)

(provide 'jss-debugger)
