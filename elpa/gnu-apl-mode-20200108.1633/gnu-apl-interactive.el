;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'gnu-apl-util)
(require 'gnu-apl-network)

(defvar gnu-apl-current-session nil
  "The buffer that holds the currently active GNU APL session,
or NIL if there is no active session.")

(defvar gnu-apl-libemacs-location "libemacs"
  "The location of the native code library from the interpreter.
This shouldn't normally need to be changed except when doing
development of the native code.")

(defun gnu-apl-interactive-send-string (string &optional file line)
  "Send STRING to the current active interpreter.
If given, FILE and LINE indicates the file and location where the
code was read from."
  (let ((content (split-string string "\n")))
    (gnu-apl--send-network-command (concat "sendcontent"
                                           (if (and file line)
                                               (format ":%s:%d" file line)
                                             "")))
    (gnu-apl--send-block content)
    (let ((reply (gnu-apl--read-network-reply-block)))
      (if (string= (car reply) "content sent")
          (progn
            (gnu-apl--send (gnu-apl--get-interactive-session) "⊣⍬\n")
            (message "Content sent to APL interpreter"))
        (error "Error sending content to APL interpreter")))))

(defun gnu-apl--get-interactive-session-with-nocheck ()
  (when gnu-apl-current-session
    (let ((proc-status (comint-check-proc gnu-apl-current-session)))
      (when (eq (car proc-status) 'run)
        gnu-apl-current-session))))

(defun gnu-apl--get-interactive-session ()
  (let ((session (gnu-apl--get-interactive-session-with-nocheck)))
    (unless session
      (user-error "No active GNU APL session"))
    session))

(defvar *gnu-apl-native-lib* "EMACS_NATIVE")
(defvar *gnu-apl-ignore-start* "IGNORE-START")
(defvar *gnu-apl-ignore-end* "IGNORE-END")
(defvar *gnu-apl-network-start* "NATIVE-STARTUP-START")
(defvar *gnu-apl-network-end* "NATIVE-STARTUP-END")

(defun gnu-apl--send (proc string)
  "Filter for any commands that are sent to comint."
  (let* ((trimmed (gnu-apl--trim-spaces string)))
    (cond ((and gnu-apl-auto-function-editor-popup
                (plusp (length trimmed))
                (string= (subseq trimmed 0 1) "∇"))
           ;; The command is a function definition command
           (unless (gnu-apl--parse-function-header (subseq trimmed 1))
             (user-error "Error when parsing function definition command"))
           (unwind-protect
               (gnu-apl--get-function (gnu-apl--trim-spaces (subseq string 1)))
             (let ((buffer (process-buffer proc)))
               (with-current-buffer buffer
                 (let ((inhibit-read-only t))
                   (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char (process-mark proc))
                       (insert "      ")
                       (set-marker (process-mark proc) (point))))))))
           nil)

          (t
           ;; Default, simply pass the input to the process
           (comint-simple-send proc string)))))

(defun gnu-apl--set-face-for-parsed-text (start end mode string)
  (case mode
    (cerr (add-text-properties start end '(font-lock-face gnu-apl-error) string))
    (uerr (add-text-properties start end '(font-lock-face gnu-apl-user-status-text) string))))

(defun gnu-apl--parse-text (string)
  (let ((tags nil))
    (let ((result (with-output-to-string
                    (loop with current-mode = gnu-apl-input-display-type
                          with pos = 0
                          for i from 0 below (length string)
                          for char = (aref string i)
                          for newmode = (case char
                                          (#xf00c0 'cin)
                                          (#xf00c1 'cout)
                                          (#xf00c2 'cerr)
                                          (#xf00c3 'uerr)
                                          (t nil))
                          if (and newmode (not (eq current-mode newmode)))
                          do (progn
                               (push (list pos newmode) tags)
                               (setq current-mode newmode))
                          unless newmode
                          do (progn
                               (princ (char-to-string char))
                               (incf pos))))))
      (let ((prevmode gnu-apl-input-display-type)
            (prevpos 0))
        (loop for v in (reverse tags)
              for newpos = (car v)
              unless (= prevpos newpos)
              do (gnu-apl--set-face-for-parsed-text prevpos newpos prevmode result)
              do (progn
                   (setq prevpos newpos)
                   (setq prevmode (cadr v))))
        (unless (= prevpos (length result))
          (gnu-apl--set-face-for-parsed-text prevpos (length result) prevmode result))
        (setq gnu-apl-input-display-type prevmode)
        result))))

(defun gnu-apl--erase-and-set-function (name content)
  (gnu-apl-interactive-send-string (concat "'" *gnu-apl-ignore-start* "'"))
  (gnu-apl-interactive-send-string (concat ")ERASE " name))
  (gnu-apl-interactive-send-string (concat "'" *gnu-apl-ignore-end* "'"))
  (setq gnu-apl-function-content-lines (split-string content "\n"))
  (gnu-apl-interactive-send-string (concat "'" *gnu-apl-send-content-start* "'")))

(defun gnu-apl--output-disconnected-message (output-fn)
  (funcall output-fn "The GNU APL environment has been started, but the Emacs mode was
unable to connect to the backend. Because of this, some
functionality will not be available, such as the external
function editor.
      "))

(defun gnu-apl--preoutput-filter (line)
  (let ((result "")
        (first t))

    (cl-labels ((add-to-result (s)
                  (if first
                      (setq first nil)
                    (setq result (concat result "\n")))
                  (setq result (concat result s)))
                (do-connect (mode addr command)
                  (gnu-apl--connect mode addr)
                  (message "Connected to APL interpreter")
                  ;; starting from remote protocol version 1.6 the HELP command is
                  ;; available to retrieve all symbols help
                  (when (version< "1.5" *gnu-apl-remote-protocol*)
                    (setf gnu-apl--symbol-doc (gnu-apl--load-help)))
                  (add-to-result command)))
      (dolist (plain (split-string line "\n"))
        (let ((command (gnu-apl--parse-text plain)))
          (ecase gnu-apl-preoutput-filter-state
            ;; Default parse state
            (normal
             (cond ((string-match (regexp-quote *gnu-apl-ignore-start*) command)
                    (setq gnu-apl-preoutput-filter-state 'ignore))

                   ((and (not gnu-apl-use-new-native-library) ; Backwards compatibility
                         (string-match (regexp-quote *gnu-apl-network-start*) command))
                    (setq gnu-apl-preoutput-filter-state 'native))

                   ((and gnu-apl-use-new-native-library
                         (or (not (boundp 'gnu-apl--connection))
                             (not (process-live-p gnu-apl--connection)))
                         (string-match (concat "Network listener started.*"
                                               "mode:\\([a-z]+\\) "
                                               "addr:\\([a-zA-Z0-9_/]+\\)")
                                       command))
                    (let ((mode (match-string 1 command))
                          (addr (match-string 2 command)))
                      (do-connect mode addr command)))
                   (t
                    (add-to-result command))))

            ;; Ignoring output
            (ignore
             (cond ((string-match (regexp-quote *gnu-apl-ignore-end*) command)
                    (setq gnu-apl-preoutput-filter-state 'normal))
                   (t
                    nil)))

            ;; Initialising native code
            (native
             (cond ((string-match (regexp-quote *gnu-apl-network-end*) command)
                    (unless (and (boundp 'gnu-apl--connection)
                                 gnu-apl--connection
                                 (process-live-p gnu-apl--connection))
                      (gnu-apl--output-disconnected-message #'add-to-result))
                    (setq gnu-apl-preoutput-filter-state 'normal))
                   ((string-match (concat "Network listener started.*"
                                          "mode:\\([a-z]+\\) "
                                          "addr:\\([a-zA-Z0-9_/]+\\)")
                                  command)
                    (let ((mode (match-string 1 command))
                          (addr (match-string 2 command)))
                      (do-connect mode addr command)))
                   (t
                    (add-to-result command))))))))
    result))

(defun gnu-apl--make-interactive-mode-map ()
  (let ((map (gnu-apl--make-base-mode-map gnu-apl-interactive-mode-map-prefix)))
    (define-key map (kbd "TAB") 'completion-at-point)
    (define-key map (kbd "C-c C-f") 'gnu-apl-edit-function)
    (define-key map (kbd "C-c C-v") 'gnu-apl-edit-variable)
    (define-key map (kbd "C-c C-m") 'gnu-apl-plot-line)
    (define-key map [menu-bar gnu-apl edit-function] '("Edit function" . gnu-apl-edit-function))
    (define-key map [menu-bar gnu-apl edit-matrix] '("Edit variable" . gnu-apl-edit-variable))
    (define-key map [menu-bar gnu-apl plot-line] '("Plot line graph of variable content" . gnu-apl-plot-line))
    map))

(defun gnu-apl--set-interactive-mode-map-prefix (symbol new)
  "Recreate the prefix and the keymap."
  (set-default symbol new)
  (setq gnu-apl-interactive-mode-map (gnu-apl--make-interactive-mode-map)))

(defcustom gnu-apl-interactive-mode-map-prefix "s-"
  "The keymap prefix for ‘gnu-apl-interactive-mode-map’ used both to store the new value
using ‘set-create’ and to update ‘gnu-apl-interactive-mode-map’ using
‘gnu-apl--make-interactive-mode-map’. Kill and re-start your interactive APL
buffers to reflect the change."
  :type 'string
  :group 'gnu-apl
  :set 'gnu-apl--set-interactive-mode-map-prefix)

(defvar gnu-apl-interactive-mode-map (gnu-apl--make-interactive-mode-map)
  "The keymap for ‘gnu-apl-interactive-mode'.")

(define-derived-mode gnu-apl-interactive-mode comint-mode "GNU-APL/Comint"
  "Major mode for interacting with GNU APL."
  :syntax-table gnu-apl-mode-syntax-table
  :group 'gnu-apl
  (use-local-map gnu-apl-interactive-mode-map)
  (gnu-apl--init-mode-common)

  (setq comint-prompt-regexp "^\\(      \\)\\|\\(\\[[0-9]+\\] \\)")
  (setq-local gnu-apl-preoutput-filter-state 'normal)
  (setq-local gnu-apl-input-display-type 'cout)
  (setq-local comint-input-sender 'gnu-apl--send)
  (setq-local gnu-apl-trace-symbols nil)
  (add-hook 'comint-preoutput-filter-functions 'gnu-apl--preoutput-filter nil t)

  (setq font-lock-defaults '(nil t)))

(defun gnu-apl-open-customise ()
  "Open the customisation editor for the gnu-apl customisation group."
  (interactive)
  (customize-group 'gnu-apl t))

(defun gnu-apl--insert-tips ()
  (insert "This is the gnu-apl-mode interactive buffer.\n\n"
          "To toggle keyboard help, call M-x gnu-apl-show-keyboard (C-c C-k by default).\n"
          "APL symbols are bound to the standard keys with the Super key. You can also\n"
          "activate the APL-Z ")
  (insert-button "input method"
                 'action 'toggle-input-method
                 'follow-link t)
  (insert " (M-x toggle-input-method or C-\\) which\n"
          "allows you to input APL symbols by prefixing the key with a \".\" (period).\n\n"
          "There are several ")
  (insert-button "customisation"
                 'action #'(lambda (event) (customize-group 'gnu-apl t))
                 'follow-link t)
  (insert " options that can be set.\n"
          "Click the link or run M-x customize-group RET gnu-apl to set up.\n\n"
          "To disable this message, set gnu-apl-show-tips-on-start to nil.\n\n"))


(defun gnu-apl-find-function-at-point ()
  "Jump to the definition of the function at point."
  (interactive)
  (let ((name (gnu-apl--name-at-point)))
    (let ((resolved-name (if (and name (string-match "[a-zA-Z_∆⍙][a-zA-Z0-9_∆⍙¯]*" name))
                             name
                           (gnu-apl--choose-variable "Function" :function))))
      (gnu-apl--send-network-command (concat "functiontag:" resolved-name))
      (let ((result (gnu-apl--read-network-reply-block)))
        (if (not (string= (car result) "tag"))
            (message "No function definition found")
          (let ((reference (cadr result)))
            (cond ((string-match "^\\(.*\\):\\([0-9]+\\)$" reference)
                   (let ((file (match-string 1 reference))
                         (line-num (string-to-number (match-string 2 reference))))
                     (ring-insert find-tag-marker-ring (point-marker))
                     (let ((buffer (find-buffer-visiting file)))
                       (if buffer
                           (let ((window (get-buffer-window buffer)))
                             (if window
                                 (select-window window)
                               (switch-to-buffer buffer)))
                         (find-file-existing file)))
                     (gnu-apl--move-to-line line-num)))
                  ((string-match "^⎕FX$" reference)
                   (message "%s: No source information" resolved-name))
                  (t
                   (error "Unexpected tag format: %S" reference)))))))))

(defun gnu-apl-switch-to-interactive ()
  "Switch to the GNU APL interaction buffer if it has been started."
  (interactive)
  (let ((buffer (gnu-apl--get-interactive-session)))
    (pop-to-buffer buffer)
    (goto-char (point-max))))

(provide 'gnu-apl-interactive)
