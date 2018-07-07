;;; mu4e-conversation.el --- Show a complete thread in a single buffer -*- lexical-binding: t -*-

;; Copyright (C) 2018 Pierre Neidhardt <ambrevar@gmail.com>

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://gitlab.com/Ambrevar/mu4e-conversation
;; Package-Version: 20180627.545
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: mail, convenience, mu4e

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package offers an alternate view to `mu4e' e-mail display.  It shows all
;; e-mails of a thread in a single view, where each correspondant has their own
;; face.  Threads can be displayed linearly (in which case e-mails are displayed
;; in chronological order) or as an Org document where the node tree maps the
;; thread tree.
;;
;; * Setup
;;
;; From the headers view, call `M-x mu4e-conversation'.
;;
;; To fully replace `mu4e-view' with `mu4e-conversation' from any other command
;; (e.g. `mu4e-headers-next', `helm-mu'), call
;;
;;   (global-mu4e-conversation-mode)
;;
;; * Features
;;
;; Call `mu4e-conversation-toggle-view' (bound to "V" by default) to switch between
;; linear and tree view.
;;
;; The last section is writable.
;;
;; Call `mu4e-conversation-send' ("C-c C-c" by default) to send the message.
;;
;; When the region is active anywhere in the thread, `mu4e-conversation-cite'
;; ("<return>" by default) will append the selected text as citation to the
;; message being composed.
;;
;; Each conversation gets its own buffer.


;;; Code:

;; TODO: Overrides are not commended.  Use unwind-protect to set handlers?  I don't think it would work.
;; TODO: Only mark visible messages as read.
;; TODO: Detect subject changes.
;; TODO: Check out mu4e gnus view.
;; TODO: Should we reply to the selected message or to the last?  Make it an option: 'current, 'last, 'ask.
;; TODO: Does toggle-display HTML work?
;; TODO: Save using "save-buffer"?  This would allow different bindings to work
;; transparently (e.g. ":w" with Evil).  Problem is that the draft buffer and
;; the conversation view are different buffers.

;; TODO: Views should be structure with
;; - Thread sort function
;; - Previous / next function
;; - Print function
;; TODO: Indent user messages?  Make formatting more customizable.
;; TODO: Tweak Org indentation?  See `org-adapt-indentation'.

;; TODO: Sometimes messages are not marked as read.
;; TODO: Auto-update conversation buffer when receiving/sending mail.
;; TODO: Add convenience functions to check if some recipients have been left out, or to return the list of all recipients.
;; TODO: Mark/flag messages that are in thread but not in headers buffer.  See `mu4e-mark-set'.
;; TODO: Fine-tune the recipient list display and composition in linear view.
;; In tree view, we could read properties from the composition subtree.

;; TODO: Evil mode: Preserve normal-state bindings when returning from composition.
;; TODO: `org-open-line'(?) and `evil-open-below' remove the local-map from the
;; text properties.  Solution would be as for the above Evil issue: define
;; "special-<kbd>" bindings such when read-only, act special, otherwise act
;; normal.
;; Check out `org-mu4e-compose-org-mode'.

(require 'mu4e)
(require 'rx)
(require 'outline)
(require 'org)
(require 'subr-x)

(defvar mu4e-conversation--thread-headers nil)
(defvar mu4e-conversation--thread nil)
(defvar mu4e-conversation--current-message nil)

(defvar mu4e-conversation-print-message-function 'mu4e-conversation-print-message-linear
  "Function that insert the formatted content of a message in the current buffer.
The argument is the message index in `mu4e-conversation--thread',
counting from 0.")

(defvar mu4e-conversation--is-view-buffer nil
  "Tell whether current buffer is a conversation view.")
(make-variable-buffer-local 'mu4e-conversation--is-view-buffer)

(defgroup mu4e-conversation nil
  "Settings for the mu4e conversation view."
  :group 'mu4e)

(defcustom mu4e-conversation-own-name "Me"
  "Name to display instead of your own name.
This applies to addresses matching `mu4e-user-mail-address-list'.
If nil, the name value is not substituted."
  :type 'string
  :group 'mu4e-conversation)

(defcustom mu4e-conversation-buffer-name-format "*mu4e-view-%s*"
  "Format of the conversation buffer name.
'%s' will be replaced by the buffer name."
  :type 'string
  :group 'mu4e-conversation)

(defcustom mu4e-conversation-send-hook nil
  "A hook run before sending messages.
For example, to disable appending signature at the end of a message:

  (add-hook
   'mu4e-conversation-send-hook
   (lambda ()
     (set (make-local-variable 'mu4e-compose-signature-auto-include) nil)))
"
  :type 'hook
  :group 'mu4e-conversation)

(defcustom mu4e-conversation-use-citation-line nil
  "If non-nil, precede each citation with a line as per
`mu4e-conversation-citation-line-function'."
  :type 'boolean
  :group 'mu4e-conversation)

(defcustom mu4e-conversation-citation-line-function 'mu4e-conversation-insert-citation-line
  "Function called to insert the \"Whomever writes:\" line."
  :type '(choice
	  (function-item :tag "default" mu4e-conversation-insert-citation-line)
	  (function :tag "Other"))
  :group 'mu4e-conversation)

(defface mu4e-conversation-unread
  '((t :weight bold))
  "Face for unread messages."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-me
  '((t :inherit default))
  "Face for conversation message sent by yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-1
  `((t :foreground ,(face-foreground 'outline-1)))
  "Face for conversation message from the 1st sender who is not yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-2
  `((t :foreground ,(face-foreground 'outline-2)))
  "Face for conversation message from the 2rd sender who is not yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-3
  `((t :foreground ,(face-foreground 'outline-3)))
  "Face for conversation message from the 3rd sender who is not yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-4
  `((t :foreground ,(face-foreground 'outline-4)))
  "Face for conversation message from the 4th sender who is not yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-5
  `((t :foreground ,(face-foreground 'outline-5)))
  "Face for conversation message from the 5th sender who is not yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-6
  `((t :foreground ,(face-foreground 'outline-6)))
  "Face for conversation message from the 6th sender who is not yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-7
  `((t :foreground ,(face-foreground 'outline-7)))
  "Face for conversation message from the 7th sender who is not yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-8
  `((t :foreground ,(face-foreground 'outline-8)))
  "Face for conversation message from the 8th sender who is not yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-header
  '((t :foreground "grey70" :background "grey25"))
  "Face for conversation message sent by someone else."
  :group 'mu4e-conversation)

(defcustom mu4e-conversation-max-colors -1
  "Max number of colors to use to colorize sender messages.
If 0, don't use colors.
If less than 0, don't limit the number of colors."
  :type 'integer
  :group 'mu4e-conversation)

(defvar mu4e-conversation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "V") 'mu4e-conversation-toggle-view)
    (define-key map (kbd "#") 'mu4e-conversation-toggle-hide-cited)
    map)
  "Map for `mu4e-conversation'.")

(defvar mu4e-conversation-compose-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map global-map)
    (define-key map (kbd "C-c C-c") 'mu4e-conversation-send)
    (define-key map (kbd "C-x C-s") 'mu4e-conversation-save)
    (define-key map (kbd "C-c C-p") 'mu4e-conversation-previous-message)
    (define-key map (kbd "C-c C-n") 'mu4e-conversation-next-message)
    map)
  "Map for `mu4e-conversation' in compose area.")

(defvar mu4e-conversation-linear-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") 'mu4e-conversation-cite)
    (define-key map (kbd "C-c C-c") 'mu4e-conversation-send)
    (define-key map (kbd "C-x C-s") 'mu4e-conversation-save)
    (define-key map (kbd "C-c C-p") 'mu4e-conversation-previous-message)
    (define-key map (kbd "C-c C-n") 'mu4e-conversation-next-message)
    (define-key map (kbd "M-q") 'mu4e-conversation-fill-long-lines)
    (define-key map (kbd "q") 'mu4e-conversation-quit)
    map)
  "Map for `mu4e-conversation' in linear view.")

(defvar mu4e-conversation-tree-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'mu4e-conversation-quit)
    (define-key map (kbd "C") 'mu4e-compose-new)
    (define-key map (kbd "R") 'mu4e-compose-reply)
    (define-key map (kbd "E") 'mu4e-compose-edit)
    (define-key map (kbd "F") 'mu4e-compose-forward)
    (define-key map (kbd ".") 'mu4e-view-raw-message)
    (define-key map (kbd "A") 'mu4e-view-attachment-action)
    (define-key map (kbd "a") 'mu4e-view-action)
    (define-key map (kbd "|") 'mu4e-view-pipe)
    (define-key map (kbd "M-q") 'mu4e-conversation-fill-long-lines)
    map)
  "Map for `mu4e-conversation' in tree view.")

(defun mu4e-conversation-fill-long-lines ()
  "Same as `mu4e-view-fill-long-lines' but does not change the modified state."
  (interactive)
  (unless mu4e-conversation--is-view-buffer
    (mu4e-warn "Not a conversation buffer"))
  (let ((modified-p (buffer-modified-p))
        (mu4e~view-buffer-name (buffer-name)))
    (set-buffer-modified-p nil)         ; Don't warn if modified.
    (mu4e-view-fill-long-lines)
    (set-buffer-modified-p modified-p)))

(defun mu4e-conversation-set-attachment (&optional msg)
  "Call before attachment
functions (e.g. `mu4e-view-save-attachment-multi') so that it
works for message at point.  Suitable as a :before advice."
  (interactive)
  (unless mu4e-conversation--is-view-buffer
    (mu4e-warn "Not a conversation buffer"))
  (setq msg (or msg (mu4e-message-at-point)))
  (mu4e~view-construct-attachments-header msg))

(defun mu4e-conversation-previous-message (&optional count)
  "Go to previous message in linear view.
With numeric prefix argument or if COUNT is given, move that many
messages.  A negative COUNT goes forwards."
  (interactive "p")
  (mu4e-conversation-next-message (if count (- count) -1)))

(defun mu4e-conversation-next-message (&optional count)
  "Go to next message in linear view.
With numeric prefix argument or if COUNT is given, move that many
messages.  A negative COUNT goes backwards."
  (interactive "p")
  (unless mu4e-conversation--is-view-buffer
    (mu4e-warn "Not a conversation buffer"))
  (setq count (or count 1))
  (if (eq major-mode 'org-mode)
      (org-next-visible-heading count)
    (let ((move-function (if (< count 0)
                             'previous-char-property-change
                           'next-char-property-change)))
      (setq count (abs count))
      (dotimes (_ count)
        (while (and (goto-char (funcall move-function (point)))
                    (not (eq (get-text-property (point) 'face) 'mu4e-conversation-header))
                    (not (eobp))))))))

(defun mu4e-conversation-toggle-hide-cited ()
  "Toggle hiding of cited lines in the message body."
  (interactive)
  (if (and (listp buffer-invisibility-spec)
           (member '(mu4e-conversation-quote . t) buffer-invisibility-spec))
      (remove-from-invisibility-spec '(mu4e-conversation-quote . t))
    (add-to-invisibility-spec '(mu4e-conversation-quote . t)))
  (force-window-update))

(defun mu4e-conversation-kill-buffer-query-function ()
  "Ask before killing a modified mu4e conversation buffer."
  (or (not mu4e-conversation--is-view-buffer)
      (not (buffer-modified-p))
      (yes-or-no-p  "Reply message has been modified.  Kill anyway? ")))

(defun mu4e-conversation-quit (&optional no-confirm)
  "Quit conversation window.
If NO-CONFIRM is nil, ask for confirmation if message was not saved."
  ;; This function is useful as a replacement for `mu4e~view-quit-buffer': it
  ;; allows us to keep focus on the view buffer.
  (interactive)
  (unless mu4e-conversation--is-view-buffer
    (mu4e-warn "Not a conversation buffer"))
  (when (or no-confirm
            (not (buffer-modified-p))
            (yes-or-no-p "Reply message has been modified.  Kill anyway? "))
    ;; Don't ask for confirmation again in the `kill-buffer-query-functions'.
    (set-buffer-modified-p nil)
    ;; `mu4e~view-quit-buffer' must be called from a buffer in `mu4e-view-mode'.
    (unless (eq major-mode 'mu4e-view-mode)
      (mu4e-view-mode))
    ;; Change major mode reset the local variable and we need to let know
    ;; subsequent calls that this still is a conversation buffer.
    (setq mu4e-conversation--is-view-buffer t)
    (mu4e~view-quit-buffer)))

(defun mu4e-conversation-toggle-view ()
  "Switch between tree and linear view."
  (interactive)
  (unless mu4e-conversation--is-view-buffer
    (mu4e-warn "Not a conversation buffer"))
  (when (and buffer-undo-list
             (not (yes-or-no-p "Undo list will be reset after switching view.  Continue? ")))
    (mu4e-warn "Keeping undo list"))
  ;; Org properties skew line calculation, so remove it first.
  (let ((inhibit-read-only t)
        (block (org-get-property-block))
        (modified (buffer-modified-p))
        begin)
    (when block
      (save-excursion
        (goto-char (car block))
        (forward-line -1)
        (setq begin (point))
        (goto-char (cdr block))
        (forward-line 1)
        (delete-region begin (point)))
      (set-buffer-modified-p modified)))
  (let* ((current-message (mu4e-message-at-point 'no-error))
         (line-offset (save-excursion
                        (let ((current-line (line-number-at-pos)))
                          (mu4e-conversation-previous-message)
                          (if (or (not current-message)
                                  ;; current-message might be nil when point is in a draft.
                                  (eq current-message (mu4e-message-at-point 'no-error)))
                              (- current-line (line-number-at-pos))
                            0))))
         (column (- (point) (line-beginning-position))))
    (mu4e-conversation--show-thread
     (if (eq major-mode 'org-mode)
         'mu4e-conversation-print-message-linear
       'mu4e-conversation-print-message-tree))
    ;; Restore point.
    (if (not current-message)
        ;; Draft.
        (progn
          (goto-char (point-max))
          (mu4e-conversation-previous-message))
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (eq current-message (mu4e-message-at-point 'no-error))))
        (mu4e-conversation-next-message)))
    (let ((block (org-get-property-block))
          begin end)
      (when block
        (save-excursion
          (goto-char (car block))
          (forward-line -1)
          (setq begin (point))
          (goto-char (cdr block))
          (forward-line 1)
          (setq end (point))))
      (dotimes (_ line-offset)
        (forward-line)
        (when (and block
                   (<= begin (point) end))
          ;; If point meets an Org property block, skip it at once.
          (goto-char end))))
    (move-to-column column)))

(defun mu4e-conversation--body-without-signature (message)
  "Return the message body (a string) stripped from its signature."
  (with-temp-buffer
    (insert (mu4e-message-body-text message))
    (goto-char (point-min))
    (kill-whole-line) ; Skip MML line.  TODO: This is brittle, MML line is not necessarily on the first line.
    (message-goto-signature)
    (unless (eobp)
      (forward-line -1)
      (delete-region (point) (point-max)))
    (buffer-string)))

(defun  mu4e-conversation--get-buffer (&optional title)
  "Return conversation buffer.
This mimics the behaviour of `mu4e-get-view-buffer' but supports multiple
buffers.

- It TITLE is non-nil and return a buffer named (format
mu4e-conversation-buffer-name-format title) and create it if necessary.
- If current buffer is a conversation, return it.
- Otherwise get most recent buffer for which `mu4e-conversation--is-view-buffer'
is non-nil."
  (cond
   (title (get-buffer-create (format mu4e-conversation-buffer-name-format title)))
   (mu4e-conversation--is-view-buffer (current-buffer))
   (t (seq-find (lambda (b)
                  (with-current-buffer b
                    mu4e-conversation--is-view-buffer))
                (buffer-list)))))

(defun mu4e-converation--headers-redraw-get-view-window ()
  "Like `mu4e~headers-redraw-get-view-window' but preserve conversation buffers."
  (if (eq mu4e-split-view 'single-window)
      (or (and (buffer-live-p (mu4e-get-view-buffer))
	       (get-buffer-window (mu4e-get-view-buffer)))
	  (selected-window))
    (mu4e-hide-other-mu4e-buffers)
    (unless (buffer-live-p (mu4e-get-headers-buffer))
      (mu4e-error "No headers buffer available"))
    (switch-to-buffer (mu4e-get-headers-buffer))
    ;; kill the existing view window
    (when (and (buffer-live-p (mu4e-get-view-buffer))
               (with-current-buffer (mu4e-get-view-buffer)
                 mu4e-conversation--is-view-buffer))
      (let ((win (get-buffer-window (mu4e-get-view-buffer))))
        (when (eq t (window-deletable-p win))
          (delete-window win))))
    ;; get a new view window
    (setq mu4e~headers-view-win
     (let* ((new-win-func
	     (cond
	      ((eq mu4e-split-view 'horizontal) ;; split horizontally
	       '(split-window-vertically mu4e-headers-visible-lines))
	      ((eq mu4e-split-view 'vertical) ;; split vertically
	       '(split-window-horizontally mu4e-headers-visible-columns)))))
       (cond ((with-demoted-errors "Unable to split window: %S"
		(eval new-win-func)))
	     (t ;; no splitting; just use the currently selected one
	      (selected-window)))))))

(defun mu4e-conversation--show-thread (&optional print-function)
  "Display the conversation in BUFFER.
If BUFFER is nil, buffer is as returned by `mu4e-conversation--get-buffer'.
If print-function is nil, use `mu4e-conversation-print-message-function'."
  ;; See the docstring of `mu4e-message-field-raw'.
  (setq print-function (or print-function mu4e-conversation-print-message-function))
  (switch-to-buffer (mu4e-conversation--get-buffer
                     (mu4e-message-field (car mu4e-conversation--thread) :subject)))
  (let* ((current-message-pos 0)
         (index 0)
         ;; let-bind the thread variables to preserve them when changing major modes.
         ;; We can make them buffer local once the major mode is set.
         (thread mu4e-conversation--thread)
         (thread-headers mu4e-conversation--thread-headers)
         ;; If we want to re-order a thread, let's do it on a copy so that we
         ;; don't lose the tree structure.
         (thread-sorted thread)
         (thread-headers-sorted thread-headers)
         (inhibit-read-only t)
         ;; Extra care must be taken to copy along the draft with its properties, in
         ;; case it wasn't saved.
         (draft-text (when (buffer-modified-p)
                       (buffer-substring (save-excursion
                                           (goto-char (point-max))
                                           (mu4e-conversation-previous-message)
                                           (forward-line)
                                           (point))
                                         (point-max))))
         (buffer-modified (buffer-modified-p))
         draft-messages)
    (when (eq print-function
              'mu4e-conversation-print-message-linear)
      ;; In linear view, it makes more sense to sort messages chronologically.
      (let ((filter (lambda (seq)
                      (sort (copy-seq seq)
                            (lambda (msg1 msg2)
                              (time-less-p (mu4e-message-field msg1 :date)
                                           (mu4e-message-field msg2 :date)))))))
        (setq thread-sorted (funcall filter thread)
              thread-headers-sorted (funcall filter thread-headers))))
    (erase-buffer)
    (delete-all-overlays)
    (dolist (msg thread-sorted)
      (if (member 'draft (mu4e-message-field msg :flags))
          (push msg draft-messages)
        (when (= (mu4e-message-field msg :docid)
                 (mu4e-message-field mu4e-conversation--current-message :docid))
          (setq current-message-pos (point)))
        (let ((begin (point)))
          (funcall print-function
                   index
                   thread-sorted
                   thread-headers-sorted)
          (mu4e~view-show-images-maybe msg)
          (goto-char (point-max))
          (add-text-properties begin (point) (list 'msg msg)))
        (insert (propertize "\n" 'msg msg)) ; Insert a final newline after potential images.
        (mu4e~view-mark-as-read-maybe msg)
        (goto-char (point-max)))
      (setq index (1+ index)))
    (add-text-properties (point-min) (point-max) '(read-only t))
    ;; Used as "marker" so that we can tell the buffer is a mu4e-conversation.
    ;; Must set this here for the rest of the functions to work,
    ;; e.g. `mu4e-conversation-previous-message'.
    (setq mu4e-conversation--is-view-buffer t)
    (insert (propertize (format "%sCompose new message:" (if (eq major-mode 'org-mode) "* NEW " ""))
                        'face 'mu4e-conversation-header 'read-only t)
            (propertize "\n"
                        'face 'mu4e-conversation-header
                        'rear-nonsticky t
                        'local-map mu4e-conversation-compose-map)
            (if draft-messages ""
              (propertize
               "\n"
               'local-map mu4e-conversation-compose-map
               'front-sticky t)))
    (cond
     (draft-text
      (save-excursion
        (goto-char (point-max))
        (mu4e-conversation-previous-message)
        (forward-line)
        (delete-region (point) (point-max))
        (insert draft-text)))
     (draft-messages
      ;; REVIEW: Discard signature.
      (add-text-properties
       (save-excursion (mu4e-conversation-previous-message)
                       (point))
       (point-max)
       (list 'msg (car draft-messages)))
      (if (= (length draft-messages) 1)
          (insert (propertize (mu4e-conversation--body-without-signature (car draft-messages))
                              'msg (car draft-messages)
                              'local-map mu4e-conversation-compose-map
                              'front-sticky t))
        (warn "Multiple drafts found.  You must clean up the drafts manually.")
        (let ((count 1))
          (dolist (draft draft-messages)
            (insert (propertize (concat (format "--Draft #%s--\n" count)
                                        (mu4e-conversation--body-without-signature draft))
                                'msg (car draft-messages) ; Use first draft file.
                                'local-map mu4e-conversation-compose-map
                                'front-sticky t))
            (setq count (1+ count)))))))
    (goto-char current-message-pos)
    (recenter)
    (unless (eq major-mode 'org-mode)
      (mu4e~view-make-urls-clickable))  ; TODO: Don't discard sender face.
    (setq header-line-format (propertize
                              (mu4e-message-field (car thread-sorted) :subject)
                              'face 'bold))
    (add-to-invisibility-spec '(mu4e-conversation-quote . t))
    ;; TODO: Undo history is not preserved accross redisplays.
    (set-buffer-modified-p buffer-modified)
    ;; Save the thread in for the current buffer.  This is useful for redisplays.
    (set (make-local-variable 'mu4e-conversation--thread) thread)
    (set (make-local-variable 'mu4e-conversation--thread-headers) thread-headers)
    (make-local-variable 'mu4e-conversation--current-message)
    (add-to-list 'kill-buffer-query-functions 'mu4e-conversation-kill-buffer-query-function)
    (buffer-disable-undo)               ; Reset `buffer-undo-list'.
    (buffer-enable-undo)))

(defun mu4e-conversation--get-message-face (index thread)
  "Map 'from' addresses to 'sender-N' faces in chronological
order and return corresponding face for e-mail at INDEX in
THREAD.
E-mails whose sender is in `mu4e-user-mail-address-list' are skipped."
  (let* ((message (nth index thread))
         (from (car (mu4e-message-field message :from)))
         ;; The e-mail address is not enough as key since automated messaging
         ;; system such as the one from github have the same address with
         ;; different names.
         (sender-key (concat (car from) (cdr from)))
         (sender-faces (make-hash-table :test 'equal))
         (face-index 1))
    (dotimes (i (1+ index))
      (let* ((msg (nth i thread))
             (from (car (mu4e-message-field msg :from)))
             (sender-key (concat (car from) (cdr from)))
             (from-me-p (member (cdr from) mu4e-user-mail-address-list)))
        (unless (or from-me-p
                    (gethash sender-key sender-faces))
          (when (or (not (facep (intern (format "mu4e-conversation-sender-%s" face-index))))
                    (< 0 mu4e-conversation-max-colors face-index))
            (setq face-index 1))
          (puthash sender-key
                   (intern (format "mu4e-conversation-sender-%s" face-index))
                   sender-faces)
          (setq face-index (1+ face-index)))))
    (gethash sender-key sender-faces)))

(defun mu4e-conversation--from-name (message)
  "Return a string describing the sender (the 'from' field) of MESSAGE."
  (let* ((from (car (mu4e-message-field message :from)))
         (from-me-p (member (cdr from) mu4e-user-mail-address-list)))
    (if (and mu4e-conversation-own-name from-me-p)
        mu4e-conversation-own-name
      (concat (car from)
              (when (car from) " ")
              (format "<%s>" (cdr from))))))

(defun mu4e-conversation--propertize-quote (message)
  "Trim the replied-to emails quoted at the end of message."
  (with-temp-buffer
    (insert message)
    (goto-char (point-min))
    ;; Regexp seemed to be doomed to kill performance here, so we do it manually
    ;; instead.  It's not much longer anyways.
    (let (start)
      (while (not (eobp))
        (while (and (not (eobp)) (not (= (following-char) ?>)))
          (forward-line))
        (unless (eobp)
          (setq start (point))
          (while (and (not (eobp)) (= (following-char) ?>))
            (forward-line))
          (unless (eobp)
            ;; Optional gap.
            (while (and (not (eobp))
                        (string-match (rx line-start (* (any space)) line-end)
                                      (buffer-substring-no-properties
                                                  (line-beginning-position)
                                                  (line-end-position))))
              (forward-line))
            (if (or (eobp)
                    (string-match (rx line-start "--" (* (any space)) line-end)
                                  (buffer-substring-no-properties
                                                (line-beginning-position)
                                                (line-end-position))))
                ;; Found signature or end of buffer, no need to continue.
                (goto-char (point-max))
              ;; Restart the loop.
              (setq start nil)))))
      (when start
        ;; Buffer functions like (point) return 1-based indices while string
        ;; functions use 0-based indices.
        (add-text-properties (1- start) (length message)
                             '(invisible mu4e-conversation-quote) message)))))

(defun mu4e-conversation-print-message-linear (index thread &optional _thread-headers)
  "Insert formatted message found at INDEX in THREAD."
  (unless (eq major-mode 'mu4e-view-mode)
    (mu4e-view-mode)
    (read-only-mode 0)
    (use-local-map (make-composed-keymap (list mu4e-conversation-linear-map mu4e-conversation-map)
                                         mu4e-view-mode-map)))
  (let* ((msg (nth index thread))
         (from (car (mu4e-message-field msg :from)))
         (from-me-p (member (cdr from) mu4e-user-mail-address-list))
         (sender-face (or (get-text-property (point) 'face)
                          (and from-me-p 'mu4e-conversation-sender-me)
                          (and (/= 0 mu4e-conversation-max-colors)
                               (mu4e-conversation--get-message-face index thread))
                          'default)))
    (insert (propertize (format "%s, %s %s\n"
                                (mu4e-conversation--from-name msg)
                                (current-time-string (mu4e-message-field msg :date))
                                (mu4e-message-field msg :flags))
                        'face 'mu4e-conversation-header)
            (or (mu4e~view-construct-attachments-header msg) "") ; TODO: Append newline?
            ;; TODO: Add button to display trimmed quote of current message only.
            (let ((s (mu4e-message-body-text msg)))
              (add-face-text-property 0 (length s) sender-face nil s)
              (mu4e-conversation--propertize-quote s)
              (when (memq 'unread (mu4e-message-field msg :flags))
                (add-face-text-property 0 (length s) 'mu4e-conversation-unread nil s))
              s))))

(defun mu4e-conversation--format-address-list (address-list)
  "Return ADDRESS-LIST as a string.
The list is in the following format:
  ((\"name\" . \"email\")...)"
  (mapconcat
   (lambda (addrcomp)
     (if (and message-recipients-without-full-name
              (string-match
               (regexp-opt message-recipients-without-full-name)
               (cdr addrcomp)))
         (cdr addrcomp)
       (if (car addrcomp)
           (message-make-from (car addrcomp) (cdr addrcomp))
         (cdr addrcomp))))
   address-list
   ", "))

(defun mu4e-conversation-print-message-tree (index thread thread-headers)
  "Insert Org-formatted message found at INDEX in THREAD."
  (unless (eq major-mode 'org-mode)
    (insert "#+SEQ_TODO: UNREAD READ NEW\n\n")
    (org-mode)
    (erase-buffer) ; TODO: Is it possible to set `org-todo-keywords' locally without this workaround?
    (use-local-map (make-composed-keymap (list mu4e-conversation-tree-map mu4e-conversation-map)
                                         org-mode-map)))
  (let* ((msg (nth index thread))
         (msg-header (nth index thread-headers))
         (level (plist-get (mu4e-message-field msg-header :thread) :level))
         (org-level (make-string (1+ level) ?*))
         body-start)
    ;; Header.
    (insert (format "%s %s%s, %s %s\n"
                    org-level
                    (if (memq 'unread (mu4e-message-field msg :flags))
                        "UNREAD "
                      "")
                    (mu4e-conversation--from-name msg)
                    (current-time-string (mu4e-message-field msg :date))
                    (mu4e-message-field msg :flags)))
    ;; Body
    (goto-char (point-max))
    (setq body-start (point))
    ;; TODO: Propertize HTML links.
    (insert (mu4e-message-body-text msg))
    ;; Prefix "*" at the beginning of lines with a space to prevent them
    ;; from being interpreted as Org sections.
    (goto-char body-start)
    (while (re-search-forward (rx line-start "*") nil t) (replace-match " *"))
    (goto-char body-start)
    (while (re-search-forward (rx line-start ">" (* blank)) nil t) (replace-match ": "))
    (goto-char body-start)
    (while (re-search-forward (rx line-start "--8<---------------cut here---------------start------------->8---") nil t)
      (replace-match "#+begin_src"))
    (goto-char body-start)
    (while (re-search-forward (rx line-start "--8<---------------cut here---------------end--------------->8---") nil t)
      (replace-match "#+end_src"))
    (goto-char (point-max))
    (org-set-property "To" (mu4e-conversation--format-address-list
                            (mu4e-message-field msg :to)))
    (when (mu4e-message-field msg :cc)
      (org-set-property "CC" (mu4e-conversation--format-address-list
                              (mu4e-message-field msg :cc))))
    (let ((attachments (mu4e~view-construct-attachments-header msg)))
      ;; TODO: Propertize attachments.
      (when attachments
        (org-set-property "Attachments" (replace-regexp-in-string "\n$" "" attachments)))
      (when (and (< (length (mu4e-message-field msg :to)) 2)
                 (not (mu4e-message-field msg :cc))
                 (not attachments))
        (save-excursion
          (goto-char (car (org-get-property-block)))
          (forward-line -1)
          (org-cycle))))))

(defun mu4e-conversation-insert-citation-line (&optional msg)
  "This is similar to `message-insert-citation-line' but takes a
mu4e message as argument."
  (setq msg (or msg (mu4e-message-at-point)))
  (concat
   (mu4e-conversation--format-address-list (mu4e-message-field msg :from))
   " writes:\n"))

(defun mu4e-conversation-cite (start end &optional toggle-citation-line)
  (interactive "r\nP")
  (unless mu4e-conversation--is-view-buffer
    (mu4e-warn "Not a conversation buffer"))
  (if (not (use-region-p))
      (mu4e-scroll-up)                  ; TODO: Call function associate to `this-command-key' in mu4e-view-mode / org-mode.
    (let ((text (buffer-substring-no-properties start end))
          (mu4e-conversation-use-citation-line (if toggle-citation-line
                                             (not mu4e-conversation-use-citation-line)
                                           mu4e-conversation-use-citation-line))
          (msg (mu4e-message-at-point)))
      (save-excursion
        (goto-char (point-max))
        (backward-char)
        (insert
         (propertize
          (concat
           "\n\n"
           (if (and mu4e-conversation-use-citation-line msg)
               (funcall mu4e-conversation-citation-line-function msg)
             "")
           "> "
          ;; TODO: Re-cite first line properly.
           (replace-regexp-in-string
            "\n" "\n> "
            text))
          'local-map mu4e-conversation-compose-map))))))

(defun mu4e-conversation--open-draft (&optional msg)
  "Open conversation composed message as a mu4e draft buffer.
This is a helper function for operations such as saving and sending."
  (unless mu4e-conversation--is-view-buffer
    (mu4e-warn "Not a conversation buffer"))
  (let ((mu4e-compose-in-new-frame nil)
        (body (save-excursion
                (goto-char (point-max))
                (mu4e-conversation-previous-message)
                (forward-line)
                (buffer-substring-no-properties (line-beginning-position 1) (point-max))))
        (draft-message (save-excursion
                         (goto-char (point-max))
                         (mu4e-conversation-previous-message)
                         (forward-line)
                         (mu4e-message-at-point 'noerror)))
        (msg (or msg
                 (mu4e-message-at-point 'noerror)
                 (save-excursion
                   (goto-char (point-max))
                   (mu4e-conversation-previous-message 2)
                   (mu4e-message-at-point)))))
    (when (string-blank-p
           (replace-regexp-in-string
            (rx string-start ">" (* not-newline))
            ""
            (replace-regexp-in-string (rx "\n>" (* not-newline)) "" body)))
      (mu4e-warn "Empty or citation-only message"))
    ;; Pick context from parent message.  This is important if the user
    ;; configuration sets variable like `smtpmail-smtp-user' in a context.
    (mu4e~context-autoswitch msg
                             mu4e-compose-context-policy)
    ;; `mu4e-compose-pre-hook' can be use to, for instance, set the signature.
    (run-hooks 'mu4e-compose-pre-hook)
    (if draft-message
        (mu4e-draft-open 'edit draft-message)
      ;; Advice mu4e~draft-reply-all-p so that we don't get prompted and always "reply to all".
      ;; TODO: Protect the advice so that it gets remove cleanly even in case of error.
      (advice-add 'mu4e~draft-reply-all-p :override 'mu4e-conversation--draft-reply-all-p)
      (mu4e-draft-open 'reply msg)
      (advice-remove 'mu4e~draft-reply-all-p 'mu4e-conversation--draft-reply-all-p))
    (mu4e~draft-insert-mail-header-separator)
    (mu4e-compose-mode)
    (message-goto-body)
    (forward-line) ; Skip MML line.  TODO: This is brittle, MML line is not necessarily on the first line.
    ;; Delete citation:
    (delete-region (point) (save-excursion
                             (message-goto-signature)
                             (if (eobp)
                                 (point)
                               (forward-line -2)
                               (point))))
    (insert body)))

(defun mu4e-conversation-send (&optional msg)
  "Send message at the end of the view buffer.
If MSG is specified, then send this message instead."
  (interactive)
  (unless mu4e-conversation--is-view-buffer
    (mu4e-warn "Not a conversation buffer"))
  (let (draft-buf)
    (run-hooks 'mu4e-conversation-send-hook)
    (save-window-excursion
      (mu4e-conversation--open-draft msg)
      (condition-case nil
          (message-send-and-exit)
        ;; Stay in draft buffer and widen in case we failed during header check.
        (error (setq draft-buf (current-buffer))
               (widen))))
    (if draft-buf
        (switch-to-buffer draft-buf)
      (mu4e-conversation-quit 'no-confirm))))

;; TODO: Can we do better than a global?  We could use `mu4e-get-view-buffer'
;; but that would only work if the buffer has not been renamed.
(defvar mu4e-conversation--draft-msg nil)
(defun mu4e-conversation--update-draft (msg _)
  "Handler for `mu4e-update-func' to get the msg structure corresponding to the saved draft."
  (setq mu4e-conversation--draft-msg msg))

(defun mu4e-conversation-save (&optional msg)
  "Save conversation draft."
  (interactive)
  (unless mu4e-conversation--is-view-buffer
    (mu4e-warn "Not a conversation buffer"))
  (unless (buffer-modified-p)
    (mu4e-warn "(No changes need to be saved)"))
  (let ((composition-start (save-excursion
                             (goto-char (point-max))
                             (mu4e-conversation-previous-message)
                             (forward-line)
                             (point)))
        (draft-message (save-excursion
                         (goto-char (point-max))
                         (mu4e-conversation-previous-message)
                         (forward-line)
                         (mu4e-message-at-point 'noerror))))
    (save-window-excursion
      (mu4e-conversation--open-draft msg)
      (unless draft-message
        (advice-add mu4e-update-func :override 'mu4e-conversation--update-draft))
      (save-buffer)
      (unless draft-message
        (advice-remove mu4e-update-func 'mu4e-conversation--update-draft))
      (kill-buffer))
    (unless draft-message
      ;; We need to add the newly created draft to the 'msg property, otherwise
      ;; every subsequent save would create a new draft.
      (add-text-properties composition-start (point-max)
                           (list 'msg mu4e-conversation--draft-msg)))
    (set-buffer-modified-p nil)))

(defun mu4e-conversation--draft-reply-all-p (&optional _origmsg)
  "Override of `mu4e~draft-reply-all-p' to always reply to all."
  t)

(defun mu4e-conversation--view-handler (msg)
  "Handler function for displaying a message."
  (push msg mu4e-conversation--thread)
  (when (= (length mu4e-conversation--thread)
           (length mu4e-conversation--thread-headers))
    (advice-remove mu4e-view-func 'mu4e-conversation--view-handler)
    ;; Headers are collected in reverse order, let's re-order them.
    (setq mu4e-conversation--thread-headers (nreverse mu4e-conversation--thread-headers))
    (let ((viewwin (mu4e~headers-redraw-get-view-window)))
      (unless (window-live-p viewwin)
        (mu4e-error "Cannot get a conversation window"))
      (select-window viewwin))
    (mu4e-conversation--show-thread)))

(defun mu4e-conversation--header-handler (msg)
  "Store thread messages.
The header handler is run for all messages before the found-handler.
See `mu4e~proc-filter'"
  (push msg mu4e-conversation--thread-headers))

(defun mu4e-conversation--erase-handler (&optional _msg)
  "Don't clear the header buffer when viewing.")

(defun mu4e-conversation--found-handler (_count)
  (advice-remove mu4e-header-func 'mu4e-conversation--header-handler)
  (advice-remove mu4e-erase-func 'mu4e-conversation--erase-handler)
  (advice-remove mu4e-found-func 'mu4e-conversation--found-handler)
  (setq mu4e-conversation--thread nil)
  (advice-add mu4e-view-func :override 'mu4e-conversation--view-handler)
  (dolist (msg mu4e-conversation--thread-headers)
    (let ((docid (mu4e-message-field msg :docid))
          ;; decrypt (or not), based on `mu4e-decryption-policy'.
          (decrypt
           (and (member 'encrypted (mu4e-message-field msg :flags))
                (if (eq mu4e-decryption-policy 'ask)
                    (yes-or-no-p (mu4e-format "Decrypt message?")) ; TODO: Never ask?
                  mu4e-decryption-policy))))
      (mu4e~proc-view docid mu4e-view-show-images decrypt))))

(define-minor-mode mu4e-conversation-mode
  "Replace `mu4e-view' with `mu4e-conversation'."
  :init-value nil
  (if mu4e-conversation-mode
      (progn
        (advice-add 'mu4e-get-view-buffer :override 'mu4e-conversation--get-buffer)
        (advice-add 'mu4e~headers-redraw-get-view-window :override 'mu4e-conversation--headers-redraw-get-view-window)
        (advice-add 'mu4e-view-save-attachment-multi :before 'mu4e-conversation-set-attachment)
        (advice-add 'mu4e-view-open-attachment :before 'mu4e-conversation-set-attachment)
        ;; We must set the variable and not override its function because we
        ;; will need the override later.
        (setq mu4e-view-func 'mu4e-conversation))
    (advice-remove 'mu4e-get-view-buffer 'mu4e-conversation--get-buffer)
    (advice-remove 'mu4e-view-save-attachment-multi 'mu4e-conversation-set-attachment)
    (advice-remove 'mu4e-view-open-attachment 'mu4e-conversation-set-attachment)
    (advice-remove 'mu4e~headers-redraw-get-view-window 'mu4e-conversation--headers-redraw-get-view-window)
    (setq mu4e-view-func 'mu4e~headers-view-handler)))

(defun mu4e-conversation--turn-on ()
  "Turn on `mu4e-conversation-mode'."
  (mu4e-conversation-mode))

(define-globalized-minor-mode global-mu4e-conversation-mode mu4e-conversation-mode mu4e-conversation--turn-on
  :require 'mu4e-conversation)

;;;###autoload
(defun mu4e-conversation (&optional msg)
  (interactive)
  (setq mu4e-conversation--current-message (or msg (mu4e-message-at-point)))
  (unless mu4e-conversation--current-message
    (mu4e-warn "No message at point"))
  (setq mu4e-conversation--thread-headers nil)
  (advice-add mu4e-header-func :override 'mu4e-conversation--header-handler)
  (advice-add mu4e-erase-func :override 'mu4e-conversation--erase-handler)
  (advice-add mu4e-found-func :override 'mu4e-conversation--found-handler)
  (mu4e~proc-find
   ;; `mu4e-query-rewrite-function' seems to be missing from mu<1.0.
   (funcall (or mu4e-query-rewrite-function 'identity)
            (format "msgid:%s" (mu4e-message-field
                                mu4e-conversation--current-message
                                :message-id)))
   'show-threads
   :date
   'ascending
   (not 'limited)
   'skip-duplicates
   'include-related))

(provide 'mu4e-conversation)
;;; mu4e-conversation.el ends here
