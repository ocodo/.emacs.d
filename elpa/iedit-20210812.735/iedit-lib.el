;;; iedit-lib.el --- APIs for editing multiple regions in the same way
;;; simultaneously.

;; Copyright (C) 2010 - 2019, 2020, 2021 Victor Ren

;; Time-stamp: <2021-08-12 15:17:29 Victor Ren>
;; Author: Victor Ren <victorhge@gmail.com>
;; Keywords: occurrence region simultaneous rectangle refactoring
;; Version: 0.9.9.9
;; X-URL: https://github.com/victorhge/iedit
;;        https://www.emacswiki.org/emacs/Iedit
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x

;; This file is not part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is iedit APIs library that allow you to write your own minor mode.
;; The functionalities of the APIs:
;; - Create occurrence overlays
;; - Navigate in the occurrence overlays
;; - Modify the occurrences
;; - Hide/show
;; - Other basic support APIs
;;
;; A few concepts that help you understand what this is about:
;;
;; Occurrence - one of the regions that are selected, highlighted, usually the
;; same and about to be modified.
;;
;; Occurrence overlay - overlay used to provide a different face for occurrence
;;
;; Occurrence line - the line that has at least one occurrence
;;
;; Context line - the line that doesn't have occurrences

;;; todo:
;; - Update comments for APIs
;; - Add more easy access keys for whole occurrence

;;; Code:

;; (eval-when-compile (require 'cl-lib))

(declare-function c-before-change "cc-mode.el")

(defgroup iedit nil
  "Edit multiple regions in the same way simultaneously.
The regions are usually the same, called 'occurrence' in the mode."
  :prefix "iedit-"
  :group 'replace
  :group 'convenience)

(defface iedit-occurrence
  '((t :inherit highlight))
  "*Face used for the occurrences' default values."
  :group 'iedit)

(defface iedit-read-only-occurrence
  '((t :inherit region))
  "*Face used for the read-only occurrences' default values."
  :group 'iedit)

(defcustom iedit-case-sensitive-default t
  "If no-nil, matching is case sensitive."
  :type 'boolean
  :group 'iedit)

(defcustom iedit-transient-mark-sensitive t
  "If no-nil, Iedit mode is sensitive to the Transient Mark mode.
It means Iedit works as expected only when regions are
highlighted.  If you want to use iedit without Transient Mark
mode, set it as nil."
  :type 'boolean
  :group 'iedit)

(defcustom iedit-auto-buffering nil
  "If no-nil, iedit-mode automatically starts buffering the changes.
 This could be a workaround for lag problem under certain modes."
  :type 'boolean
  :group 'iedit)

(defcustom iedit-overlay-priority 200
  "The priority of the overlay used to indicate matches."
  :type 'integer
  :group 'iedit)

(defcustom iedit-index-update-limit 200
  "If the number of occurrences is great than this, the
`iedit-occurrence-index' will not be updated.  This is to avoid
the traverse of the long `iedit-occurrences-overlays' list."
  :type 'integer
  :group 'iedit)

(defcustom iedit-increment-format-string "%03d"
  "Format string used to format incremented numbers.
This is used by `iedit-number-occurrences'."
  :type 'string
  :group 'iedit)

(defvar iedit-occurrences-overlays nil
  "The occurrences slot contains a list of overlays used to
indicate the position of each editable occurrence.  In addition, the
occurrence overlay is used to provide a different face
configurable via `iedit-occurrence'.")

(defvar iedit-read-only-occurrences-overlays nil
  "The occurrences slot contains a list of overlays used to
indicate the position of each read-only occurrence.  In addition, the
occurrence overlay is used to provide a different face
configurable via `iedit-ready-only-occurrence'.")

(defvar iedit-case-sensitive iedit-case-sensitive-default
  "This is buffer local variable.
If no-nil, matching is case sensitive.  If nil and `case-replace'
is no-nil, iedit try to preserve the case pattern of each
occurrence.")

(defvar iedit-search-invisible search-invisible
  "search-invisible while matching.
Either nil, t, or 'open.  'open means the same as t except that
opens hidden overlays. ")

(defvar iedit-lib-skip-invisible-count 0
  "This is buffer local varible which is the number of skipped invisible occurrence. ")

(defvar iedit-lib-skip-filtered-count 0
  "This is buffer local varible which is the number of filtered occurrence. ")

(defvar iedit-hiding nil
  "This is buffer local variable which indicates whether buffer lines are hided. ")

(defvar iedit-forward-success t
  "This is buffer local variable which indicates the moving
forward or backward successful")

(defvar iedit-before-modification-string ""
  "This is buffer local variable which is the buffer substring
that is going to be changed.")

(defvar iedit-before-buffering-string ""
  "This is buffer local variable which is the buffer substring
that is going to be changed.")

(defvar iedit-before-buffering-undo-list nil
  "This is buffer local variable which is the buffer undo list before modification.")

(defvar iedit-before-buffering-point nil
  "This is buffer local variable which is the point before modification.")

;; `iedit-record-changes' gets called twice when change==0 and
;; occurrence is zero-width (beg==end) -- for front and back insertion.
(defvar iedit-skip-modification-once t
  "Variable used to skip first modification hook run when
insertion against a zero-width occurrence.")

(defvar iedit-aborting nil
  "This is buffer local variable which indicates Iedit mode is aborting.")

(defvar iedit-lib-quit-func nil
  "Function to call to exit mode using `iedit-lib'.
Should be set in `iedit-lib-start'.")

(defvar iedit-post-undo-hook-installed nil
  "This is buffer local variable which indicated if
`iedit-post-undo' is installed in `post-command-hook'.")

(defvar iedit-buffering nil
  "This is buffer local variable which indicates iedit-mode is
buffering, which means the modification to the current occurrence
is not applied to other occurrences when it is true.")

(defvar iedit-occurrence-context-lines 1
  "The number of lines before or after the occurrence.")

(defvar iedit-occurrence-index 0
  "The index of the current occurrence, counted from the beginning of the buffer.
Used in mode-line to indicate the position of the current
occurrence.")

(defvar iedit-after-change-list nil
  "Used to store the modifications in the command being run.")

(defvar iedit-updating nil
  "Used to prevent recursive calling change hooks.
It replaces `inhibit-modification-hooks' which prevents calling
`after-change-functions'.")

(defvar iedit-line-move-ignore-invisible-value nil
  "Used to save and restore the value of `line-move-ignore-invisible'.")

(make-variable-buffer-local 'iedit-updating)
(make-variable-buffer-local 'iedit-after-change-list)
(make-variable-buffer-local 'iedit-occurrences-overlays)
(make-variable-buffer-local 'iedit-read-only-occurrences-overlays)
(make-variable-buffer-local 'iedit-hiding)
(make-variable-buffer-local 'iedit-case-sensitive)
(make-variable-buffer-local 'iedit-forward-success)
(make-variable-buffer-local 'iedit-before-modification-string)
(make-variable-buffer-local 'iedit-before-buffering-string)
(make-variable-buffer-local 'iedit-before-buffering-undo-list)
(make-variable-buffer-local 'iedit-before-buffering-point)
(make-variable-buffer-local 'iedit-skip-modification-once)
(make-variable-buffer-local 'iedit-aborting)
(make-variable-buffer-local 'iedit-buffering)
(make-variable-buffer-local 'iedit-auto-buffering)
(make-variable-buffer-local 'iedit-post-undo-hook-installed)
(make-variable-buffer-local 'iedit-occurrence-context-lines)
(make-variable-buffer-local 'iedit-occurrence-index)
(make-variable-buffer-local 'iedit-lib-quit-func)
(make-variable-buffer-local 'iedit-lib-skip-invisible-count)
(make-variable-buffer-local 'iedit-lib-skip-filtered-count)
(make-variable-buffer-local 'iedit-line-move-ignore-invisible-value)

(defconst iedit-occurrence-overlay-name 'iedit-occurrence-overlay-name)
(defconst iedit-invisible-overlay-name 'iedit-invisible-overlay-name)

;;; Define Iedit mode map
(defvar iedit-lib-keymap
  (let ((map (make-sparse-keymap)))
    ;; Default key bindings
    (define-key map (kbd "TAB") 'iedit-next-occurrence)
    (define-key map (kbd "<tab>") 'iedit-next-occurrence)
    (define-key map (kbd "<S-tab>") 'iedit-prev-occurrence)
    (define-key map (kbd "<S-iso-lefttab>") 'iedit-prev-occurrence)
    (define-key map (kbd "<backtab>") 'iedit-prev-occurrence)
    (define-key map (kbd "C-'") 'iedit-show/hide-context-lines)
	(define-key map (kbd "C-\"") 'iedit-show/hide-occurrence-lines)
    map)
  "Keymap used while Iedit mode is enabled.")

(defvar iedit-occurrence-keymap-default
  (let ((map (make-sparse-keymap)))
    ;; `yas-minor-mode' uses tab by default and installs its keymap in
    ;; `emulation-mode-map-alists', which is used before before
    ;; ‘minor-mode-map-alist’.  So TAB is bond to get used even before
    ;; `yas-minor-mode', to prevent overriding.
    (define-key map (kbd "TAB") 'iedit-next-occurrence)
    (define-key map (kbd "<tab>") 'iedit-next-occurrence)
    (define-key map (kbd "M-U") 'iedit-upcase-occurrences)
    (define-key map (kbd "M-L") 'iedit-downcase-occurrences)
    (define-key map (kbd "M-R") 'iedit-replace-occurrences)
    (define-key map (kbd "M-SPC") 'iedit-blank-occurrences)
    (define-key map (kbd "M-D") 'iedit-delete-occurrences)
    (define-key map (kbd "M-N") 'iedit-number-occurrences)
    (define-key map (kbd "M-B") 'iedit-toggle-buffering)
    (define-key map (kbd "M-<") 'iedit-goto-first-occurrence)
    (define-key map (kbd "M->") 'iedit-goto-last-occurrence)
    (define-key map (kbd "C-?") 'iedit-help-for-occurrences)
    (define-key map [remap keyboard-escape-quit] 'iedit--quit)
    (define-key map [remap keyboard-quit] 'iedit--quit)
    map)
  "Default keymap used within occurrence overlays.")

;; The declarations are to avoid compile errors if mc is unknown by Emacs.
(declare-function mc/create-fake-cursor-at-point "ext:mutiple-cursors-core.el" nil)
(declare-function multiple-cursors-mode "ext:mutiple-cursors-core.el")
(defvar mc/cmds-to-run-once)

(when (require 'multiple-cursors-core nil t)
  (defun iedit-switch-to-mc-mode ()
    "Switch to `multiple-cursors-mode'.  So that you can navigate
out of the occurrence and edit simultaneously with multiple
cursors."
    (interactive "*")
    (iedit-barf-if-buffering)
    (let* ((ov (iedit-find-current-occurrence-overlay))
	   (offset (- (point) (overlay-start ov)))
	   (master (point)))
      (save-excursion
        (dolist (occurrence iedit-occurrences-overlays)
		  (goto-char (+ (overlay-start occurrence) offset))
		  (unless (= master (point))
			(mc/create-fake-cursor-at-point))))
      (iedit--quit)
      (multiple-cursors-mode 1)))
  ;; `multiple-cursors-mode' runs `post-command-hook' function on all the
  ;; cursors for updating them .  `iedit-switch-to-mc-mode' is not supposed to
  ;; be run in mc mode.  `mc/cmds-to-run-once' is for skipping this.
  (add-to-list 'mc/cmds-to-run-once 'iedit-switch-to-mc-mode)
  (define-key iedit-occurrence-keymap-default (kbd "M-M") 'iedit-switch-to-mc-mode))

(defvar iedit-occurrence-keymap 'iedit-occurrence-keymap-default
  "Keymap used within occurrence overlays.
It should be set before occurrence overlay is created.")
(make-local-variable 'iedit-occurrence-keymap)

(defun iedit-help-for-occurrences ()
  "Display `iedit-occurrence-keymap-default'"
  (interactive)
  (message (concat (substitute-command-keys "\\[iedit-upcase-occurrences]") "/"
                   (substitute-command-keys "\\[iedit-downcase-occurrences]") ":up/downcase "
                   (substitute-command-keys "\\[iedit-replace-occurrences]") ":replace "
                   (substitute-command-keys "\\[iedit-blank-occurrences]") ":blank "
                   (substitute-command-keys "\\[iedit-delete-occurrences]") ":delete "
                   (substitute-command-keys "\\[iedit-number-occurrences]") ":number "
                   (substitute-command-keys "\\[iedit-toggle-buffering]") ":buffering "
                   (substitute-command-keys "\\[iedit-goto-first-occurrence]") "/"
                   (substitute-command-keys "\\[iedit-goto-last-occurrence]") ":first/last "
                   )))

(defun iedit--quit ()
  "Quit the current mode by calling mode exit function."
  (interactive)
  (funcall iedit-lib-quit-func))

(defun iedit-make-markers-overlays (markers)
  "Create occurrence overlays on a list of markers."
  (setq iedit-occurrences-overlays
       (mapcar #'(lambda (marker)
                   (iedit-make-occurrence-overlay (car marker) (cdr marker)))
               markers)))

(defun iedit-make-occurrences-overlays (occurrence-regexp beg end)
  "Create occurrence overlays for `occurrence-regexp' in a region.
Return the number of occurrences."
  (setq iedit-occurrences-overlays nil)
  (setq iedit-read-only-occurrences-overlays nil)
  (setq iedit-lib-skip-invisible-count 0)
  (setq iedit-lib-skip-filtered-count 0)
  ;; Find and record each occurrence's markers and add the overlay to the occurrences
  (let ((counter 0)
        (case-fold-search (not iedit-case-sensitive))
		(search-invisible iedit-search-invisible)
		(length 0))
    (save-excursion
      (save-window-excursion
        (goto-char end)
        ;; todo: figure out why re-search-forward is slow without "recenter"
        (recenter)
        (goto-char beg)
        (while (re-search-forward occurrence-regexp end t)
          (let ((beginning (match-beginning 0))
                (ending (match-end 0)))
			(if (and (> length 0) (/= (- ending beginning) length))
				(throw 'not-same-length 'not-same-length)
			  (setq length (- ending beginning)))
			(cond
             ((text-property-not-all beginning ending 'read-only nil)
              (push (iedit-make-read-only-occurrence-overlay beginning ending)
                    iedit-read-only-occurrences-overlays))
			 ((and (not (eq search-invisible t))
				   (isearch-range-invisible beginning ending))
			  (setq iedit-lib-skip-invisible-count (1+ iedit-lib-skip-invisible-count)))
			 ((not (funcall isearch-filter-predicate beginning ending))
			  (setq iedit-lib-skip-filtered-count (1+ iedit-lib-skip-filtered-count)))
			 (t
              (push (iedit-make-occurrence-overlay beginning ending)
                    iedit-occurrences-overlays)
			  (setq counter (1+ counter))))))))
    (iedit-update-index)
    counter))

(defun iedit-update-index (&optional point)
  "Update `iedit-occurrence-index' with the current occurrence,
if the total number of occurrences is less than
`iedit-index-update-limit'."
  (if (< (length iedit-occurrences-overlays) iedit-index-update-limit)
    (let ((pos (or point (point)))
	  (index 0))
      (dolist (occurrence iedit-occurrences-overlays)
	(if (>= pos (overlay-start occurrence))
	    (setq index (1+ index))))
      (setq iedit-occurrence-index index))))

(defun iedit-add-next-occurrence-overlay (occurrence-exp &optional point)
  "Create next occurrence overlay for `occurrence-exp'."
  (iedit-add-occurrence-overlay occurrence-exp point t))

(defun iedit-add-previous-occurrence-overlay (occurrence-exp &optional point)
  "Create previous occurrence overlay for `occurrence-exp'."
  (iedit-add-occurrence-overlay occurrence-exp point nil))

(defun iedit-add-occurrence-overlay (occurrence-exp point forward &optional bound)
  "Create next or previous occurrence overlay for `occurrence-exp'.
Return the start position of the new occurrence if successful."
  (or point
      (setq point (point)))
  (let ((case-fold-search (not iedit-case-sensitive))
        (pos nil))
    (save-excursion
      (goto-char point)
      (if (not (if forward
                   (re-search-forward occurrence-exp bound t)
                 (re-search-backward occurrence-exp bound t)))
          (message "No more matches.")
        (setq pos (match-beginning 0))
        (if (or (iedit-find-overlay-at-point (match-beginning 0) 'iedit-occurrence-overlay-name)
                (iedit-find-overlay-at-point (match-end 0) 'iedit-occurrence-overlay-name))
            (error "Conflict region"))
        (push (iedit-make-occurrence-overlay (match-beginning 0)
                                             (match-end 0))
              iedit-occurrences-overlays)
	(iedit-update-index point)
        (message "Add one match for \"%s\"." (iedit-printable occurrence-exp))
        (when iedit-hiding
          (iedit-show-all)
          (iedit-hide-context-lines iedit-occurrence-context-lines))
        ))
    pos))

(defun iedit-add-region-as-occurrence (beg end)
  "Add region as an occurrence.
The length of the region must the same as other occurrences if
there are."
  (or (= beg end)
      (error "No region"))
  (if (null iedit-occurrences-overlays)
      (push
       (iedit-make-occurrence-overlay beg end)
       iedit-occurrences-overlays)
    (or (= (- end beg) (iedit-occurrence-string-length))
        (error "Wrong region"))
    (if (or (iedit-find-overlay-at-point beg 'iedit-occurrence-overlay-name)
            (iedit-find-overlay-at-point end 'iedit-occurrence-overlay-name))
        (error "Conflict region"))
    (push (iedit-make-occurrence-overlay beg end)
          iedit-occurrences-overlays)
    (iedit-update-index)
    )) ;; todo test this function

(defun iedit-lib-start (mode-exit-func)
  "Initialize the hooks."
  (when iedit-auto-buffering
	(iedit-start-buffering))
  (setq iedit-aborting nil)
  ;; Enforce skip modification once, errors may happen to cause this to be
  ;; unset.
  (setq iedit-skip-modification-once t)
  (setq iedit-lib-quit-func mode-exit-func)
  (add-hook 'post-command-hook 'iedit-update-occurrences nil t)
  (add-hook 'before-revert-hook iedit-lib-quit-func nil t)
  (add-hook 'kbd-macro-termination-hook iedit-lib-quit-func nil t)
  (add-hook 'change-major-mode-hook iedit-lib-quit-func nil t)
  (setq iedit-after-change-list nil))

(defun iedit-lib-cleanup ()
  "Clean up occurrence overlay, invisible overlay and local variables."
  (iedit-cleanup-occurrences-overlays)
  (remove-hook 'post-command-hook 'iedit-update-occurrences t)
  (remove-hook 'before-revert-hook iedit-lib-quit-func t)
  (remove-hook 'kbd-macro-termination-hook iedit-lib-quit-func t)
  (remove-hook 'change-major-mode-hook iedit-lib-quit-func t)
  (setq iedit-lib-quit-func nil)
  (setq iedit-occurrences-overlays nil)
  (setq iedit-read-only-occurrences-overlays nil)
  (setq iedit-aborting nil)
  (setq iedit-before-modification-string "")
  (setq iedit-before-buffering-undo-list nil)
  (setq iedit-hiding nil))

(defun iedit-make-occurrence-overlay (begin end)
  "Create an overlay for an occurrence in Iedit mode.
Add the properties for the overlay: a face used to display a
occurrence's default value, and modification hooks to update
occurrences if the user starts typing."
  (let ((occurrence (make-overlay begin end (current-buffer) nil t)))
    (overlay-put occurrence iedit-occurrence-overlay-name t)
    (overlay-put occurrence 'face 'iedit-occurrence)
    (overlay-put occurrence 'keymap iedit-occurrence-keymap)
    (overlay-put occurrence 'insert-in-front-hooks '(iedit-record-changes))
    (overlay-put occurrence 'insert-behind-hooks '(iedit-record-changes))
    (overlay-put occurrence 'modification-hooks '(iedit-record-changes))
    (overlay-put occurrence 'priority iedit-overlay-priority)
	;; Identify case pattern of the occurrence.
    (overlay-put occurrence 'category (if (and (not iedit-case-sensitive) case-replace)
										  (iedit-case-pattern begin end)
										'no-change))
    occurrence))

(defun iedit-make-read-only-occurrence-overlay (begin end)
  "Create an overlay for an read-only occurrence in Iedit mode."
  (let ((occurrence (make-overlay begin end (current-buffer) nil t)))
    (overlay-put occurrence iedit-occurrence-overlay-name t)
    (overlay-put occurrence 'face 'iedit-read-only-occurrence)
    occurrence))

(defun iedit-make-invisible-overlay (begin end)
  "Create an invisible overlay from `begin` to `end`."
  (let ((invisible-overlay (make-overlay begin end (current-buffer) nil t)))
    (overlay-put invisible-overlay iedit-invisible-overlay-name t)
    (overlay-put invisible-overlay 'invisible 'iedit-invisible-overlay-name)
    ;;    (overlay-put invisible-overlay 'intangible t)
    invisible-overlay))

(defun iedit-post-undo ()
  "Check if it is time to abort iedit after undo command is executed.

This is added to `post-command-hook' when undo command is executed
in occurrences."
  (if (iedit-same-length)
      nil
    (iedit--quit))
  (remove-hook 'post-command-hook 'iedit-post-undo t)
  (setq iedit-post-undo-hook-installed nil))

(defun iedit-reset-aborting ()
  "Turning Iedit mode off and reset `iedit-aborting'.

This is added to `post-command-hook' when aborting Iedit mode is
decided.  `iedit-lib-quit-func' is postponed after the current
command is executed for avoiding `iedit-record-changes'
is called for a removed overlay."
  (iedit--quit)
  (remove-hook 'post-command-hook 'iedit-reset-aborting t)
  (setq iedit-aborting nil))

;; There are two ways to update all occurrences.  One is to redefine all key
;; stroke map for overlay, the other is to figure out three basic modifications
;; in the modification hook.  This function chooses the latter.
(defun iedit-record-changes (occurrence after beg end &optional change)
  "Record the changes to the current occurrence.
This modification hook is triggered when a user edits any
occurrence and is responsible for recording all the changes to
the current occurrence.  Refer to `modification-hooks' for more
details.  Current supported edits are insertion, yank, deletion
and replacement.  If this modification is going out of the
occurrence, it will abort Iedit mode."
  (if (and undo-in-progress (null iedit-after-change-list))
      ;; If the "undo" change (not part of another command) make occurrences
      ;; different, it is going to mess up occurrences.  So a length check will
      ;; be done after undo command is executed.
      (when (not iedit-post-undo-hook-installed)
        (add-hook 'post-command-hook 'iedit-post-undo nil t)
        (setq iedit-post-undo-hook-installed t))
    (when (and (not iedit-updating) (not iedit-aborting))
      ;; before modification
      (if (null after)
          (if (or (< beg (overlay-start occurrence))
                  (> end (overlay-end occurrence)))
              (progn (setq iedit-aborting t) ; abort iedit-mode
					 (add-hook 'post-command-hook 'iedit-reset-aborting nil t))
			(setq iedit-before-modification-string
                  (buffer-substring-no-properties beg end))
			;; Check if this is called twice before modification. When inserting
			;; into zero-width occurrence or between two conjoined occurrences,
			;; both insert-in-front-hooks and insert-behind-hooks will be
			;; called.  Two calls will make `iedit-skip-modification-once' true.
			(setq iedit-skip-modification-once (not iedit-skip-modification-once)))
		;; after modification
        (if iedit-skip-modification-once
            ;; Skip the first hook
            (setq iedit-skip-modification-once nil)
		  (setq iedit-skip-modification-once t)
		  (when (and (not iedit-buffering) (not iedit-updating))
			(when (or (eq 0 change) ;; insertion
                      (eq beg end)  ;; deletion
                      (not (string= iedit-before-modification-string ;; replacement
									(buffer-substring-no-properties beg end))))
			  (let* ((inslen (- end beg))
					 (dellen change))
				(push (list occurrence
							(- beg (overlay-start occurrence))			; From 1 to beg
							(- (overlay-end occurrence) end) ; From end to point-max
							(- inslen dellen))	; changed number
					  iedit-after-change-list)))))))))

(defun iedit-update-occurrences ()
  "Updating other occurrences.
This part is running in `post-command-hook'. It combines
`iedit-after-change-list' into one change and apply it to all the
other occurrences."
  (when (and (not iedit-updating) iedit-after-change-list)
	(let ((occurrence (caar iedit-after-change-list))
		  (beg (buffer-size))
		  (end (buffer-size))
		  (change 0))
	  (dolist (mod iedit-after-change-list)
		(setq beg (min beg (nth 1 mod)))
		(setq end (min end (nth 2 mod)))
		(setq change (+ change (nth 3 mod))))
	  (let* ((begpos (+ (overlay-start occurrence) beg))
			 (endpos (- (overlay-end occurrence) end))
			 (inslen (- endpos begpos))
			 (dellen (- inslen change))
			 (endpos (+ begpos inslen)))
		(iedit-apply-change
		 occurrence
		 begpos
		 endpos
		 dellen)
		(setq iedit-after-change-list nil)))))

(defun iedit-apply-change (occurrence beg end &optional change)
  "Apply the change to all the other occurrences. "
  (let ((iedit-updating t)
        (offset (- beg (overlay-start occurrence)))
        (value (buffer-substring-no-properties beg end)))
    (save-excursion
	  (iedit-move-conjoined-overlays occurrence)
	  (when (/= beg end)
		;; apply the case pattern on the current occurrence
		(cl-case (overlay-get occurrence 'category)
		  (all-caps
		   (upcase-region beg end))
		  (cap-initial
		   (when (= 0 offset) (capitalize-region beg end )))))
      (dolist (another-occurrence iedit-occurrences-overlays)
        (when (not (eq another-occurrence occurrence))
          (let* ((beginning (+ (overlay-start another-occurrence) offset))
				 (ending (+ beginning (- end beg))))
			(when (/= 0 change) (delete-region beginning (+ beginning change))) ;; delete
			(when (/= beg end) ;; insert
			  (goto-char beginning)
			  (insert-and-inherit
			   ;; preserve the case pattern of each occurrence
			   (cl-case (overlay-get another-occurrence 'category)
			     (no-change value)
			     (all-caps
			      (upcase value))
			     (cap-initial
			      (if (= 0 offset)
                                  (capitalize value)
				value))))))
          (iedit-move-conjoined-overlays another-occurrence))))))

(defun iedit-next-occurrence (&optional arg)
  "Move forward to the next occurrence in the `iedit'.
Move ARG occurrences forward (backward if ARG is negative).
Interactively, ARG is the numeric prefix argument.
If ARG is omitted or nil, move 1 occurrences forward.
If point reaches the last occurrences, you are asked to type
another `iedit-next-occurrence', it starts again from the
beginning of the buffer. If used with prefix argument, wrapping
from last to first occurrence will cost one repetition."
  (interactive "p")
  (cond ((< arg 0) (iedit-prev-occurrence (abs arg)))
        ((> arg 0)
         (dotimes (i arg)
           (let* ((pos (point))
                  (ov (iedit-find-current-occurrence-overlay)))
             (if ov
                 (if (iedit-find-overlay-at-point (overlay-end ov) 'iedit-occurrence-overlay-name)
                     (setq pos (overlay-end ov)) ; conjoined overlay
                   ;; from inside
                   (setq pos (next-single-char-property-change pos 'iedit-occurrence-overlay-name))
                   (setq pos (next-single-char-property-change pos 'iedit-occurrence-overlay-name)))
               ;; from outside
               (setq pos (next-single-char-property-change pos 'iedit-occurrence-overlay-name)))

             (if (/= pos (point-max))
                 (setq iedit-forward-success t)
               (if (and iedit-forward-success ov)
                   (progn (message "This is the last occurrence.")
                          (setq iedit-forward-success nil))
                 (progn
                   (if (get-char-property (point-min) 'iedit-occurrence-overlay-name)
                       (setq pos (point-min))
                     (setq pos (next-single-char-property-change
                                (point-min)
                                'iedit-occurrence-overlay-name)))
                   (setq iedit-forward-success t)
                   (message "Located the first occurrence."))))
             (when iedit-forward-success
               (iedit-update-index pos)
               (goto-char pos)))))))

(defun iedit-prev-occurrence (&optional arg)
  "Move backward to the previous occurrence in the `iedit'.
Move ARG occurrences backward (forward if ARG is negative).
Interactively, ARG is the numeric prefix argument.
If ARG is omitted or nil, move 1 occurrences back.
If point reaches the first occurrences, you are asked to type
another `iedit-prev-occurrence', it starts again from the
end of the buffer. If used with prefix argument, wrapping
from first to last occurrence will cost one repetition."
  (interactive "p")
  (cond ((< arg 0) (iedit-next-occurrence (abs arg)))
        ((> arg 0)
         (dotimes (i arg)
           (let ((pos (point))
                 (ov (iedit-find-current-occurrence-overlay))
                 (previous-overlay))
             (when (/= pos (point-min))
               (when ov (setq pos (overlay-start ov)))
               (if (and ov
                        (setq previous-overlay (iedit-find-overlay-at-point (1- pos) 'iedit-occurrence-overlay-name)))
                   (setq pos (overlay-start previous-overlay)) ;conjoined
                 (setq pos (previous-single-char-property-change pos 'iedit-occurrence-overlay-name))
                 (setq pos (previous-single-char-property-change pos 'iedit-occurrence-overlay-name))))
             ;; At the start of the first occurrence
             (if (or (and (eq pos (point-min))
                          (not (get-char-property (point-min) 'iedit-occurrence-overlay-name)))
                     (and (eq (point) (point-min))
                          ov))
                 (if (and iedit-forward-success ov)
                     (progn (message "This is the first occurrence.")
                            (setq iedit-forward-success nil))
                   (progn
                     (setq pos (iedit-last-occurrence))
                     (setq iedit-forward-success t)
                     (message "Located the last occurrence.")))
               (setq iedit-forward-success t))
             (when iedit-forward-success
               (iedit-update-index pos)
               (goto-char pos)))))))

(defun iedit-first-occurrence ()
  "return the position of the first occurrence."
  (if (get-char-property (point-min) 'iedit-occurrence-overlay-name)
      (point-min)
    (next-single-char-property-change
     (point-min) 'iedit-occurrence-overlay-name)))

(defun iedit-goto-first-occurrence ()
  "Move to the first occurrence."
  (interactive)
  (goto-char (iedit-first-occurrence))
  (setq iedit-forward-success t)
  (setq iedit-occurrence-index 1)
  (message "Located the first occurrence."))

(defun iedit-goto-last-occurrence ()
  "Move to the last occurrence."
  (interactive)
  (goto-char (iedit-last-occurrence))
  (setq iedit-forward-success t)
  (setq iedit-occurrence-index (length iedit-occurrences-overlays))
  (message "Located the last occurrence."))

(defun iedit-last-occurrence ()
  "return the position of the last occurrence."
  (let ((ov (iedit-find-overlay-at-point (1- (point-max)) 'iedit-occurrence-overlay-name))
		(pos (point-max)))
	(if ov
		(overlay-start ov)
	  (setq pos (previous-single-char-property-change pos 'iedit-occurrence-overlay-name))
	  (overlay-start (iedit-find-overlay-at-point (1- pos) 'iedit-occurrence-overlay-name)))))

(defun iedit-show/hide-context-lines (&optional arg)
  "Show or hide context lines.
A prefix ARG specifies how many lines before and after the
occurrences are not hidden;  negative is treated the same as zero.

If no prefix argument, the prefix argument last time or default
value of `iedit-occurrence-context-lines' is used for this time."
  (interactive "P")
  (if (null arg)
      ;; toggle visible
      (progn (setq iedit-hiding (not iedit-hiding))
             (if iedit-hiding
                 (iedit-hide-context-lines iedit-occurrence-context-lines)
               (iedit-show-all)))
    ;; reset invisible lines
    (setq arg (prefix-numeric-value arg))
    (if (< arg 0)
        (setq arg 0))
    (unless (and iedit-hiding
                 (= arg iedit-occurrence-context-lines))
	  (when iedit-hiding
        (remove-overlays nil nil iedit-invisible-overlay-name t))
	  (setq iedit-occurrence-context-lines arg)
	  (setq iedit-hiding t)
	  (iedit-hide-context-lines iedit-occurrence-context-lines))))

(defun iedit-show-all()
  "Show hidden lines."
  (setq-local line-move-ignore-invisible iedit-line-move-ignore-invisible-value)
  (remove-from-invisibility-spec '(iedit-invisible-overlay-name . t))
  (remove-overlays nil nil iedit-invisible-overlay-name t))

(defun iedit-hide-context-lines (visible-context-lines)
  "Hide context lines using invisible overlay."
  (let ((prev-occurrence-end 1)
        (hidden-regions nil))
    (save-excursion
      (goto-char (iedit-first-occurrence))
      (while (/= (point) (point-max))
        ;; Now at the beginning of an occurrence
        (let ((current-start (point)))
          (forward-line (- visible-context-lines))
          (let ((line-beginning (line-beginning-position)))
            (if (> line-beginning prev-occurrence-end)
                (push  (list prev-occurrence-end (1- line-beginning)) hidden-regions)))
          ;; goto the end of the occurrence
          (goto-char (next-single-char-property-change current-start 'iedit-occurrence-overlay-name)))
        (let ((current-end (point)))
          (forward-line visible-context-lines)
          (setq prev-occurrence-end (1+ (line-end-position)))
          ;; goto the beginning of next occurrence
          (goto-char (next-single-char-property-change current-end 'iedit-occurrence-overlay-name))))
      (if (< prev-occurrence-end (point-max))
          (push (list prev-occurrence-end (point-max)) hidden-regions))
      (when hidden-regions
		(setq iedit-line-move-ignore-invisible-value
			  (buffer-local-value 'line-move-ignore-invisible (current-buffer)))
        (setq-local line-move-ignore-invisible t)
        (add-to-invisibility-spec '(iedit-invisible-overlay-name . t))
        (dolist (region hidden-regions)
          (iedit-make-invisible-overlay (car region) (cadr region)))))
    hidden-regions))

;;;; functions for overlay keymap
(defun iedit-hide-occurrence-lines ()
  "Hide occurrence lines using invisible overlay."
  (let ((hidden-regions nil)
		  (beginning  nil)
		  (end nil))
      (save-excursion
		(goto-char (iedit-first-occurrence))
		;; Now at the beginning of an occurrence
		(setq beginning (line-beginning-position))
		(while (/= (point) (point-max))
          ;; goto the end of the occurrence
          (goto-char (next-single-char-property-change (point) 'iedit-occurrence-overlay-name))
		  (setq end (line-end-position))
		  ;; goto the next beginning of the occurrence
		  (goto-char (next-single-char-property-change (point) 'iedit-occurrence-overlay-name))
		  (when (or (> (line-beginning-position) (1+ end))
					(= (line-end-position) (point-max)))
			(push (list beginning end) hidden-regions)
			(setq beginning (line-beginning-position)))))
	  (when hidden-regions
		(setq iedit-line-move-ignore-invisible-value
			  (buffer-local-value 'line-move-ignore-invisible (current-buffer)))
        (setq-local line-move-ignore-invisible t)
		(dolist (region hidden-regions)
          (iedit-make-invisible-overlay (car region) (cadr region))))
	    ;; Value returned is for ert
	  hidden-regions))

(defun iedit-show/hide-occurrence-lines ()
  "Show or hide occurrence lines using invisible overlay."
  (interactive "*")
  (setq iedit-hiding (not iedit-hiding))
  (if (not iedit-hiding)
	  (iedit-show-all)
	(iedit-hide-occurrence-lines)))

(defun iedit-apply-on-occurrences (function &rest args)
  "Call function for each occurrence."
  (let ((iedit-updating t))
      (save-excursion
        (dolist (occurrence iedit-occurrences-overlays)
          (apply function (overlay-start occurrence) (overlay-end occurrence) args)))))

(defun iedit-upcase-occurrences ()
  "Convert occurrences to upper case."
  (interactive "*")
  (iedit-barf-if-buffering)
  (iedit-apply-on-occurrences 'upcase-region))

(defun iedit-downcase-occurrences()
  "Convert occurrences to lower case."
  (interactive "*")
  (iedit-barf-if-buffering)
  (iedit-apply-on-occurrences 'downcase-region))

(defun iedit-number-occurrences (start-at &optional format-string)
  "Insert numbers in front of the occurrences.
START-AT, if non-nil, should be a number from which to begin
counting.  FORMAT, if non-nil, should be a format string to pass
to `format-string' along with the line count.  When called
interactively with a prefix argument, prompt for START-AT and
FORMAT."
  (interactive
   (if current-prefix-arg
       (let* ((start-at (read-number "Number to count from: " 1)))
		 (list start-at
			   (read-string
				(format "Format incremented numbers (default '%s'): "
						iedit-increment-format-string)
				nil nil iedit-increment-format-string)))
	 (list 1 iedit-increment-format-string)))
  (iedit-barf-if-buffering)
  (let ((number start-at)
        (iedit-updating t))
    (save-excursion
	  (goto-char (iedit-first-occurrence))
	  (cl-loop for counter from number
			   for ov = (iedit-find-current-occurrence-overlay)
			   while (/= (point) (point-max))
			   do (progn
		  (if (re-search-forward "\\\\#" (overlay-end ov) t)
			  (replace-match (format format-string counter) t)
			(insert (format format-string counter)))
		  (iedit-move-conjoined-overlays ov)
		  ;; goto the beginning of the next occourrence overlay
		  (if (iedit-find-overlay-at-point (overlay-end ov) 'iedit-occurrence-overlay-name)
			  (goto-char (overlay-end ov)) ; conjoined overlay
			(when (< (point) (overlay-end ov))
			  (goto-char (next-single-char-property-change (point) 'iedit-occurrence-overlay-name)))
			(goto-char (next-single-char-property-change (point) 'iedit-occurrence-overlay-name))))))))

;;; Don't downcase from-string to allow case freedom!
(defun iedit-replace-occurrences(&optional to-string)
  "Replace occurrences with STRING."
  (interactive "*")
  (iedit-barf-if-buffering)
  (let* ((ov (iedit-find-current-occurrence-overlay))
         (offset (- (point) (overlay-start ov)))
         (from-string (buffer-substring-no-properties
                       (overlay-start ov)
                       (overlay-end ov)))
         (to-string (if (not to-string)
			(read-string "Replace with: "
				     nil nil
				     from-string
				     nil)
                      to-string)))
    (iedit-apply-on-occurrences
     (lambda (beg end from-string to-string)
       (goto-char beg)
       (search-forward from-string end)
       (replace-match to-string (not (and (not iedit-case-sensitive) case-replace))))
     from-string to-string)
    (goto-char (+ (overlay-start ov) offset))))

(defun iedit-blank-occurrences()
  "Replace occurrences with blank spaces."
  (interactive "*")
  (iedit-barf-if-buffering)
  (let* ((ov (car iedit-occurrences-overlays))
         (offset (- (point) (overlay-start ov)))
         (count (- (overlay-end ov) (overlay-start ov))))
    (iedit-apply-on-occurrences
     (lambda (beg end )
       (delete-region beg end)
       (goto-char beg)
       (insert-and-inherit (make-string count 32))))
    (goto-char (+ (overlay-start ov) offset))))

(defun iedit-delete-occurrences()
  "Delete occurrences."
  (interactive "*")
  (iedit-barf-if-buffering)
  (iedit-apply-on-occurrences 'delete-region))

;; todo: add cancel buffering function
(defun iedit-toggle-buffering ()
  "Toggle buffering.
This is intended to improve iedit's response time.  If the number
of occurrences are huge, it might be slow to update all the
occurrences for each key stoke.  When buffering is on,
modification is only applied to the current occurrence and will
be applied to other occurrences when buffering is off."
  (interactive "*")
  (if iedit-buffering
      (iedit-stop-buffering)
    (iedit-start-buffering))
  (message (concat "Modification Buffering "
                   (if iedit-buffering
                       "started."
                     "stopped."))))

(defun iedit-start-buffering ()
  "Start buffering."
  (setq iedit-buffering t)
  (setq iedit-before-buffering-string (iedit-current-occurrence-string))
  (setq iedit-before-buffering-undo-list buffer-undo-list)
  (setq iedit-before-buffering-point (point))
  (buffer-disable-undo)
  (message "Start buffering editing..."))

(defun iedit-case-pattern (beg end)
  "Distinguish the case pattern of the text between `beg' and `end'.

These case ptterns are the same as the ones Emacs replace
commands can recognized - three alternatives - all caps,
captilized, the others.

If the text has only capital letters and has at least one
multiletter word, it is 'all caps'. If all words are capitalized,
it is captilized.'"
  (let ((some-word nil)
		(some-lowercase nil)
		(some-uppercase nil)
		(some-non-uppercase-init nil)
		(previous-char ?\n)
		(char nil)
		(index 0)
		(text (buffer-substring beg end)))
	(while (< index (- end beg))
	  (setq char (elt text index))
	  (if (and (= char (downcase char))
			   (/= char (upcase char)))
		  (progn
			(setq some-lowercase t)
			(if (/= ?w (char-syntax previous-char))
				(setq some-non-uppercase-init t)
			  (setq some-word t)))
		(if (/= char (downcase char))
			(progn
			  (setq some-uppercase t)
			  (when (= ?w (char-syntax previous-char))
				(setq some-word t)))
		  (when (/= ?w (char-syntax previous-char))
			(setq some-non-uppercase-init t))))
	  (setq previous-char char)
	  (setq index (1+ index)))
	(if (and (null some-lowercase) some-word)
		'all-caps
	  (if (and (null some-non-uppercase-init) some-word)
		  'cap-initial
		(if (and (null some-non-uppercase-init) some-uppercase)
			'all-caps
		  'no-change)))))

(defun iedit-stop-buffering ()
  "Stop buffering and apply the modification to other occurrences.
If current point is not at any occurrence, the buffered
modification is not going to be applied to other occurrences."
  (let ((ov (iedit-find-current-occurrence-overlay)))
    (when ov
      (let* ((beg (overlay-start ov))
             (end (overlay-end ov))
             (modified-string (buffer-substring-no-properties beg end))
             (offset (- (point) beg)) ;; delete-region moves cursor
             (iedit-updating t))
        (when (not (string= iedit-before-buffering-string modified-string))
          (save-excursion
            ;; Rollback the current modification and buffer-undo-list. This is
            ;; to avoid the inconsistency if user undoes modifications
            (delete-region beg end)
            (goto-char beg)
            (insert-and-inherit iedit-before-buffering-string)
			(goto-char iedit-before-buffering-point)
			(buffer-enable-undo)
            (setq buffer-undo-list iedit-before-buffering-undo-list)
			;; go back here if undo
			(push (point) buffer-undo-list)
            (dolist (occurrence iedit-occurrences-overlays) ; todo:extract as a function
              (let ((beginning (overlay-start occurrence))
                    (ending (overlay-end occurrence)))
                (delete-region beginning ending)
                (unless (eq beg end) ;; replacement
                  (goto-char beginning)
                  (insert-and-inherit
				   (cl-case (overlay-get occurrence 'category)
				     (all-caps
				      (upcase modified-string))
				     (cap-initial
				      (if (= 0 offset)
                          (capitalize modified-string)
						modified-string))
				     (t modified-string))))
                (iedit-move-conjoined-overlays occurrence))))
          (goto-char (+ (overlay-start ov) offset))))))
  (setq iedit-buffering nil)
  (message "Buffered modification applied.")
  (setq iedit-before-buffering-undo-list nil))

(defun iedit-move-conjoined-overlays (occurrence)
  "This function keeps overlays conjoined after modification.
After modification, conjoined overlays may be overlapped."
  (let ((beginning (overlay-start occurrence))
        (ending (overlay-end occurrence)))
    (unless (= beginning (point-min))
      (let ((previous-overlay (iedit-find-overlay-at-point
                               (1- beginning)
                               'iedit-occurrence-overlay-name)))
        (if previous-overlay ; two conjoined occurrences
            (move-overlay previous-overlay
                          (overlay-start previous-overlay)
                          beginning))))
    (unless (= ending (point-max))
      (let ((next-overlay (iedit-find-overlay-at-point
                           ending
                           'iedit-occurrence-overlay-name)))
        (if next-overlay ; two conjoined occurrences
            (move-overlay next-overlay ending (overlay-end next-overlay)))))))

;;; help functions
(defun iedit-find-current-occurrence-overlay ()
  "Return the current occurrence overlay  at point or point - 1.
This function is supposed to be called in overlay keymap."
  (or (iedit-find-overlay-at-point (point) 'iedit-occurrence-overlay-name)
      (iedit-find-overlay-at-point (1- (point)) 'iedit-occurrence-overlay-name)))

(defun iedit-find-overlay-at-point (point property)
  "Return the overlay with PROPERTY at POINT."
  (let ((overlays (overlays-at point))
        found)
    (while (and overlays (not found))
      (let ((overlay (car overlays)))
        (if (overlay-get overlay property)
            (setq found overlay)
          (setq overlays (cdr overlays)))))
    found))

(defun iedit-same-column ()
  "Return t if all occurrences are at the same column."
  (save-excursion
    (let ((column (progn (goto-char (overlay-start (car iedit-occurrences-overlays)))
                         (current-column)))
          (overlays (cdr  iedit-occurrences-overlays))
          (same t))
      (while (and overlays same)
        (let ((overlay (car overlays)))
          (if (/= (progn (goto-char (overlay-start overlay))
                         (current-column))
                  column)
              (setq same nil)
            (setq overlays (cdr overlays)))))
      same)))

(defun iedit-same-length ()
  "Return t if all occurrences are the same length."
  (save-excursion
    (let ((length (iedit-occurrence-string-length))
          (overlays (cdr iedit-occurrences-overlays))
          (same t))
      (while (and overlays same)
        (let ((ov (car overlays)))
          (if (/= (- (overlay-end ov) (overlay-start ov))
                  length)
              (setq same nil)
            (setq overlays (cdr overlays)))))
      same)))

;; This function might be called out of any occurrence
(defun iedit-current-occurrence-string ()
  "Return current occurrence string.
Return nil if occurrence string is empty string."
  (let ((ov (or (iedit-find-current-occurrence-overlay)
                 (car iedit-occurrences-overlays))))
    (if ov
        (let ((beg (overlay-start ov))
              (end (overlay-end ov)))
          (if (/=  beg end)
              (buffer-substring-no-properties beg end)
            nil))
      nil)))

(defun iedit-occurrence-string-length ()
  "Return the length of current occurrence string."
  (let ((ov (car iedit-occurrences-overlays)))
    (- (overlay-end ov) (overlay-start ov))))

(defun iedit-find-overlay (beg end property &optional exclusive)
  "Return a overlay with property in region, or out of the region if EXCLUSIVE is not nil."
  (if exclusive
      (or (iedit-find-overlay-in-region (point-min) beg property)
          (iedit-find-overlay-in-region end (point-max) property))
    (iedit-find-overlay-in-region beg end property)))

(defun iedit-find-overlay-in-region (beg end property)
  "Return a overlay with property in region."
  (let ((overlays (overlays-in beg end))
        found)
    (while (and overlays (not found))
      (let ((overlay (car overlays)))
        (if (and (overlay-get overlay property)
                 (>= (overlay-start overlay) beg)
                 (<= (overlay-end overlay) end))
            (setq found overlay)
          (setq overlays (cdr overlays)))))
    found))

(defun iedit-cleanup-occurrences-overlays (&optional beg end inclusive)
  "Remove overlays from list `iedit-occurrences-overlays'."
  (when iedit-buffering
    (iedit-stop-buffering))
  ;; Close overlays opened by `isearch-range-invisible'
  (isearch-clean-overlays)
  (when iedit-hiding
	(iedit-show-all))
  (if (null beg)
	  (remove-overlays nil nil iedit-occurrence-overlay-name t)
	(if inclusive
		(remove-overlays beg end iedit-occurrence-overlay-name t)
      (remove-overlays (point-min) beg iedit-occurrence-overlay-name t)
      (remove-overlays end (point-max) iedit-occurrence-overlay-name t))
	(let (overlays)
      (dolist (overlay iedit-occurrences-overlays)
		(if (overlay-buffer overlay)
			(push overlay overlays)))
      (setq iedit-occurrences-overlays overlays)
      (iedit-update-index))))

(defun iedit-printable (string)
  "Return a omitted substring that is not longer than 50.
STRING is already `regexp-quote'ed"
  (let ((first-newline-index (string-match "$" string))
        (length (length string)))
    (if (and first-newline-index
             (/= first-newline-index length))
        (if (< first-newline-index 50)
            (concat (substring string 0 first-newline-index) "...")
          (concat (substring string 0 50) "..."))
      (if (> length 50)
          (concat (substring string 0 50) "...")
        string))))

(defun iedit-char-at-bol (&optional N)
  "Get char position of the beginning of the current line. If `N'
is given, move forward (or backward) that many lines (using
`forward-line') and get the char position at the beginning of
that line."
  (save-excursion
    (forward-line (if N N 0))
    (point)))

(defun iedit-char-at-eol (&optional N)
  "Get char position of the end of the current line. If `N' is
given, move forward (or backward) that many lines (using
`forward-line') and get the char position at the end of that
line."
  (save-excursion
    (forward-line (if N N 0))
    (end-of-line)
    (point)))

(defun iedit-region-active ()
  "Return t if region is active and not empty.
If variable `iedit-transient-mark-sensitive' is t, active region
means `transient-mark-mode' is on and mark is active. Otherwise,
it just means mark is active."
  (and (if iedit-transient-mark-sensitive
           transient-mark-mode
         t)
       mark-active
       (not (equal (mark) (point)))))

(defun iedit-barf-if-lib-active()
  "Signal error if Iedit lib is active."
  (or (and (null iedit-occurrences-overlays)
           (null iedit-read-only-occurrences-overlays))
      (error "Iedit lib is active")))

(defun iedit-barf-if-buffering()
  "Signal error if Iedit lib is buffering."
  (or  (null iedit-buffering)
      (error "Iedit is buffering")))

(provide 'iedit-lib)

;;; iedit-lib.el ends here

;;  LocalWords:  iedit el MERCHANTABILITY kbd isearch todo ert Lindberg Tassilo
;;  LocalWords:  eval rect defgroup defcustom boolean defvar assq alist nconc
;;  LocalWords:  substring cadr keymap defconst purecopy bkm defun princ prev
;;  LocalWords:  iso lefttab backtab upcase downcase concat setq autoload arg
;;  LocalWords:  refactoring propertize cond goto nreverse progn rotatef eq elp
;;  LocalWords:  dolist pos unmatch args ov sReplace iedit's cdr quote'ed yas
;;  LocalWords:  defface alists SPC mc cmds
