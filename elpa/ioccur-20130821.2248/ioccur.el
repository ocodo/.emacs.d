;;; ioccur.el --- Incremental occur

;; Copyright (C) 2010-2013  Free Software Foundation, Inc.

;; Author: Thierry Volpiatto <thierry dot volpiatto at gmail dot com>
;; X-URL: https://github.com/thierryvolpiatto/ioccur
;; Version: 2.4
;; Package-Version: 20130821.2248
;; Compatibility: GNU Emacs >=22.3

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.

;;; Install:
;;
;; Add this file to your `load-path', BYTE-COMPILE it and
;; add (require 'ioccur) in your .emacs.
;;
;; Start with (C-u) M-x ioccur
;;            or
;;            (C-u) M-x ioccur-find-buffer-matching
;;
;; Do C-h f ioccur or ioccur-find-buffer-matching for more info.

;;; Commentary:
;;
;; This package provides the command M-x ioccur, which is similar to
;; M-x occur, except that it is incremental.
;;
;; You can jump and quit to an occurrence, or jump and save the search
;; buffer (ioccur-buffer) for further use.  You can toggle literal and
;; regexp searching while running.  It is auto documented both in
;; mode-line and tooltip.  It has its own history, `ioccur-history',
;; which is a real ring.
;;
;; To save `ioccur-history' via the Desktop package, add this to your
;; init file (see (info "(emacs) Saving Emacs Sessions") for details):
;;
;; (add-to-list 'desktop-globals-to-save 'ioccur-history)

;;; Code:
(require 'derived)
(eval-when-compile (require 'cl))
(require 'outline)
(eval-when-compile (require 'wdired))

(defvar ioccur-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")        'ioccur-quit)
    (define-key map (kbd "RET")      'ioccur-jump-and-quit)
    (define-key map (kbd "<left>")   'ioccur-jump-and-quit)
    (define-key map (kbd "<right>")  'ioccur-jump-without-quit)
    (define-key map (kbd "C-z")      'ioccur-jump-without-quit)
    (define-key map (kbd "<C-down>") 'ioccur-scroll-down)
    (define-key map (kbd "<C-up>")   'ioccur-scroll-up)
    (define-key map (kbd "C-v")      'ioccur-scroll-other-window-up)
    (define-key map (kbd "M-v")      'ioccur-scroll-other-window-down)
    (define-key map (kbd "<down>")   'ioccur-next-line)
    (define-key map (kbd "<up>")     'ioccur-precedent-line)
    (define-key map (kbd "C-n")      'ioccur-next-line)
    (define-key map (kbd "C-p")      'ioccur-precedent-line)
    (define-key map (kbd "R")        'ioccur-restart)
    (define-key map (kbd "C-|")      'ioccur-split-window)
    (define-key map (kbd "M-<")      'ioccur-beginning-of-buffer)
    (define-key map (kbd "M->")      'ioccur-end-of-buffer)
    map)
  "Keymap used for ioccur commands.")


(defgroup ioccur nil
  "Mode that provide incremental searching in buffer."
  :prefix "ioccur-"
  :group 'text)

;;; User variables.
(defcustom ioccur-search-delay 0.5
  "During incremental searching, display is updated all these seconds."
  :group 'ioccur
  :type  'integer)

(defcustom ioccur-search-prompt "Pattern: "
  "Prompt used for `ioccur-occur'."
  :group 'ioccur
  :type  'string)

(defcustom ioccur-mode-line-string
  (if (window-system)
      " RET:Exit,C-g:Quit,C-j/left:Jump&quit,C-z/right:Jump,\
C-k/x:Kill(as sexp),M-p/n:Hist,C/M-v:Scroll,C-down/up:Follow,C-w:Yank tap"

      " RET:Exit,C-g:Quit,C-j:Jump&quit,C-z:Jump,C-k/x:Kill(as sexp),\
S-/Tab:Hist,C-v/t:Scroll,C-d/u:Follow,C-w:Yank tap")

  "Minimal documentation of `ioccur' commands displayed in mode-line.
Set it to nil to remove doc in mode-line."
  :group 'ioccur
  :type  'string)

(defcustom ioccur-length-line 80
  "Length of the line displayed in ioccur buffer.
When set to nil lines displayed in `ioccur-buffer' will not be modified.
See `ioccur-truncate-line'."
  :group 'ioccur
  :type 'integer)

(defcustom ioccur-max-length-history 100
  "Maximum number of element stored in `ioccur-history'."
  :group 'ioccur
  :type 'integer)

(defcustom ioccur-buffer-completion-use-ido nil
  "Use ido to choose buffers in `ioccur-find-buffer-matching'."
  :group 'ioccur
  :type 'symbol)

(defcustom ioccur-default-search-function 're-search-forward
  "Default search function.
Use here one of `re-search-forward' or `search-forward'."
  :group 'ioccur
  :type 'symbol)

(defcustom ioccur-highlight-match-p t
  "Highlight matchs in `ioccur-buffer' when non--nil."
  :group 'ioccur
  :type 'boolean)

(defcustom ioccur-fontify-buffer-p nil
  "Fontify `ioccur-current-buffer' when non--nil.
This allow to have syntactic coloration in `ioccur-buffer' but
it slow down the start of ioccur at first time on large buffers."
  :group 'ioccur
  :type 'boolean)

(defcustom ioccur-case-fold-search 'smart
  "Add 'smart' option to `case-fold-search'.
When smart is enabled, Ignore case in the search strings
if pattern contains no uppercase characters.
Otherwise, with a nil or t value, the behavior is same as
`case-fold-search'.
Default value is smart, other possible values are nil and t."
  :group 'ioccur
  :type 'symbol)

(defvar ioccur-read-char-or-event-skip-read-key nil
  "Force not using `read-key' to read input in minibuffer even if bounded.
Set it to non--nil if menu disapear or if keys are echoing in minibuffer.
Deprecated, should be used only in old Emacs versions.")

(defvar ioccur-save-pos-before-jump-hook nil
  "A hook that run before jumping and quitting `ioccur'.")

;;; Faces.
(defface ioccur-overlay-face
    '((t (:background "Green4" :underline t)))
  "Face for highlight line in ioccur buffer."
  :group 'ioccur-faces)

(defface ioccur-match-overlay-face
    '((t (:background "Indianred4" :underline t)))
  "Face for highlight line in matched buffer."
  :group 'ioccur-faces)

(defface ioccur-title-face
    '((t (:background "Dodgerblue4")))
  "Face for highlight incremental buffer title."
  :group 'ioccur-faces)

(defface ioccur-regexp-face
    '((t (:background "DeepSkyBlue" :underline t)))
  "Face for highlight found regexp in `ioccur-buffer'."
  :group 'ioccur-faces)

(defface ioccur-match-face
    '((t (:background "DeepSkyBlue")))
  "Face for highlight matches in `ioccur-buffer'."
  :group 'ioccur-faces)

(defface ioccur-num-line-face
    '((t (:foreground "OrangeRed")))
  "Face for highlight number line in ioccur buffer."
  :group 'ioccur-faces)

(defface ioccur-invalid-regexp
    '((t (:foreground "Goldenrod")))
  "Face for highlight wrong regexp message in ioccur buffer."
  :group 'ioccur-faces)

(defface ioccur-cursor
    '((t (:foreground "green")))
  "Face for cursor color in minibuffer."
  :group 'ioccur-faces)

;;; Internal variables.
;; String entered in prompt.
(defvar ioccur-pattern "")
;; The ioccur timer.
(defvar ioccur-search-timer nil)
;; Signal C-g hit.
(defvar ioccur-quit-flag nil)
;; The buffer we search in.
(defvar ioccur-current-buffer nil)
;; The overlay in `ioccur-buffer'.
(defvar ioccur-occur-overlay nil)
(make-variable-buffer-local 'ioccur-occur-overlay)
;; Signal we quit and kill `ioccur-buffer'.
(defvar ioccur-exit-and-quit-p nil)
;; A list to store history.
(defvar ioccur-history nil)
;; The overlay in `ioccur-current-buffer'.
(defvar ioccur-match-overlay nil)
;; Number of occurences found.
(defvar ioccur-count-occurences 0)
;;The buffer where we send results.
(defvar ioccur-buffer nil)
(make-variable-buffer-local 'ioccur-buffer)
;; True when jumping to a founded occurence.
(defvar ioccur-success nil)
;; Search function actually in use.
(defvar ioccur-search-function ioccur-default-search-function)
;; Message to send when ioccur exit
(defvar ioccur-message nil)
;; Store last window-configuration
(defvar ioccur-last-window-configuration nil)
;; Save point in current buffer here.
(defvar ioccur-current-pos nil)

(define-derived-mode ioccur-mode
    text-mode "ioccur"
    "Major mode to search occurences of regexp in current buffer.

Special commands:
\\{ioccur-mode-map}"
    (if ioccur-mode-line-string
        (setq mode-line-format
              '(" " mode-line-buffer-identification " "
                (line-number-mode "%l") " "
                ioccur-mode-line-string "-%-"))
        (kill-local-variable 'mode-line-format)))

(defsubst* ioccur-position (item seq &key (test 'eq))
  "A simple replacement of CL `position'."
  (loop for i in seq for index from 0
     when (funcall test i item) return index))

;; Compatibility
(unless (fboundp 'window-system)
  (defun window-system (&optional _arg)
    window-system))

;;; Iterators.
(defmacro ioccur-iter-list (list-obj)
  "Return an iterator from list LIST-OBJ."
  `(lexical-let ((lis ,list-obj))
     (lambda ()
       (let ((elm (car lis)))
         (setq lis (cdr lis))
         elm))))

(defun ioccur-iter-next (iterator)
  "Return next elm of ITERATOR."
  (funcall iterator))

(defun ioccur-iter-circular (seq)
  "Infinite iteration on SEQ."
  (lexical-let ((it  (ioccur-iter-list seq))
                (lis seq))
    (lambda ()
      (let ((elm (ioccur-iter-next it)))
        (or elm
            (progn (setq it (ioccur-iter-list lis))
                   (ioccur-iter-next it)))))))

(defun ioccur-butlast (seq pos)
  "Return SEQ from index 0 to POS."
  (butlast seq (- (length seq) pos)))

(defun* ioccur-sub-prec-circular (seq elm &key (test 'eq))
  "Infinite reverse iteration of SEQ starting at ELM."
  (lexical-let* ((rev-seq  (reverse seq))
                 (pos      (ioccur-position elm rev-seq :test test))
                 (sub      (append (nthcdr (1+ pos) rev-seq)
                                   (ioccur-butlast rev-seq pos)))
                 (iterator (ioccur-iter-list sub)))
     (lambda ()
       (let ((elm (ioccur-iter-next iterator)))
         (or elm
             (progn (setq iterator (ioccur-iter-list sub))
                    (ioccur-iter-next iterator)))))))

(defun* ioccur-sub-next-circular (seq elm &key (test 'eq))
  "Infinite iteration of SEQ starting at ELM."
  (lexical-let* ((pos      (ioccur-position elm seq :test test))
                 (sub      (append (nthcdr (1+ pos) seq)
                                   (ioccur-butlast seq pos)))
                 (iterator (ioccur-iter-list sub)))
     (lambda ()
       (let ((elm (ioccur-iter-next iterator)))
         (or elm (progn
                   (setq iterator (ioccur-iter-list sub))
                   (ioccur-iter-next iterator)))))))

(defun ioccur-print-results (regexp)
  "Print in `ioccur-buffer' lines matching REGEXP in `ioccur-current-buffer'."
  (setq ioccur-count-occurences 0)
  (with-current-buffer ioccur-current-buffer
    (let ((case-fold-search (case ioccur-case-fold-search
                              (smart (let ((case-fold-search nil))
                                       (if (string-match "[A-Z]" regexp) nil t)))
                              (t ioccur-case-fold-search))))
      (save-excursion
        (goto-char (point-min))
        (loop
              while (not (eobp))
              ;; We need to read also C-g from here
              ;; Because when loop is started `ioccur-read-search-input'
              ;; will read key only when loop is finished
              ;; and we have no chance to exit loop.
              when quit-flag do (setq ioccur-quit-flag t) and return nil
              for count from 0
              when (funcall ioccur-search-function regexp (point-at-eol) t)
              do (ioccur-print-line
                  (buffer-substring (point-at-bol) (point-at-eol))
                  count (match-string 0))
              do (forward-line 1))))))


(defun ioccur-print-match (str &optional all)
  "Highlight in string STR all occurences matching `ioccur-pattern'.
If ALL is non--nil highlight the whole string STR."
  (condition-case nil
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (if all
            (add-text-properties
             (point) (point-at-eol)
             '(face ioccur-match-face))  
            (while (and (funcall ioccur-search-function ioccur-pattern nil t)
                        ;; Don't try to highlight line with a length <= 0.
                        (> (- (match-end 0) (match-beginning 0)) 0))
              (add-text-properties
               (match-beginning 0) (match-end 0)
               '(face ioccur-match-face))))
        (buffer-string))
    (error nil)))

(defun ioccur-print-line (line nline match)
  "Prepare and insert a matched LINE at line number NLINE in `ioccur-buffer'."
  (with-current-buffer ioccur-buffer
    (let* ((lineno             (int-to-string (1+ nline)))
           (whole-line-matched (string= match line))
           (hightline          (if ioccur-highlight-match-p
                                   (ioccur-print-match
                                    line
                                    whole-line-matched)
                                   line))
           (trunc-line          (ioccur-truncate-line hightline)))
      (incf ioccur-count-occurences)
      (insert " " (propertize lineno 'face 'ioccur-num-line-face
                              'help-echo line)
              ":" trunc-line "\n"))))

(defun* ioccur-truncate-line (line &optional (columns ioccur-length-line))
  "Remove indentation in LINE and truncate modified LINE of num COLUMNS.
COLUMNS default value is `ioccur-length-line'.
If COLUMNS is nil return original indented LINE.
If COLUMNS is 0 only remove indentation in LINE.
So just set `ioccur-length-line' to nil if you don't want lines truncated."
  (let ((old-line line))
    (when (string-match "^[[:blank:]]*" line)
      ;; Remove tab and spaces at beginning of LINE.
      (setq line (replace-match "" nil nil line)))
    (if (and columns (> columns 0) (> (length line) columns))
        (substring line 0 columns)
        (if columns line old-line))))

(defun ioccur-buffer-contain (buffer regexp)
  "Return BUFFER if it contain an occurence of REGEXP."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward regexp nil t) buffer))))

(defun ioccur-list-buffers-matching (buffer-match regexp buffer-list)
  "Collect all buffers in BUFFER-LIST whose names match BUFFER-MATCH and \
contain lines matching REGEXP."
  (loop
     with ini-buf-list = (loop for buf in buffer-list
                            unless (rassq buf dired-buffers)
                            collect buf)
     for buf in ini-buf-list
     for bname = (buffer-name buf)
     when (and (string-match buffer-match bname)
               (ioccur-buffer-contain buf regexp))
     collect bname))

(defun ioccur-list-buffers-containing (regexp buffer-list)
  "Collect all buffers in BUFFER-LIST containing lines matching REGEXP."
  (loop with buf-list = (loop for i in buffer-list
                           when (buffer-file-name (get-buffer i))
                           collect i)
     for buf in buf-list
     when (ioccur-buffer-contain buf regexp)
     collect (buffer-name buf)))

(defun* ioccur-find-buffer-matching1 (regexp
                                      &optional
                                      match-buf-name
                                      (buffer-list (buffer-list)))
  "Find all buffers containing a text matching REGEXP \
and connect `ioccur' to the selected one.

If MATCH-BUF-NAME is non--nil search is performed only in buffers
with name matching specified expression (prompt).

Hitting C-g in a `ioccur' session will return to completion list.
Hitting C-g in the completion list will jump back to initial buffer.

The buffer completion list is provided by one of:
`ido-completing-read', `completing-read'
depending on which `ioccur-buffer-completion-use-ido' you have choosen."
  ;; Remove doublons maybe added by minibuffer in `ioccur-history'.
  (setq ioccur-history
        (loop for i in ioccur-history
           when (not (member i hist)) collect i into hist
           finally return hist))

  (let ((prompt   (format "Search (%s) in Buffer: " regexp))
        (win-conf (current-window-configuration))
        (buf-list (if match-buf-name
                      (ioccur-list-buffers-matching
                       (read-string "In Buffer names matching: ")
                       regexp buffer-list)
                      (ioccur-list-buffers-containing regexp buffer-list))))

    (labels
        ((find-buffer ()
           (let ((buf (if (and ido-mode
                               (eq ioccur-buffer-completion-use-ido 'ido))
                          (ido-completing-read prompt buf-list nil t)
                          (completing-read prompt buf-list nil t))))
             (unwind-protect
                  (progn
                    (switch-to-buffer buf)
                    (ioccur regexp)
                    ;; Exit if we jump to this `ioccur-current-buffer',
                    ;; otherwise, if C-g is hitten,
                    ;; go back to buffer completion list.
                    (unless ioccur-success
                      (find-buffer)))
               ;; C-g hit in buffer completion restore window config.
               (unless ioccur-success
                 (set-window-configuration win-conf))))))

      (find-buffer))))

(defvar savehist-save-minibuffer-history)

;;;###autoload
(defun ioccur-find-buffer-matching (regexp)
  "Find all buffers containing a text matching REGEXP.
See `ioccur-find-buffer-matching1'."
  (interactive (list (let ((savehist-save-minibuffer-history nil))
                       (read-from-minibuffer "Search for Pattern: "
                                             nil nil nil '(ioccur-history . 0)
                                             (thing-at-point 'symbol)))))
  (ioccur-find-buffer-matching1 regexp current-prefix-arg))

;;; Ioccur dired
;;;###autoload
(defun ioccur-dired (regexp)
  (interactive (list (let ((savehist-save-minibuffer-history nil))
                       (read-from-minibuffer "Search for Pattern: "
                                             nil nil nil '(ioccur-history . 0)
                                             (thing-at-point 'symbol)))))
  (let ((buf-list (loop for f in (dired-get-marked-files)
                     do (find-file-noselect f)
                     unless (file-directory-p f)
                     collect (get-buffer (file-name-nondirectory f)))))
    (ioccur-find-buffer-matching1 regexp nil buf-list)))

;;;###autoload
(defun ioccur-restart ()
  "Restart `ioccur' from `ioccur-buffer'.
`ioccur-buffer' is erased and a new search is started."
  (interactive)
  (when (eq major-mode 'ioccur-mode)
    (pop-to-buffer ioccur-current-buffer)
    (kill-buffer ioccur-buffer)
    (set-window-configuration ioccur-last-window-configuration)
    (ioccur)))

;;;###autoload
(defun ioccur-quit ()
  "Quit `ioccur-buffer'."
  (interactive)
  (let ((pos (with-current-buffer ioccur-current-buffer (point))))
    (when ioccur-match-overlay
      (delete-overlay ioccur-match-overlay))
    (quit-window)
    (set-window-configuration ioccur-last-window-configuration)
    (pop-to-buffer ioccur-current-buffer)
    (goto-char pos)))

(defun ioccur-goto-line (lineno)
  "Goto LINENO without modifying outline visibility if needed."
  (goto-char (point-min))
  (forward-line (1- lineno))
  (if (and (fboundp 'org-reveal)
           (or (derived-mode-p 'org-mode)
               outline-minor-mode))
      (org-reveal)))

(defun ioccur-forward-line (n)
  "Forward N lines but empty one's."
  (let (pos)
    (save-excursion
      (forward-line n) (forward-line 0)
      (when (looking-at "^\\s-[0-9]+:")
        (forward-line 0) (setq pos (point))))
  (when pos (goto-char pos) (ioccur-color-current-line))))

;;;###autoload
(defun ioccur-next-line ()
  "Goto next line if it is not an empty line."
  (interactive)
  (ioccur-forward-line 1))

;;;###autoload
(defun ioccur-precedent-line ()
  "Goto precedent line if it is not an empty line."
  (interactive)
  (ioccur-forward-line -1))

;;;###autoload
(defun ioccur-beginning-of-buffer ()
  "Goto beginning of `ioccur-buffer'."
  (interactive)
  (when (looking-at "^\\s-[0-9]+:")
    (goto-char (point-min))
    (re-search-forward "^\\s-[0-9]+:" nil t)
    (forward-line 0)
    (ioccur-color-current-line)))

;;;###autoload
(defun ioccur-end-of-buffer ()
  "Go to end of `ioccur-buffer'."
  (interactive)
  (when (looking-at "^\\s-[0-9]+:")
    (goto-char (point-max))
    (forward-line -1)
    (ioccur-color-current-line)))

(defun ioccur-jump (&optional win-conf)
  "Jump to line in other buffer and put an overlay on it.
Move point to first occurence of `ioccur-pattern'."
  (let* ((line           (buffer-substring (point-at-bol) (point-at-eol)))
         (pos            (string-to-number line)))
    (unless (string= line "")
      (if win-conf
          (set-window-configuration win-conf)
          (pop-to-buffer ioccur-current-buffer))
      (ioccur-goto-line pos)
      (recenter)
      ;; Go to beginning of first occurence in this line
      ;; of what match `ioccur-pattern'.
      (when (funcall ioccur-search-function
                     ioccur-pattern (point-at-eol) t)
        (goto-char (match-beginning 0)))
      (ioccur-color-matched-line))))

;;;###autoload
(defun ioccur-jump-and-quit ()
  "Jump to line in other buffer and quit search buffer."
  (interactive)
  (run-hooks 'ioccur-save-pos-before-jump-hook)
  (when (ioccur-jump ioccur-last-window-configuration)
    (sit-for 0.3)
    (when ioccur-match-overlay
      (delete-overlay ioccur-match-overlay))))

(defun ioccur-save-current-pos-to-mark-ring ()
  "Save current buffer position to mark ring.
To use this add it to `ioccur-save-pos-before-jump-hook'."
  (with-current-buffer ioccur-current-buffer
    (set-marker (mark-marker) ioccur-current-pos)
    (push-mark ioccur-current-pos 'nomsg)))

;;;###autoload
(defun ioccur-jump-without-quit (&optional mark)
  "Jump to line in `ioccur-current-buffer' without quitting."
  (interactive)
  (when (ioccur-jump ioccur-last-window-configuration)
    (and mark (set-marker (mark-marker) (point))
         (push-mark (point) 'nomsg))
    (switch-to-buffer-other-window ioccur-buffer t)))

;;;###autoload
(defun ioccur-scroll-other-window-down ()
  "Scroll other window down."
  (interactive)
  (let ((other-window-scroll-buffer ioccur-current-buffer))
    (scroll-other-window 1)))

;;;###autoload
(defun ioccur-scroll-other-window-up ()
  "Scroll other window up."
  (interactive)
  (let ((other-window-scroll-buffer ioccur-current-buffer))
    (scroll-other-window -1)))

(defun ioccur-scroll (n)
  "Scroll `ioccur-buffer' and `ioccur-current-buffer' simultaneously."
  (ioccur-forward-line n)
  (ioccur-color-current-line)
  (and (ioccur-jump ioccur-last-window-configuration)
       (switch-to-buffer-other-window ioccur-buffer t)))

;;;###autoload
(defun ioccur-scroll-down ()
  "Scroll down `ioccur-buffer' and `ioccur-current-buffer' simultaneously."
  (interactive)
  (ioccur-scroll 1))

;;;###autoload
(defun ioccur-scroll-up ()
  "Scroll up `ioccur-buffer' and `ioccur-current-buffer' simultaneously."
  (interactive)
  (ioccur-scroll -1))

;;;###autoload
(defun ioccur-split-window ()
  "Toggle split window, vertically or horizontally."
  (interactive)
  (with-current-buffer ioccur-current-buffer
    (let ((old-size (window-height)))
      (delete-window)
      (set-window-buffer
       (select-window (if (= (window-height) old-size)
                          (split-window-vertically)
                          (split-window-horizontally)))
       (get-buffer ioccur-buffer)))))

(defun ioccur-read-char-or-event (prompt)
  "Replace `read-key' when not available using PROMPT."
  (if (and (fboundp 'read-key)
           (not ioccur-read-char-or-event-skip-read-key))
      (read-key prompt)
      (let* ((chr (condition-case nil (read-char prompt) (error nil)))
             (evt (unless chr (read-event prompt))))
        (or chr evt))))

(defun ioccur-read-search-input (initial-input start-point)
  "Read each keyboard input and add it to `ioccur-pattern'.
INITIAL-INPUT is a string given as default input, generally thing at point.
START-POINT is the point where we start searching in buffer."
  (let* ((prompt         (propertize ioccur-search-prompt
                                     'face 'minibuffer-prompt))
         (inhibit-quit   (or (eq system-type 'windows-nt)
                             (not (fboundp 'read-key))
                             ioccur-read-char-or-event-skip-read-key))
         (tmp-list       ())
         (it-prec        nil)
         (it-next        nil)
         (cur-hist-elm   (car ioccur-history))
         (start-hist     nil) ; Flag to notify if cycling history started.
         yank-point
         (index 0))
    (unless (string= initial-input "")
      (loop for char across initial-input do (push char tmp-list)))
    (setq ioccur-pattern initial-input)
    ;; Cycle history function.
    ;;
    (flet ((cycle-hist (arg)
             ;; ARG can be positive or negative depending we call M-p or M-n.
             (if ioccur-history
                 (progn
                   ;; Cycle history will start at second call,
                   ;; at first call just use the car of hist ring.
                   ;; We build a new iterator based on a sublist
                   ;; starting at the current element of history.
                   ;; This is a circular iterator. (no end)
                   (if start-hist ; At first call, start-hist is nil.
                       (progn
                         (if (< arg 0)
                             ;; M-p (move from left to right in hist ring).
                             (unless it-prec ; Don't rebuild iterator if exists.
                               (setq it-prec (ioccur-sub-next-circular
                                              ioccur-history
                                              cur-hist-elm :test 'equal))
                               (setq it-next nil)) ; Kill forward iterator.
                             ;; M-n (move from right to left in hist ring).
                             (unless it-next ; Don't rebuild iterator if exists.
                               (setq it-next (ioccur-sub-prec-circular
                                              ioccur-history
                                              cur-hist-elm :test 'equal))
                               (setq it-prec nil))) ; kill backward iterator.
                         (let ((it (or it-prec it-next)))
                           (setq cur-hist-elm (ioccur-iter-next it))
                           (setq tmp-list nil)
                           (loop for char across cur-hist-elm
                                 do (push char tmp-list))
                           (setq ioccur-pattern cur-hist-elm)))
                       ;; First call use car of history ring.
                       (setq tmp-list nil)
                       (loop for char across cur-hist-elm
                             do (push char tmp-list))
                       (setq ioccur-pattern cur-hist-elm)
                       (setq start-hist t)))
                 (message "No history available.") (sit-for 2) t))
           ;; Insert INITIAL-INPUT.
           ;;
           (insert-initial-input ()
             (unless (string= initial-input "")
               (loop for char across initial-input
                     do (push char (nthcdr index tmp-list)))))
           ;; Maybe start timer.
           ;;
           (start-timer ()
             (unless ioccur-search-timer
               (ioccur-start-timer)))
           ;; Maybe stop timer.
           ;;
           (stop-timer ()
             (when ioccur-search-timer
               (ioccur-cancel-search)))
           ;; Kill pattern
           ;;
           (kill (str)
             (with-current-buffer ioccur-current-buffer
               (goto-char start-point)
               (setq yank-point start-point))
             (kill-new (substring str (- (length tmp-list) index)))
             (setq tmp-list (nthcdr index tmp-list)))
           ;; Add cursor in minibuffer
           ;;
           (set-cursor (str pos)
             (setq pos (min index (1- (length tmp-list))))
             (when (not (string= str ""))
               (let* ((real-index (- (1- (length tmp-list)) pos))
                      (cur-str (substring str real-index (1+ real-index))))
                 (concat (substring str 0 real-index)
                         (propertize cur-str 'display
                                     (if (= index (length tmp-list))
                                         (concat
                                          (propertize "|" 'face 'ioccur-cursor)
                                          cur-str)
                                         (concat
                                          cur-str
                                          (propertize "|" 'face 'ioccur-cursor))))
                         (substring str (1+ real-index)))))))
      
      ;; Start incremental loop.
      (while (let ((char (ioccur-read-char-or-event
                          (concat prompt (set-cursor ioccur-pattern index)))))
               (message nil)
               (case char
                 ((not (?\M-p ?\M-n ?\t C-tab)) ; Reset history
                  (setq start-hist nil)
                  (setq cur-hist-elm (car ioccur-history)) t)
                 ((down ?\C-n)                  ; Next line.
                  (stop-timer) (ioccur-next-line)
                  (ioccur-color-current-line) t)
                 ((up ?\C-p)                    ; Precedent line.
                  (stop-timer) (ioccur-precedent-line)
                  (ioccur-color-current-line) t)
                 (?\M-<                         ; Beginning of buffer.
                  (when (ioccur-beginning-of-buffer)
                    (stop-timer)) t)
                 (?\M->                         ; End of buffer.
                  (when (ioccur-end-of-buffer)
                    (stop-timer)) t)
                 ((?\C-d C-down)                ; Scroll both windows down.
                  (stop-timer) (ioccur-scroll-down) t)
                 ((?\C-u C-up)                  ; Scroll both windows up.
                  (stop-timer) (ioccur-scroll-up) t)
                 (?\r                           ; RET break and exit code.
                  nil)
                 (?\d                           ; Delete backward with DEL.
                  (start-timer)
                  (with-current-buffer ioccur-current-buffer
                    (goto-char start-point)
                    (setq yank-point start-point))
                  (with-no-warnings (pop (nthcdr index tmp-list)))
                  t)
                 (?\C-g                         ; Quit and restore buffers.
                  (setq ioccur-quit-flag t) nil)
                 ((right ?\C-z)                 ; Persistent action.
                  (ioccur-jump-without-quit) t)
                 ((?\C- )                       ; Persistent action save mark.
                  (ioccur-jump-without-quit t) t)                 
                 ((left ?\C-j)                  ; Jump and kill search buffer.
                  (setq ioccur-exit-and-quit-p t) nil)
                 ((next ?\C-v)                  ; Scroll down.
                  (ioccur-scroll-other-window-down) t)
                 ((?\C-t ?\M-v prior)           ; Scroll up.
                  (ioccur-scroll-other-window-up) t)
                 (?\C-s                         ; Toggle split window.
                  (ioccur-split-window) t)
                 ((?\C-: ?\C-l)                 ; Toggle regexp/litteral search.
                  (start-timer)
                  (if (eq ioccur-search-function 're-search-forward)
                      (setq ioccur-search-function 'search-forward)
                      (setq ioccur-search-function 're-search-forward)) t)
                 (?\C-k                         ; Kill input.
                  (start-timer)
                  (kill ioccur-pattern) (setq index 0) t)
                 ((?\M-k ?\C-x)                 ; Kill input as sexp.
                  (start-timer)
                  (let ((sexp (prin1-to-string ioccur-pattern)))
                    (kill sexp)
                    (setq ioccur-quit-flag t)
                    (setq ioccur-message (format "Killed: %s" sexp)))
                  nil)
                 (?\C-y                         ; Yank from `kill-ring'.
                  (setq initial-input (car kill-ring))
                  (insert-initial-input) t)
                 (?\C-w                         ; Yank stuff at point.
                  (start-timer)
                  (with-current-buffer ioccur-current-buffer
                    ;; Start to initial point if C-w have never been hit.
                    (unless yank-point (setq yank-point start-point))
                    ;; After a search `ioccur-print-results' have put point
                    ;; to point-max, so reset position.
                    (when yank-point (goto-char yank-point))
                    (let ((pmax (point-at-eol))
                          (eoword (save-excursion (forward-word 1) (point))))
                      ;; Don't yank further than eol.
                      (unless (> eoword pmax)
                        (goto-char eoword)
                        (setq initial-input (buffer-substring-no-properties
                                             yank-point (point)))
                        (setq yank-point (point)) ; End of last forward-word
                        (insert-initial-input)))) t)
                 ((?\t ?\M-p)                   ; Precedent history elm.
                  (start-timer)
                  (setq index 0)
                  (cycle-hist -1))
                 ((backtab ?\M-n)               ; Next history elm.
                  (start-timer)
                  (setq index 0)
                  (cycle-hist 1))
                 (?\C-q                         ; quoted-insert.
                  (stop-timer)
                  (let ((char (with-temp-buffer
                                (call-interactively 'quoted-insert)
                                (buffer-string))))
                    (push (string-to-char char) tmp-list))
                  (start-timer)
                  t)
                 ;; Movements in minibuffer
                 (?\C-b                         ; backward-char.
                  (setq index (min (1+ index) (length tmp-list))) t)
                 (?\C-f                         ; forward-char.
                  (setq index (max (1- index) 0)) t)
                 (?\C-a                         ; move bol.
                  (setq index (length tmp-list)) t)
                 (?\C-e                         ; move eol.
                  (setq index 0) t)
                 (t                             ; Store character.
                  (start-timer)
                  (if (characterp char)
                      (push char (nthcdr index tmp-list))
                      (setq unread-command-events
                            (nconc (mapcar 'identity
                                           (this-single-command-raw-keys))
                                   unread-command-events))
                      nil))))
        (setq ioccur-pattern (apply 'string (reverse tmp-list)))))))

(defun ioccur-print-buffer (regexp)
  "Pretty Print results matching REGEXP in `ioccur-buffer'."
  ;; FIXME: Why force tooltip-mode?  What about sessions with both GUI and
  ;; tty frames?
  (unless (window-system) (setq tooltip-use-echo-area t) (tooltip-mode 1))
  (let* ((cur-method (if (eq ioccur-search-function 're-search-forward)
                         "Regexp" "Literal"))
         (title      (propertize
                      (format
                       "* Ioccur %s searching %s"
                       cur-method
                       (if (window-system)
                           "* (`C-:' to Toggle Method, Mouse over for help.)"
                           "* (`C-l' to Toggle Method.)"))
                      'face 'ioccur-title-face
                      'help-echo
                      "                  Ioccur map:\n
C-n or <down>      Next line.\n
C-p or <up>        Precedent line.\n
C-v and M-v/C-t    Scroll up and down.\n
C-z or <right>     Jump without quitting loop.\n
C-TAB              Jump without quitting and save to mark-ring.\n
C-j or <left>      Jump and kill `ioccur-buffer'.\n
RET                Exit keeping `ioccur-buffer'.\n
DEL                Remove last character entered.\n
C-k                Kill current input.\n
C-a/e/b/f          Movements in minibuffer.\n
M-k/C-x            Kill current input as sexp.\n
C-w                Yank stuff at point.\n
C-g                Quit and restore buffer.\n
C-s                Toggle split window.\n
C-:/l              Toggle regexp/litteral search.\n
C-down or C-u      Follow in other buffer.\n
C-up/d or C-d      Follow in other buffer.\n
M-<, M->           Beginning and end of buffer.\n
M-p/n or tab/S-tab History."))
           wrong-regexp)
    (if (string= regexp "")
        (progn (erase-buffer) (insert title "\n\n"))
        (erase-buffer)
        (condition-case _
            (ioccur-print-results regexp)
          (error (setq wrong-regexp t)))
        (goto-char (point-min))
        (if wrong-regexp
            (insert
             title "\n\n"
             (propertize "Invalid Regexp: "
                         'face 'ioccur-invalid-regexp)
             (format "No match for `%s'" regexp) "\n\n")
            (insert title "\n\n"
                    (propertize (format "Found %s occurences matching "
                                        ioccur-count-occurences)
                                'face 'underline)
                    (propertize regexp 'face 'ioccur-regexp-face)
                    (propertize
                     (format " in %s" ioccur-current-buffer)
                     'face 'underline) "\n\n")
            (ioccur-color-current-line)))))

(defun ioccur-start-timer ()
  "Start ioccur incremental timer."
  (setq ioccur-search-timer
        (run-with-idle-timer
         ioccur-search-delay 'repeat
         #'(lambda ()
             (ioccur-print-buffer
              ioccur-pattern)))))

(defun ioccur-send-message ()
  "Send message defined in `ioccur-message'."
  (message ioccur-message))

;;;###autoload
(defun ioccur (&optional initial-input)
  "Incremental search of lines in current buffer matching input.
With a prefix arg search symbol at point (INITIAL-INPUT).

While you are incremental searching, commands provided are:

C-n or <down>  next line.
C-p or <up>    precedent line.
C-v and M-v    scroll up and down.
C-z or <right> jump without quitting loop.
C-j or <left>  jump and kill `ioccur-buffer'.
RET            exit keeping `ioccur-buffer'.
DEL            remove last character entered.
C-k            Kill current input from cursor to eol.
C-a/e/b/f      Movements in minibuffer.
M-k            Kill current input as sexp.
C-w            Yank stuff at point.
C-g            quit and restore buffer.
C-s            Toggle split window.
C-:            Toggle regexp/litteral search.
C-down         Follow in other buffer.
C-up           Follow in other buffer.
M-p/n          Precedent and next `ioccur-history' element.
M-<, M->       Beginning and end of buffer.

Unlike minibuffer history, cycling in ioccur history have no end:

M-p ,-->A B C D E F G H I---,
    |                       |
    `---I H G F E D C B A<--'

M-n ,-->I H G F E D C B A---,
    |                       |
    `---A B C D E F G H I<--'


Special NOTE for terms:
=======================
  tab/S-tab are bound to history.
  C-d/u are for following in other buffer.
  Use C-t to Scroll up.
 
When you quit incremental search with RET, see `ioccur-mode'
for commands provided in the `ioccur-buffer'."
  (interactive "P")
  (let (pop-up-frames)
    (setq ioccur-exit-and-quit-p nil)
    (setq ioccur-success nil)
    (setq ioccur-current-buffer (buffer-name (current-buffer)))
    (when ioccur-fontify-buffer-p
      (message "Fontifying buffer...Please wait it could be long.")
      (jit-lock-fontify-now) (message nil))
    (setq ioccur-buffer (concat "*ioccur-" ioccur-current-buffer "*"))
    (setq ioccur-last-window-configuration (current-window-configuration))
    (setq ioccur-current-pos (point))
    (if (and (not initial-input)
             (get-buffer ioccur-buffer)
             (not (get-buffer-window ioccur-buffer)))
        ;; An hidden `ioccur-buffer' exists jump to it and reuse it.
        (switch-to-buffer-other-window ioccur-buffer t)
        ;; `ioccur-buffer' doesn't exists or is visible, start searching
        ;; Creating a new `ioccur-buffer' or reusing the visible one after
        ;; erasing it.
        (let* ((init-str (if initial-input
                             (if (stringp initial-input)
                                 initial-input (thing-at-point 'symbol))
                             ""))
               (len      (length init-str))
               (curpos   (point))
               (inhibit-read-only t)
               (cur-mode (with-current-buffer ioccur-current-buffer
                           (prog1
                               major-mode
                             ;; If current `major-mode' is wdired
                             ;; Turn it off.
                             (when (eq major-mode 'wdired-mode)
                               (wdired-change-to-dired-mode)))))
               str-no-prop)
          (set-text-properties 0 len nil init-str)
          (setq str-no-prop init-str)
          (pop-to-buffer (get-buffer-create ioccur-buffer))
          (ioccur-mode)
          (unwind-protect
               ;; Start incremental search.
               (progn
                 (ioccur-start-timer)
                 (ioccur-read-search-input str-no-prop curpos))
            ;; At this point incremental search loop is exited.
            (progn
              (ioccur-cancel-search)
              (kill-local-variable 'mode-line-format)
              (when (equal (buffer-substring (point-at-bol) (point-at-eol)) "")
                (setq ioccur-quit-flag t))
              (cond (ioccur-quit-flag ; C-g hit or empty `ioccur-buffer'.
                     (kill-buffer ioccur-buffer)
                     (pop-to-buffer ioccur-current-buffer)
                     (when ioccur-match-overlay
                       (delete-overlay ioccur-match-overlay))
                     (set-window-configuration ioccur-last-window-configuration)
                     (goto-char curpos)
                     (ioccur-send-message)
                     ;; If `ioccur-message' is non--nil, thats mean we exit
                     ;; with a specific action other than `C-g',
                     ;; e.g kill-as-sexp, so we save history.
                     (when ioccur-message (ioccur-save-history)))
                    (ioccur-exit-and-quit-p ; Jump and kill `ioccur-buffer'.
                     (ioccur-jump-and-quit)
                     (kill-buffer ioccur-buffer)
                     (ioccur-send-message) (ioccur-save-history))
                    (t                 ; Jump keeping `ioccur-buffer'.
                     (ioccur-jump)
                     (pop-to-buffer ioccur-buffer)
                     (setq buffer-read-only t)
                     (ioccur-save-history)))
              ;; Maybe reenable `wdired-mode'.
              (when (eq cur-mode 'wdired-mode) (wdired-change-to-wdired-mode))
              (setq ioccur-count-occurences 0)
              (setq ioccur-quit-flag nil)
              (setq ioccur-message nil)
              (setq ioccur-search-function ioccur-default-search-function)
              (setq ioccur-current-pos nil)))))))

(defun ioccur-save-history ()
  "Save last ioccur element found in `ioccur-history'."
  (unless (string= ioccur-pattern "")
    (setq ioccur-history
          (cons ioccur-pattern (delete ioccur-pattern ioccur-history)))
    (when (> (length ioccur-history) ioccur-max-length-history)
      (setq ioccur-history (delete (car (last ioccur-history))
                                         ioccur-history)))
    (setq ioccur-success t)))

(defun ioccur-cancel-search ()
  "Cancel timer used for ioccur searching."
  (when ioccur-search-timer
    (cancel-timer ioccur-search-timer)
    (setq ioccur-search-timer nil)))

(defun ioccur-color-current-line ()
  "Highlight and underline current line in `ioccur-buffer'."
  (if ioccur-occur-overlay
      (move-overlay ioccur-occur-overlay
                    (point-at-bol) (1+ (point-at-eol)) ioccur-buffer)
      (setq ioccur-occur-overlay
            (make-overlay (point-at-bol) (1+ (point-at-eol)) ioccur-buffer)))
  (overlay-put ioccur-occur-overlay 'face 'ioccur-overlay-face))

(defun ioccur-color-matched-line ()
  "Highlight and underline current position \
of matched line in `ioccur-current-buffer'."
  (if ioccur-match-overlay
      (move-overlay ioccur-match-overlay
                    (point-at-bol) (1+ (point-at-eol)))
      (setq ioccur-match-overlay
            (make-overlay (point-at-bol) (1+ (point-at-eol)))))
  (overlay-put ioccur-match-overlay 'face 'ioccur-match-overlay-face))

            
(provide 'ioccur)

;;; ioccur.el ends here
