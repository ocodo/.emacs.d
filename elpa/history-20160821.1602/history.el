;;; history.el --- History utility for source code navigation
;;
;; Copyright (C) 2014-2015
;;
;; Author: boyw165
;; Version: 20141206.1800
;; URL: https://github.com/boyw165/history
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This tool is similar to `pop-global-mark' but more powerful. You can go
;; through the whole history without losing them. Actually, `pop-global-mark'
;; will use the latest record but also discard it. But this tool will preserve
;; all the history and smartly ignored killed buffers or invalid symbol string.
;;
;; You'll feel the power and convenience of using `history-add-history', 
;; `history-prev-history' and `history-next-history' instead of built-in old way.
;;
;; Basic Concept
;; -------------
;; * Normal history database:
;;   (1) - (2) - (3) - (4) - (5)
;;                            ^ index
;; * Goto previous Nth history:
;;   (1) - (2) - (3) - (4) - (5)
;;                ^ index
;; * Add a new history into the database:
;;   (1) - (2) - (3) - (6)
;;                      ^ index, histories behind index will be discard, and new
;;                        one will be appended to the end.
;;
;; Usage
;; -----
;; * M-x `history-mode'
;;   Add menu items and tool-bar items of history utility.
;; * (`history-add-history')
;;   Save current point and buffer as a history into the database.
;; * (`history-add-history' t)
;;   Like above, but also save symbol string at point. When navigating to the
;;   history, the tool compare the matched string so that it make sure the
;;   history is VALID.
;; * M-x `history-prev-history'
;;   Goto previous history.
;; * M-x `history-next-history'
;;   Goto new history.
;; * M-x `history-kill-histories'
;;   Discard whole history database.
;;
;; Advanced Usage
;; --------------
;; * M-x `history-configuration'
;;   Add history for you automatically for specific functions!!!
;;
;; Customization
;; -------------
;; * `history-history-max'
;;   The maximum length of the history database.
;; * `history-ignore-buffer-names'
;;   A REGEXP list to ignore specific buffers.
;; * `history-window-local-history'
;;   A boolean indicates the history is whether local to window or global to
;;   all buffers.
;; * `history-advised-after-functions' and `history-advised-after-functions'
;;   A functions list to be advised to call `history-add-history'.
;;
;; TODO:
;; -----
;; n/a
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2015-01-10
;; * Support `history-window-local-history' to make history local to window.
;; * Support `history-advised-before-functions' and
;;   `history-advised-after-functions'.
;; * Add `history-goto-history' menu.
;;
;; 2014-12-28
;; * Support `history-ignore-buffer-names' to ignore some buffer with specific
;;   names.
;; * Enhance visualization of `history-show-history'.
;; * Add `history-mode'.
;;
;; 2014-06-01
;; * Initial release.
;;
;;; Code:

;; GNU Library.
(require 'thingatpt)
(require 'tool-bar)
(require 'advice)

(defgroup history nil
  "A lightweight history utility."
  :group 'convenience)

(defgroup history-advice nil
  "Advising functions for history."
  :group 'history)

(defgroup history-face nil
  "Faces of history."
  :group 'history)

(defface history-prompt
  '((t (:inherit minibuffer-prompt :height 1.3)))
  "Face of prompt when calling `history-goto-history'."
  :group 'history-face)

(defface history-current-history
  '((t (:foreground "black" :background "gold1" :weight bold :height 1.8)))
  "Face for current history. See `history-histories-string'."
  :group 'history-face)

(defface history-current-temp-history
  '((t (:inherit history-current-history :underline t)))
  "Face for current history which is also a temporary history. 
See `history-histories-string'."
  :group 'history-face)

(defface history-other-history
  '((t (:foreground "dim gray" :background "#d1f5ea" :height 1.3)))
  "Face for other history. See `history-histories-string'."
  :group 'history-face)

(defface history-temp-history
  '((t (:inherit history-other-history :underline t)))
  "Face for other history which is also a temporary history. 
See `history-histories-string'."
  :group 'history-face)

(defcustom history-history-max 64
  "The maximum length of history."
  :type 'integer
  :group 'history)

(defcustom history-ignore-buffer-names '("^\\*.*\\*$")
  "The REGEXP list for matched ignore buffer names."
  :type '(repeat regexp)
  :group 'history)

(defcustom history-window-local-history nil
  "In some cases, window-local history will give big convenience to us. t means 
to use window-local history; nil means to use a global history."
  :type 'boolean
  :group 'history)

(defun history-set-advices (symbol value)
  "Customization setter for `history-advised-before-functions' and
`history-advised-after-functions'."
  (history-init-advices nil)
  (set symbol value)
  (history-init-advices t))

(defcustom history-advised-before-functions '(imenu
                                              isearch-mode
                                              beginning-of-buffer
                                              end-of-buffer)
  "Add history automatically before executing these functions'. 
See `advice' feature."
  :type '(repeat function)
  :initialize 'custom-initialize-default
  :set 'history-set-advices
  :group 'history-advice)

(defcustom history-advised-after-functions '()
  "Add history automatically after executing these functions'. 
See `advice' feature."
  :type '(repeat function)
  :initialize 'custom-initialize-default
  :set 'history-set-advices
  :group 'history-advice)

(defvar history-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'history-undefined)
    (define-key map (kbd "<up>") 'history-undefined)
    (define-key map (kbd "<down>") 'history-undefined)
    (define-key map (kbd "<left>") 'history-preview-prev-history)
    (define-key map (kbd "<right>") 'history-preview-next-history)
    (define-key map (kbd "C-p") 'history-preview-prev-history)
    (define-key map (kbd "C-n") 'history-preview-next-history)
    (define-key map (kbd "<return>") 'exit-minibuffer)
    (define-key map (kbd "q") 'history-preview-cancel-history)
    (define-key map (kbd "<escape>") 'history-preview-cancel-history)
    map)
  "The key map for browsing the history.")

(defvar history-stack nil
  "The history database. See `history-add-history' for details.")

(defvar history-index 0
  "The index of current history in the database.")

(defvar history-from-scratch? nil
  "t to remove all history, like start from scratch.")

(defvar history-window nil
  "The cached window for `history-goto-history' usage.")

(defun history-thingatpt (thing)
  "Adapter to `thing-at-point' for compatibility of Emacs 24.3 and 24.4."
  (cond
   ((= 4 emacs-minor-version) (thing-at-point thing t))
   ((= 3 emacs-minor-version)
    (let ((bounds (bounds-of-thing-at-point thing)))
      (and bounds
           (buffer-substring-no-properties (car bounds)
                                           (cdr bounds)))))))

(defun history-same-line? (pos1 pos2)
  "Is POS2 and POS2 (must in the same buffer) at same line."
  (let ((line-pos1 (save-excursion
                     (goto-char pos1)
                     (beginning-of-line)
                     (point)))
        (line-pos2 (save-excursion
                     (goto-char pos2)
                     (beginning-of-line)
                     (point))))
    (= line-pos1 line-pos2)))

(defun history-add? (new-history)
  "Check readiness to add history, like avoiding duplicates."
  (if history-stack
      (let* ((history (nth history-index history-stack))
             (marker (plist-get history :marker))
             (buffer (marker-buffer marker))
             (pos (marker-position marker))
             (symbol (plist-get history :symbol))
             ;; New history
             (new-symbol (plist-get new-history :symbol)))
        (not (and (eq (current-buffer) buffer)
                  (history-same-line? (point) pos)
                  (cond
                   (symbol
                    (equal new-symbol symbol))
                   (t
                    (= (point) pos))))))
    t))

(defun history-window ()
  "Return `history-window' if minibuffer is active; `selected-window' if 
inactive."
  (if (active-minibuffer-window)
      history-window
    (selected-window)))

(defun history-stack ()
  (if history-window-local-history
      (window-parameter nil 'history-stack)
    history-stack))

(defun history-index ()
  (if history-window-local-history
      (window-parameter nil 'history-index)
    history-index))

(defmacro history-do (&rest body)
  "Convenient macro to access `history-stack' and `history-index' without caring
whether `history-window-local-history' is true or false."
  (declare (indent 0) (debug t))
  `(let (global-stack
         global-index)
     (let ((history-stack (history-stack))
           (history-index (history-index)))
       ;; Evaluate BODY~
       (prog1 (progn ,@body)
         ;; Final save!!!
         (if history-window-local-history
             ;; Window-local history ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             (progn
               (set-window-parameter nil 'history-stack history-stack)
               (set-window-parameter nil 'history-index history-index))
           ;; Global history ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           (setq global-stack history-stack
                 global-index history-index))))
     (and global-index
          (setq history-stack global-stack
                history-index global-index))))

(defun history-create-history (save-thing? temp?)
  "Create a history."
  (current-buffer)
  (let ((thing (history-thingatpt 'symbol))
        (history (list :marker (copy-marker (point) t)
                       :window-start (window-start))))
    ;; Save the symbol string if SAVE-THING? is t.
    (and save-thing? thing
         (setq history (plist-put history :symbol thing)))
    ;; Make it a temporary entry if TEMP? is t.
    (and temp?
         (setq history (plist-put history :temp t)))
    history))

(defun history-sync-max ()
  "Keep total amount of history less than `history-history-max'."
  (and (> (length history-stack) history-history-max)
       (setcdr (nthcdr (1- history-history-max) history-stack) nil)))

(defun history-push-history (history)
  "Push history, which is exactly using `push'. For instance:
 <-- old    new -->
 (0) (1) (2) (3)
      ^ index
 (0) (1) (new)
           ^ index"
  (when (history-add? history)
    (if history-from-scratch?
        ;; Discard all histories if navigating beyond the oldest one.
        (setq history-stack nil)
      ;; Just discard the histories behind the index.
      (and history-stack (>= history-index 1)
           (let ((current (nthcdr history-index history-stack)))
             (setq history-stack (cdr current)))))
    ;; Add new history.
    (push history history-stack)
    (setq history-index 0)
    ;; Keep maximum.
    (history-sync-max)
    ;; Return history.
    history-stack))

(defun history-insert-history (history)
  "Insert history at current index. For instance:
 <-- old        new -->
 (0) (1) (2) (3)
          ^ index
 (0) (1) (2) (new) (3)
               ^ index"
  (let ((tail (append (list history)
                      (nthcdr history-index history-stack))))
    (if (= history-index 0)
        (setq history-stack tail)
      (setcdr (nthcdr (1- history-index) history-stack) tail))))

(defun history-move-history (step)
  (setq history-index (+ history-index step)
        history-from-scratch? nil)
  (cond
   ((>= history-index (length history-stack))
    (setq history-index (1- (length history-stack))
          history-from-scratch? t))
   ((< history-index 0)
    (setq history-index 0))
   (t history-index)))

(defun history-use-current-history ()
  (let* ((history (nth history-index history-stack))
         (marker (plist-get history :marker))
         (buffer (marker-buffer marker))
         (pos (marker-position marker))
         (wpos (plist-get history :window-start)))
    ;; Switch to buffer.
    (set-window-buffer (history-window) buffer)
    (set-buffer buffer)
    ;; Update window-start.
    (set-window-start nil wpos)
    ;; Update point.
    (goto-char pos)))

(defun history-remove-invalid-history (&optional remove-temp?)
  "Go through the histories and check each buffer's validness."
  (dolist (history history-stack)
    (let* ((marker (plist-get history :marker))
           (buffer (marker-buffer marker))
           (pos (marker-position marker))
           (symbol (plist-get history :symbol))
           (temp? (plist-get history :temp)))
      (if (buffer-live-p buffer)
          (cond
           ;; Remove it if thing at point doesn't match history.
           (symbol
            (with-current-buffer buffer
              (save-excursion
                (goto-char pos)
                (unless (equal symbol (history-thingatpt 'symbol))
                  (setq history-stack (delq history history-stack))))))
           ;; Remove temporary history.
           ((and remove-temp? temp?)
            (setq history-stack (delq history history-stack))))
        ;; Remove it if its buffer was killed.
        (setq history-stack (delq history history-stack)))))
  ;; Update index if necessary.
  (when (and history-stack
             (>= history-index (length history-stack)))
    (setq history-index (1- (length history-stack))))
  ;; Return current history.
  history-stack)

(defun history-histories-string ()
  "Histories list string."
  (let* ((total (length history-stack))
         (prompt (propertize (format "History %d/%d: "
                                     (- total (or history-index 0)) total)
                             'face 'history-prompt))
         value)
    (dolist (history history-stack)
      (setq value (concat (if (eq history (nth history-index history-stack))
                              (if (plist-get history :temp)
                                  (propertize "*"
                                              'face 'history-current-temp-history)
                                (propertize "*"
                                            'face 'history-current-history))
                            (if (plist-get history :temp)
                                (propertize "."
                                            'face 'history-temp-history)
                              (propertize "."
                                          'face 'history-other-history)))
                          value)))
    (concat prompt value)))

(defun history-undefined ()
  "Empty command for keymap binding."
  (interactive))

(defun history-preview-prev-history ()
  "Keymap function for previewing previous history."
  (interactive)
  (delete-minibuffer-contents)
  (setq history-index (1+ history-index))
  (and (>= history-index (length history-stack))
       (setq history-index (1- (length history-stack))))
  (insert (history-histories-string))
  (re-search-backward "\*")
  ;; Use history.
  (with-selected-window (history-window)
    (history-use-current-history)))

(defun history-preview-next-history ()
  "Keymap function for previewing next history."
  (interactive)
  (delete-minibuffer-contents)
  (setq history-index (1- history-index))
  (and (< history-index 0)
       (setq history-index 0))
  (insert (history-histories-string))
  (re-search-backward "\*")
  ;; Use history.
  (with-selected-window (history-window)
    (history-use-current-history)))

(defun history-preview-cancel-history ()
  "Keymap function for canceling history."
  (interactive)
  (delete-minibuffer-contents)
  (exit-minibuffer))

(defun history-init-advices (activate?)
  "Advise functions to call `history-add-history'.
See `history-advised-before-functions'
    `history-advised-after-functions'."
  ;; Before-advised.
  (mapc (lambda (func)
          (eval
           `(defadvice ,func (before history-add-history
                                     ,(if activate? 'activate 'disable))
              (history-add-history))))
        history-advised-before-functions)
  ;; After-advised.
  (mapc (lambda (func)
          (eval
           `(defadvice ,func (after history-add-history
                                    ,(if activate? 'activate 'disable))
              (history-add-history))))
        history-advised-after-functions))

(defun history-configuration ()
  "Configure history group."
  (interactive)
  (customize-group 'history))

(defun history-ignore-buffer? (&optional buffer)
  (let ((name (buffer-name buffer)))
    (or (null name)
        (catch 'ignore
          (dolist (ignore history-ignore-buffer-names)
            (when (string-match ignore name)
              (throw 'ignore t)))))))

(defun history-enable? ()
  "Menu command for enabling/disabling menu item."
  (unless (history-ignore-buffer?)
    (> (length (if history-window-local-history
                   (window-parameter nil 'history-stack)
                 history-stack)) 0)))

(defun history-add-menu-items ()
  "Add menu and tool-bar buttons."
  ;; Menu items.
  (define-key-after global-map [menu-bar edit history-group]
    (cons "History" (make-sparse-keymap))
    'separator-search)
  (let ((map (lookup-key global-map [menu-bar edit history-group])))
    (define-key-after map [window-local-history]
      '(menu-item "Window Local History" history-toggle-window-local-history
                  :button (:toggle . history-window-local-history)))
    (define-key-after map [setup-hook]
      '(menu-item "Configuration" history-configuration))
    (define-key-after map [history-separator-1]
      '(menu-item "--single-line"))
    (define-key-after map [add-history]
      '(menu-item "Add History" history-add-history
                  :enable (not (minibufferp))))
    (define-key-after map [previous-history]
      '(menu-item "Previous History" history-prev-history
                  :enable (history-enable?)))
    (define-key-after map [next-history]
      '(menu-item "Next History" history-next-history
                  :enable (history-enable?)))
    (define-key-after map [goto-history]
      '(menu-item "Goto History" history-goto-history
                  :enable (history-enable?)))
    (define-key-after map [show-history]
      '(menu-item "List History" history-show-history))
    (define-key-after map [discard-history]
      '(menu-item "Kill All History" history-kill-histories
                  :enable (history-enable?))))
  ;; Tool-bar buttons.
  (when tool-bar-mode
    (define-key-after tool-bar-map [add-history]
      '(menu-item "Add History" history-add-history
                  :image (find-image '((:type xpm :file "images/add-history.xpm")))
                  :enable (not (minibufferp))))
    (define-key-after tool-bar-map [previous-history]
      '(menu-item "Previous History" history-prev-history
                  :image (find-image '((:type xpm :file "images/prev-history.xpm")))
                  :enable (history-enable?)))
    (define-key-after tool-bar-map [next-history]
      '(menu-item "Next History" history-next-history
                  :image (find-image '((:type xpm :file "images/next-history.xpm")))
                  :enable (history-enable?)))
    (define-key-after tool-bar-map [goto-history]
      '(menu-item "Goto History" history-goto-history
                  :image (find-image '((:type xpm :file "images/goto-history.xpm")))
                  :enable (history-enable?)))))

(defun history-remove-menu-items ()
  "Remove menu and tool-bar buttons."
  ;; Menu items.
  (define-key global-map [menu-bar edit history-group] nil)
  ;; Tool-bar buttons.
  (when tool-bar-mode
    (define-key tool-bar-map [add-history] nil)
    (define-key tool-bar-map [previous-history] nil)
    (define-key tool-bar-map [next-history] nil)
    (define-key tool-bar-map [goto-history] nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun history-add-history (&optional save-thing?)
  "Add current position into the database, which is `global-mark-ring'. If 
SAVE-THING? is t, it will cache the symbol string at point (if any) and use it as 
a comparison in checking algorithm when navigating to it. If they are not matched, 
the history will be deleted immediately."
  (interactive '(t))
  (history-do
    (unless (history-ignore-buffer?)
      (history-remove-invalid-history t)
      (history-push-history (history-create-history save-thing? nil))
      (when (called-interactively-p 'interactive)
        (message (history-histories-string))))))

;;;###autoload
(defun history-show-history ()
  "Show histories in a pretty way."
  (interactive)
  (history-do
    (history-remove-invalid-history)
    (message (history-histories-string))))

;;;###autoload
(defun history-goto-history ()
  (interactive)
  (history-do
    (when history-stack
      (let* ((cached-history-index history-index)
             (history-window (selected-window))
             (str (history-histories-string))
             (index (1+ (string-match "\*" str)))
             (buffer (current-buffer))
             (pos (point)))
        (history-use-current-history)
        (if (string= (read-from-minibuffer "" (cons str index) history-map) "")
            (progn
              ;; Not to use history, revert buffer and point to original status.
              (setq history-index cached-history-index)
              ;; Switch to buffer.
              (set-window-buffer (history-window) buffer)
              (set-buffer buffer)
              ;; Update point.
              (goto-char pos))
          ;; Use history.
          (history-use-current-history))))))

;;;###autoload
(defun history-kill-histories ()
  "Discard all the histories."
  (interactive)
  (history-do
    (setq history-stack nil
          history-index 0)))

;;;###autoload
(defun history-prev-history ()
  "Navigate to previous history."
  (interactive)
  (history-do
    (when history-stack
      (history-remove-invalid-history)
      ;; If point is far away from current history, use current history.
      ;; If point is close from current history, use next/previous history.
      (let* ((history (nth history-index history-stack))
             (marker (plist-get history :marker))
             (buffer (marker-buffer marker))
             (pos (marker-position marker)))
        (if (and (eq buffer (current-buffer))
                 (history-same-line? (point) pos))
            (history-move-history 1)
          ;; Save current point as a temporary history.
          (history-remove-invalid-history t)
          (history-insert-history (history-create-history nil t))
          (history-move-history 1)))
      ;; Use history.
      (history-use-current-history))
    (message (history-histories-string))))

;;;###autoload
(defun history-next-history ()
  "Navigate to next history."
  (interactive)
  (history-do
    (when history-stack
      (history-remove-invalid-history)
      (let* ((history (nth history-index history-stack))
             (marker (plist-get history :marker))
             (buffer (marker-buffer marker))
             (pos (marker-position marker)))
        ;; If point is far away from current history, use current history.
        ;; If point is close from current history, use next/previous history.
        (when (and (eq buffer (current-buffer))
                   (history-same-line? (point) pos))
          (history-move-history -1)))
      ;; Use history.
      (history-use-current-history))
    (message (history-histories-string))))

;;;###autoload
(defun history-toggle-window-local-history ()
  "Switch between window-local history or global history mode.
See `history-window-local-history'."
  (interactive)
  (setq history-window-local-history (not history-window-local-history))
  (message "%s window-local history is %s!"
           (propertize "History:" 'face 'history-prompt)
           (if history-window-local-history
               "enabled" "disabled")))

;;;###autoload
(define-minor-mode history-mode
  "Add menus, toolbar buttons and more."
  :lighter " history"
  :global t
  (if history-mode
      (progn
        (history-add-menu-items)
        ;; Enable advice.
        (history-init-advices t))
    (history-remove-menu-items)
    ;; Disable advice.
    (history-init-advices nil)))

(provide 'history)
;;; history.el ends here
