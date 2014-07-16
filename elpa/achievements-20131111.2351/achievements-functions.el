;;; achievements-functions.el --- achievements for emacs usage

;; Author: Ivan Andrus <darthandrus@gmail.com>
;; Maintainer: Ivan Andrus <darthandrus@gmail.com>
;; Created: 2012-10-07
;; Keywords: games

;;; Code:

;; TODO: easy way to show a random unearned achievement, perhaps on an idle timer

(require 'cl)

(defconst achievements-file
  (expand-file-name ".achievements" user-emacs-directory)
  "File to store the achievements in.")

(defvar achievements-list nil
  "List of all possible achievements.")

(defvar achievements-post-command-list nil
  "List of achievements that need to be checked on `post-command-hook'.")

(defvar achievements-score 0
  "Score of all earned achievements.")

(defvar achievements-total 0
  "Highest possible score of all unlocked achievements.")

(defcustom achievements-debug nil
  "If non-nil, various debug messages will be printed regarding achievements activity."
  :type 'bool
  :group 'achievements)

;;{{{ Persistence & startup

(defun achievements-save-achievements ()
  "Saves achievements to a super secret file."
  (interactive)
  (let ((print-level nil)
        (print-length nil))
    (with-temp-file achievements-file
      (prin1 achievements-list (current-buffer)))))

(defun achievements-load-achievements ()
  "Load achievements from a super secret file.
This overwrites `achievements-list'."
  (interactive)
  (setq achievements-list
        (when (file-exists-p achievements-file)
          ;; Load sexp
          (let* ((l (condition-case nil
                        (with-temp-buffer
                          (insert-file-contents achievements-file)
                          (goto-char (point-min))
                          (read (current-buffer)))
                      ;; Catch empty file i.e., end of file during parsing
                      (error nil)))
                 (ll (and (listp l) l)))
            ;; Was it valid sexp?
            (and achievements-debug
                 (null ll)
                 (message "File %s does not contain valid data"
                          achievements-file))
            ll))))

;; Set up hooks and initialization
;;;###autoload
(defun achievements-init ()
  "Initialize achievements package."
  (when (null achievements-list)
    (achievements-load-achievements))
  (add-hook 'kill-emacs-hook #'achievements-save-achievements)
  ;; Load the basic achievements
  (require 'basic-achievements))

;;}}}
;;{{{ Defining achievements

(defstruct
    (emacs-achievement
     (:constructor nil)
     (:constructor make-achievement
                   (name description
                         ;; &optional (predicate t)
                         &key
                         ;; slots
                         points transient min-score predicate unlocks post-command
                         ;; convenience
                         package variable command
                         &aux (predicate
                               `(lambda ()
                                  ;; package
                                  (and
                                   ,@(when package
                                       (list (list 'featurep
                                                   (list 'quote package))))
                                   ,@(when variable
                                       (list (list 'achievements-variable-was-set
                                                   (list 'quote variable))))
                                   ,@(when command
                                       (list (list 'achievements-command-was-run
                                                   (list 'quote command))))
                                   ,@(when post-command
                                      (list nil))
                                   ;; TODO: allow functions here not just forms
                                   ,@(when predicate
                                       (list predicate))))))))

  (name nil :read-only t)
  description
  predicate ;; t if satisfied, nil if opted out, otherwise a function which should return non-nil on success
  transient ;; if non-nil then results won't be saved, but constantly re-evaluated.
  post-command ;; a predicate that needs to be run in post-command-hook
  (points 5)
  (min-score 0)
  unlocks
  )

(defmacro defachievement (name &rest body)
  `(add-to-list 'achievements-list
                ,(if (stringp (car-safe body))
                     `(make-achievement ,name ,@body)
                   `(make-achievement ,name nil ,@body)
                   )
                t
                ;; We compare by name only, since the predicate will often be different
                (lambda (a b)
                  (equal (emacs-achievement-name a)
                         (emacs-achievement-name b)))))

(defmacro defcommand-achievements (format-str body &rest arguments)
  (cons 'progn
        (loop for achiev in body
              collect (append
                       (list 'defachievement
                             (cadr achiev)
                             (format format-str
                                     (car achiev)
                                     (cddr achiev))
                             :command (list 'function (car achiev)))
                       arguments))))

(defmacro defvalue-achievements (var format-str body &rest arguments)
  (cons 'progn
        (loop for achiev in body
              collect (append
                       (list 'defachievement
                             (car achiev)
                             (format format-str
                                     (if (car-safe (cddr achiev))
                                         (car (cddr achiev))
                                       (cadr achiev))
                                     var)
                             :variable (list 'quote
                                             (list var (cadr achiev))))
                       arguments))))

;;}}}
;;{{{ Testing achievements

(defun achievements-variable-was-set (var)
  "If VAR is a cons, return non-nil if (car VAR) is equal to (cdr VAR).
If VAR is a symbol, return non-nil if VAR has been set in
customize or .emacs (not yet implemented)."
  (if (listp var)
      (equal (symbol-value (car var)) (cadr var))
    ;; it was set via customize etc.
    (or (and (symbol-value var)
             (string-match "\\(-hook\\|-function\\)\\'" (symbol-name var)))
        (and
         (get var 'custom-type) (get var 'standard-value)
         (not (equal (symbol-value var) (eval (car (get var 'standard-value)))))))))

(defun achievements-num-times-commands-were-run (command-list)
  "Return the number of times any one of the commands was run.
Right now this is checked it `command-frequency', but it is hoped
that in the future there will be other methods."
  (cond ((require 'keyfreq nil t)
         (let ((table (copy-hash-table keyfreq-table))
               (total 0))
           ;; Merge with the values in .emacs.keyfreq file
           (keyfreq-table-load table)
           (maphash
            (lambda (k v)
              (when (memq (cdr k) command-list)
                (setq total (+ total v))))
            table)
           total))
        ((require 'command-frequency nil t)
         (let ((command-freq (cdr (command-frequency-list)))
               (total 0))
           (loop for com in command-freq
                 if (member (car com) command-list)
                 do (setq total (+ total (cdr com))))
           total))
        (t (let ((total 0))
             (mapc
              (lambda (x)
                (when (memq (car x) command-list)
                  (setq total (+ total 1))))
              command-history)
             total))))

(defun achievements-command-was-run (command)
  "Return non-nil if COMMAND has been run.
It can be a single command form or list of command forms.
If it's a list of forms, then all must be run.
Each form has one of the forms
 COMMAND -- must be run once
 (CMD1 CMD2 ...) -- any can be run
 (COMMAND . COUNT) -- must be run COUNT times
 ((CMD1 CMD2 ...) . COUNT) -- must be run COUNT times
symbol for a command which must be."
  (let (command-list)
    (cond
     ;; A symbol
     ((symbolp command)
      (>= (achievements-num-times-commands-were-run (list command))
          1))
     ;; cdr is a number
     ((numberp (cdr command))
      (>= (achievements-num-times-commands-were-run
           (if (listp (car command)) (car command) (list (car command))))
          (cdr command)))
     ;; A list of commands that are AND-ed
     ((or (symbolp (car-safe command))
          (numberp (cdr-safe (car-safe command))))
      (every 'achievements-command-was-run command))
     ;; Otherwise it's a list of commands, any of which could be run
     (t
      (>= (achievements-num-times-commands-were-run
           (car command))
          1)))))

;;}}}
;;{{{ Display

(defun achievements-earned-message (achievement)
  "Display the message when an achievement is earned."
  (message "ACHIEVEMENT UNLOCKED: You've earned the `%s' achievement!"
           (emacs-achievement-name achievement))
  (with-current-buffer (get-buffer-create "*achievements-log*")
    (goto-char (point-min))
    (when (> (buffer-size) 0)
      (insert "\n")
      (goto-char (point-min)))
    (insert (format "You've earned the `%s' achievement! [%s]"
                    (emacs-achievement-name achievement)
                    (emacs-achievement-description achievement)))))

(defun achievements-update-score ()
  "Recalculate whether each achievement has been earned."
  (message "Calculating achievements...")
  (let ((score 0)
        (total 0))
    (dolist (achievement achievements-list)
      (let ((points (emacs-achievement-points achievement)))
        (incf total points)
        (when (achievements-earned-p achievement)
          (incf score points)
          (when (emacs-achievement-unlocks achievement)
            (require (emacs-achievement-unlocks achievement) nil t))
          (unless (emacs-achievement-transient achievement)
            (when (and achievements-display-when-earned
                       (not (equal (emacs-achievement-predicate achievement) t)))
              (achievements-earned-message achievement))
            (setf (emacs-achievement-predicate achievement) t)))))
    ;; Save the updated list of achievements
    (achievements-save-achievements)
    (setq achievements-total total)
    (setq achievements-score score))
  (message "Calculating achievements... done"))

(defun achievements-earned-p (achievement)
  "Returns non-nil if the achievement is earned."
  (let ((pred (emacs-achievement-predicate achievement)))
    (or (eq pred t)
        (and (functionp pred)
             (condition-case err
                 (funcall pred)
               ('error
                (message "Error while checking if you have earned the %s achievement"
                         (emacs-achievement-name achievement))))))))

(defun achievements-get-achievements-by-name (name)
  "Return the achievement identified by NAME."
  (let ((l achievements-list)
        ret)
    (while l
      (when (equal name (emacs-achievement-name (car l)))
        (setq ret (car l))
        (setq l nil))
      (setq l (cdr l)))
    ret))

;;}}}
;;{{{ Achievements List

(defun achievements-tabulated-list-entries ()
  "Turn `achievements-list' into a list for `tabulated-list-entries'."
  (loop for achievement in achievements-list
        for pred = (emacs-achievement-predicate achievement)
        if (and pred ;; Not disabled
                (>= achievements-score
                    (emacs-achievement-min-score achievement)))
        collect (list (emacs-achievement-name achievement)
                      (vector
                       (cond ((achievements-earned-p achievement) "✓")
                             ((eq pred nil) "✗")
                             (t ""))
                       (format "%s" (emacs-achievement-points achievement))
                       (emacs-achievement-name achievement)
                       (if (achievements-earned-p achievement)
                           (emacs-achievement-description achievement)
                         "")))))

(defun achievements-disable ()
  "Disable the current achievement.
This expects to be called from `achievements-list-mode'."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (achievement (achievements-get-achievements-by-name id)))
    (when (and achievement
               (y-or-n-p "Do you really want to disable this achievement? "))
      (setf (emacs-achievement-predicate achievement) nil)
      (revert-buffer))))

(define-derived-mode achievements-list-mode tabulated-list-mode
  "Achievements"
  "Mode for display the list of achievements."
  (setq tabulated-list-format
        '[("E"            3 t . (:pad-right 0))
          ("Pts"          3 t . (:pad-right 1 :right-align t))
          ("Name"        30 t . (:pad-right 1))
          ("Description" 20 t . (:pad-right 1))])
  (add-hook 'tabulated-list-revert-hook #'achievements-update-score)
  (setq tabulated-list-entries #'achievements-tabulated-list-entries)
  (setq tabulated-list-padding 1)
  (set (make-local-variable 'show-trailing-whitespace) nil)
  ;; Maybe set `tabulated-list-printer'
  ;; (setq tabulated-list-sort-key '("Name"))
  (tabulated-list-init-header))

(defvar achievements-list-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "Achievements")))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "d" 'achievements-disable)
    ;; (define-key map "t" 'achievements-toggle-show-disabled)
    (define-key map [menu-bar achievements-menu] (cons "Achievements" menu-map))
    (define-key menu-map [mq]
      '(menu-item "Quit" quit-window
                  :help "Quit Viewing Achievements"))
    (define-key menu-map [s1] '("--"))
    (define-key menu-map [mn]
      '(menu-item "Next" next-line
                  :help "Next Line"))
    (define-key menu-map [mp]
      '(menu-item "Previous" previous-line
                  :help "Previous Line"))
    (define-key menu-map [s2] '("--"))
    (define-key menu-map [md]
      '(menu-item "Disable" achievements-disable
                  :help "Disable an achievement. It won't show up in this list, and you can never earn it"))
    (define-key menu-map [s3] '("--"))
    (define-key menu-map [mg]
      '(menu-item "Refresh list" revert-buffer
                  :help "Recalculate this list"))
    map)
  "Local keymap for `achievements-list-mode' buffers.")

;;;###autoload
(defun achievements-list-achievements ()
  "Display all achievements including whether they have been achieved."
  (interactive)
  (pop-to-buffer "*Achievements*")
  (achievements-list-mode)
  (achievements-update-score)
  (tabulated-list-print t))

;;}}}
;;{{{ Achievements Mode

(defvar achievements-timer nil
  "Holds the idle timer.")

(defcustom achievements-display-when-earned t
  "If non-nil, various debug messages will be printed regarding achievements activity."
  :type 'bool
  :group 'achievements)

(defcustom achievements-idle-time 10
  "Number of seconds for Emacs to be idle before checking if achievements have been earned."
  :type 'numberp
  :group 'achievements)

(defun achievements-setup-post-command-hook ()
  "Add the appropriate achievements for the post-command-hook."
  (setq achievements-post-command-list nil)
  (dolist (achievement achievements-list)
    (when (and (emacs-achievement-post-command achievement)
               (not (eq t (emacs-achievement-predicate achievement))))
      (add-to-list 'achievements-post-command-list achievement))))

(defun achievements-post-command-function ()
  "Check achievements on `post-command-hook'."
  (flet ((remove (v) (setq achievements-post-command-list
                           (delete v achievements-post-command-list))))
    (dolist (achievement achievements-post-command-list)
      (let ((pred (emacs-achievement-post-command achievement)))
        (if (functionp pred)
            (when (funcall pred)
              (setf (emacs-achievement-predicate achievement) t)
              (achievements-earned-message achievement)
              (remove achievement))
          (remove achievement))))))

;;;###autoload
(define-minor-mode achievements-mode
  "Turns on automatic earning of achievements when idle."
  ;; The lighter is a trophy
  nil " 🏆" nil
  (if achievements-mode
      (progn
        (unless achievements-timer
          (setq achievements-timer
                (run-with-idle-timer achievements-idle-time
                                     t #'achievements-update-score)))
        (achievements-setup-post-command-hook)
        (add-hook 'post-command-hook #'achievements-post-command-function))
    (setq achievements-timer (cancel-timer achievements-timer))
    (remove-hook 'post-command-hook #'achievements-post-command-function)))

;;}}}

(provide 'achievements-functions)

;;; achievements-functions.el ends here
