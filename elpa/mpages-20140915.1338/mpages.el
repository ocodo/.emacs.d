;;; mpages.el --- An Emacs buffer for quickly writing your Morning Pages

;; Copyright (C) 2014 Sean Levin

;; Author: Sean Levin
;; Created: 20 Aug 2014
;; Version: 20140915.1338
;; X-Original-Version: 20140913
;; URL: https://github.com/slevin/mpages

;;; Commentary:
;; A tool to quickly open a daily diary buffer.

;; Morning Pages are a concept named by Julia Cameron in her book
;; "The Artist's Way".  They are defined by the activity of waking up every
;; day and writing three pages of anything or everything that might be on
;; your mind.  The purpose is to express one's most urgent thoughts in a slightly
;; more coherent manner than they tend to occur in the realm of pure thought.
;; Unlike a "diary" they are meant to be a dumping ground, not a saving place.
;; Think it, write it, clear your mind, and move on.  Just doing that is enough.
;; In my experience of doing it daily I fret less about silly things and work
;; through my issues more productively.  I recommend it highly.

;; Opening up Emacs and running `M-x mpages' will open up an empty buffer
;; pointed at a txt file for today.  As you type it will keep track of the time
;; spent and the number of words typed.  The default, 750  words approximates
;; three written pages.  A competent typist, just letting his or her thoughts
;; flow should be able to finish between 15 and 20 minutes.  I find the effort
;; required is absolutely worth the time spent.  Enough so that I made this
;; package.  `M-x customize' has a few mpages related options under Text->mpages.


;;; Change Log:
;; 20140824 First Version
;; 20140913 Made fit for consumption

;;; Code:

(defgroup mpages nil
  "Mpages helps with writing a daily Morning Pages style diary."
  :prefix "mpages-"
  :group 'wp)

(defcustom mpages-word-threshold 750
  "This threshold is the number of words required before daily diary is complete."
  :type 'integer
  :group 'mpages)

(defcustom mpages-update-frequency 1
  "How many seconds before recounting your words.
Increasing this number may improve performance."
  :type 'integer
  :group 'mpages)

(defcustom mpages-content-directory nil
  "This is the directory to store your daily Morning Pages documents."
  :type 'directory
  :group 'mpages)

(defvar mpages-start-time)
(defvar mpages-count-timer)

(defun mpages-formatted-count (num threshold)
  "Colorize the NUM based on being above/below THRESHOLD."
  (let ((numstr (number-to-string num)))
    (if (< num threshold)
        (propertize numstr 'face '(:foreground "red"))
      (propertize numstr 'face '(:foreground "green")))))

(defun mpages-word-count-string (time-elapsed word-count)
  "Generate header line format string for TIME-ELAPSED and WORD-COUNT."
  (concat "Words: "
          (mpages-formatted-count word-count mpages-word-threshold)
          "   "
          "Time Elapsed: "
          (mpages-tfmt time-elapsed)))

(defun mpages-timer-tick ()
  "Run update on header with latest values."
  ;; this check makes sure it only runs update if the timer has been
  (if mpages-count-timer
      (let ((word-count (count-words 1 (length (buffer-string))))
            (time-elapsed (time-subtract (current-time) mpages-start-time)))
        (mpages-update-word-count time-elapsed word-count))))

(defun mpages-update-word-count (time-elapsed word-count)
  "Set the generated header line with TIME-ELAPSED and WORD-COUNT."
  (setq header-line-format (mpages-word-count-string time-elapsed word-count)))

(defun mpages-end-timer-stuff ()
  "Remove all runtime stuff related to this mode."
  (cancel-timer mpages-count-timer)
  (setq header-line-format nil)
  (makunbound 'mpages-count-timer)
  (makunbound 'mpages-start-time))

(defun mpages-tfmt (time)
  "Format the TIME for the header."
  (format-time-string "%M:%S" time))

(defun mpages-setup-time ()
  "Capture the start time of the mode."
  (setq mpages-start-time (current-time)))

(defun mpages-setup-timer ()
  "Start periodic timer to update the header with word count and time."
  (setq-local mpages-count-timer (run-at-time nil mpages-update-frequency 'mpages-timer-tick))
  (add-hook 'kill-buffer-hook 'mpages-end-timer-stuff nil t))

(defun mpages-open-today ()
  "Open a Morning Pages file for today."
  (find-file (concat (file-name-as-directory mpages-content-directory) (format-time-string "%Y%m%d") ".txt"))
  (auto-fill-mode)
  (set-fill-column 80))

;; open todays file
(defun mpages ()
  "Entry point to starting mpages."
  (interactive)
  (if (not mpages-content-directory)
      (customize-save-variable 'mpages-content-directory (file-name-as-directory (read-directory-name "Directory to put your Morning Pages: "))))
  (make-directory mpages-content-directory t) ;; ensure it exists
  (mpages-open-today)
  (mpages-setup-time)
  (mpages-setup-timer))

;; (defun testy ()
;;   "Throwaway function for testing."
;;   (interactive)
;;   (makunbound 'mpages-content-directory))

(provide 'mpages)
;;; mpages.el ends here
