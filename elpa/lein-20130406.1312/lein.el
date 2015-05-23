;;; lein.el --- Eshell interface to Leiningen

;; Copyright © 2013 Phil Hagelberg

;; Author: Phil Hagelberg
;; URL: https://github.com/technomancy/lein.el
;; Package-Version: 20130406.1312
;; Version: 0.1
;; Created: 2013-01-26
;; Keywords: tools, convenience
;; Package-Requires: ((nrepl "0.1.7"))

;; Additional contributions by:
;; - Gary W. Johnson (lambdatronic@gmail.com)

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This provides an eshell interface to the Leiningen project
;; automation tool for Clojure. (http://leiningen.org) It communicates
;; over nREPL (https://github.com/kingtim/nrepl.el) to avoid starting
;; a new process for every command. Note that tasks which call
;; eval-in-project will still start a project JVM; it's only
;; Leiningen's own startup time which is avoided.

;;; Usage

;; Currently you need to launch Leiningen once per Emacs instance with
;; M-x lein-launch. Then start eshell with M-x eshell and use
;; Leiningen as you would normally.

;;; License:

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

;;; Code:

(require 'cl)
(require 'nrepl)

;;==========================================================
;; Section 1: Formulating the Leiningen launch command
;; - Construct the java invocation
;; - Download the leiningen-*-standalone.jar if not present
;;==========================================================

(defcustom lein-home (expand-file-name "~/.lein") "Leiningen home directory.")

(defcustom lein-version "2.0.0"
  "Version of Leiningen to use. Note that changing this
  arbitrarily will not always work.")

(defcustom lein-java-command (or (getenv "LEIN_JAVA_CMD")
                                 (getenv "JAVA_CMD")
                                 "java")
  "Java executable to use to launch Leiningen.")

(defcustom lein-jvm-opts (or (getenv "LEIN_JVM_OPTS") "-Xms64m -Xmx512m")
  "Extra arguments to the java command to launch Leiningen.")

(defvar lein-download-url
  "https://leiningen.s3.amazonaws.com/downloads/leiningen-%s-standalone.jar")

(defun lein-self-install-callback (status lein-jar)
  (search-forward "\n\n")
  (write-region (point) (point-max) lein-jar)
  (message "Leiningen download complete. Please retry your command."))

(defun lein-self-install (lein-jar)
  (message "Leiningen not found. Downloading...")
  (sit-for 1) ;; Why the delay here?
  (url-retrieve
   (format lein-download-url lein-version)
   'lein-self-install-callback (list lein-jar)))

;; TODO: launch lein process with nohup so it can outlast Emacs
;; TODO: check for repl-port written to lein-home
(defun lein-launch-command ()
  (let ((lein-jar (format "%s/self-installs/leiningen-%s-standalone.jar"
                          lein-home lein-version)))
    (if (not (file-exists-p lein-jar))
        (progn (lein-self-install lein-jar) nil)
      (concat "LEIN_VERSION=" lein-version " "
              lein-java-command " -client -XX:+TieredCompilation"
              " -Xbootclasspath/a:" lein-jar " " lein-jvm-opts
              " -Dfile.encoding=UTF-8 -Dmaven.wagon.http.ssl.easy=false"
              " -Dleiningen.original.pwd=" default-directory
              " -classpath " lein-jar " clojure.main -m"
              " leiningen.core.main repl :headless"))))

;;==========================================================
;; Section 2: Formulating the Leiningen task command
;; - Locate and read in the current project.clj
;; - Construct the task invocation (in Clojure)
;; - Throw an error if a trampoline task is encountered
;;==========================================================

(defun lein-project-root (&optional file)
  (locate-dominating-file (or file default-directory) "project.clj"))

(defun lein-task-command (task &rest args)
  (if (string= "trampoline" task)
      (error "Cannot trampoline from lein.el")
    (let* ((project-clj (expand-file-name "project.clj" (lein-project-root)))
           (project-rdr (if (file-exists-p project-clj)
                            (format "(leiningen.core.project/read \"%s\")" project-clj)
                          "nil"))
           (string-args (or (mapcar (apply-partially 'format "\"%s\"") args) [])))
      (format "(binding [leiningen.core.main/*exit-process?* false]
                 (try (leiningen.core.main/apply-task \"%s\" %s '%s)
                      (catch Exception e
                        (if (:exit-code (ex-data e))
                          (when-not (= \"Suppressed exit\" (.getMessage e))
                            (println (.getMessage e)))
                          (clj-stacktrace.repl/pst e)))))"
              task project-rdr string-args))))

;;==========================================================
;; Section 3: Launching Leiningen in the background
;; - If no leiningen-*-standalone.jar, install it and exit
;; - Otherwise, run the lein-launch-command asynchronously
;;   and direct its output to lein-server-buffer
;; - Use lein-server-filter to filter its output
;; - Use lein-server-sentinel to handle signals sent to it
;; - Set process-coding-system to utf-8-unix
;;==========================================================

(defvar lein-server-buffer "*lein-server*")

(defvar lein-nrepl-connection-buffer "*lein-nrepl-connection*")

(defvar lein-words-of-inspiration
  '("Take this project automation tool, brother.  May it serve you well."))

(defun lein-server-filter (process output)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (save-excursion
        ;; Insert output and advance process-mark
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point))
        ;; Search buffer for nREPL server port
        (unless (process-get process :lein-nrepl-server-port)
          (goto-char (point-min))
          (when (re-search-forward "nREPL server started on port \\([0-9]+\\)\n" nil t)
            (process-put process :lein-nrepl-server-port (string-to-number (match-string 1)))
            ;; Connect to the local nREPL server
            (let* ((nrepl-words-of-inspiration lein-words-of-inspiration)
                   (original-nrepl-connection-list nrepl-connection-list)
                   (nrepl-process (nrepl-connect "localhost"
                                                 (process-get process
                                                              :lein-nrepl-server-port))))
              ;; Set some definitions local to the lein-server buffer:
              ;; - nrepl-connection-buffer = name of buffer associated with nrepl-client process
              ;; - lein-nrepl-connection-buffer = name of buffer associated with nrepl-client process
              (with-current-buffer (process-buffer process)
                (setq nrepl-connection-buffer
                      (buffer-name (process-buffer nrepl-process))
                      lein-nrepl-connection-buffer
                      (buffer-name (process-buffer nrepl-process))))
              ;; Set some definitions local to the nrepl-client buffer:
              ;; - nrepl-server-buffer = name of buffer associated with lein-server process (*lein-server*)
              ;; - lein-server-buffer = name of buffer associated with lein-server process (*lein-server*)
              (with-current-buffer (process-buffer nrepl-process)
                (setq nrepl-server-buffer
                      (buffer-name (process-buffer process))
                      lein-server-buffer
                      (buffer-name (process-buffer process))))
              ;; Wait for the *nrepl* buffer to pop up, and hide it immediately
              (let ((max-time-remaining 4000)) ;; 4 seconds
                (while (and (not (nrepl-current-nrepl-buffer))
                            (> max-time-remaining 0))
                  (sit-for 0 100)
                  (decf max-time-remaining 100)))
              (if (nrepl-current-nrepl-buffer)
                  (delete-windows-on (nrepl-current-nrepl-buffer)))
              ;; Restore original-nrepl-connect-list
              (when original-nrepl-connection-list
                (nrepl-make-repl-connection-default
                 (car original-nrepl-connection-list))))))))))

(defun lein-server-sentinel (process event)
  (when (buffer-live-p (process-buffer process))
    (let* ((buf (process-buffer process))
           (problem (with-current-buffer buf (buffer-string))))
      (when buf
        (kill-buffer buf))
      (cond ((string-match "^killed" event) nil)
            ((string-match "^hangup" event) (nrepl-quit))
            (t (error "Could not start Leiningen: %s" (or problem "")))))))

(defun lein-launch ()
  (interactive)
  (let ((command (lein-launch-command)))
    (when command
      (let* ((default-directory lein-home)
             (process (start-process-shell-command
                       "lein-server" lein-server-buffer
                       command)))
        (set-process-filter process 'lein-server-filter)
        (set-process-sentinel process 'lein-server-sentinel)
        (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
        (message "Starting Leiningen...")
        nil)))) ;; Suppress eshell output

;;==========================================================
;; Section 4: Eshell interface
;; - Launch Leiningen if not already running
;; - Otherwise, send the Leiningen task command to nREPL
;;   and register lein-handler as the callback function
;; - Then sit and wait until lein-handler is finished
;;==========================================================

(defun lein-launched? ()
  (and (get-buffer-process lein-nrepl-connection-buffer)
       (process-live-p (get-buffer-process lein-nrepl-connection-buffer))))

(defun lein-handler (task-complete? buffer response)
  (let ((out (cdr (assoc "out" response)))
        (err (cdr (assoc "err" response)))
        (status (cdr (assoc "status" response))))
    (when out
      (with-current-buffer buffer
        (eshell-output-filter nil out)))
    (when err
      (with-current-buffer buffer
        (eshell-output-filter nil err)))
    (when (member "eval-error" status)
      (nrepl-dbind-response response (value ns out err status id ex root-ex
                                            session)
        (funcall nrepl-err-handler buffer ex root-ex session)))
    (when (or (member "done" status)
              (member "eval-error" status))
      (setf (car task-complete?) t))))
      ;; (eshell-remove-process-entry entry)))) ; FIXME: entry appears to be unbound

(defun eshell/lein (&rest args)
  (if (lein-launched?)
      (let ((nrepl-connection-buffer lein-nrepl-connection-buffer)
            ;; woo promises for dummies
            (task-complete? (list nil)))
        (nrepl-send-string (apply 'lein-task-command (or args '("help")))
                           (apply-partially 'lein-handler
                                            task-complete?
                                            (current-buffer)))
        (while (not (car task-complete?))
          (sit-for eshell-process-wait-seconds
                   eshell-process-wait-milliseconds)))
    (lein-launch) ; TODO: callback to execute command instead of manual retry
    "Launching Leiningen. Wait till it's up, and try your command again."))

;; TODO: port from pcmpl-lein.el
(defun pcomplete/lein ())

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

(provide 'lein)
;;; lein.el ends here
