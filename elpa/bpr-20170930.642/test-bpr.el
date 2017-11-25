;;; test-bpr.el --- Tests for Background Process Runner

;;; Commentary:
;; WIP
;; It isn't good tests:
;; All used emacs functions are mocked, and each test checks that
;; correct emacs function(s) have been called.
;; If some internal package logic changes (without breaking the public api behavior)
;; there is a chance that tests will fail.

;; How to run:
;; cask exec buttercup -L .

;;; Code:
(require 'bpr)
(require 'buttercup)

(defmacro with-fake-buffer (buffer-or-name &rest body)
  `(save-current-buffer
     (set-buffer ,buffer-or-name)
     (fset 'old-get-buffer-create 'get-buffer-create)
     (fset 'get-buffer-create (lambda (name) fake-buffer))
     ,@body
     (fset 'get-buffer-create 'old-get-buffer-create)))

(describe "bpr-backage"
  (let* (default-directory fake-buffer fake-process fake-plist)

    (before-each
      (setq default-directory "/test/")
      (setq fake-buffer (get-buffer-create "test-buffer"))
      (setq fake-process "I am fake process")

      (fset 'message (lambda (str &rest args) nil))
      (fset 'delete-window (lambda (window) nil))
      (fset 'get-buffer-window (lambda (buffer) nil))
      (fset 'erase-buffer (lambda () nil))
      (fset 'shell-mode (lambda () nil))
      (fset 'set-window-buffer (lambda (window buffer) nil))
      (fset 'split-window-vertically (lambda () nil))
      (fset 'process-live-p (lambda (process) nil))
      (fset 'get-process (lambda (name) nil))
      (fset 'process-buffer (lambda (process) fake-buffer))
      (fset 'process-get (lambda (process prop) (plist-get fake-plist prop)))
      (fset 'process-exit-status (lambda (process) nil))
      (fset 'set-process-plist (lambda (process plist) (setq fake-plist plist)))
      (fset 'set-process-sentinel (lambda (process func) nil))
      (fset 'start-process-shell-command (lambda (name buffer command) fake-process))

      (spy-on 'delete-window :and-call-through)
      (spy-on 'erase-buffer :and-call-through)
      (spy-on 'split-window-vertically :and-call-through)
      (spy-on 'set-window-buffer :and-call-through)
      (spy-on 'get-buffer-create :and-call-through)
      (spy-on 'start-process-shell-command :and-call-through))

    (describe "defaults"
      (it "should have correct values"
        (expect bpr-close-after-success :to-be nil)
        (expect bpr-open-after-error :to-be t)
        (expect bpr-window-creator :to-be #'split-window-vertically)
        (expect bpr-process-mode :to-be #'shell-mode)
        (expect bpr-process-directory :to-be nil)
        (expect bpr-use-projectile :to-be t)
        (expect bpr-erase-process-buffer :to-be t)
        (expect bpr-scroll-direction :to-be 1)
        (expect bpr-show-progress :to-be t)
        (expect bpr-poll-timout :to-equal 0.2)
        (expect bpr-open-after-error :to-be t)))

    (describe "bpr-spawn"
      (it "should set correct name for process buffer"
        (bpr-spawn "ls")
        (expect 'get-buffer-create
          :to-have-been-called-with
          (concat "*ls (" default-directory ")*")))

      ;; All directory checks are made by checking process buffer name...
      ;; This is because I haven't found direct way to do it.
      (it "should set correct directory for process with default options"
        (bpr-spawn "ls")
        (expect 'get-buffer-create
          :to-have-been-called-with
          "*ls (/test/)*"))

      (it "should set correct directory for process using projectile"
        (fset 'projectile-project-root (lambda () "/projects/root/"))
        (bpr-spawn "ls")
        (expect 'get-buffer-create
          :to-have-been-called-with
          "*ls (/projects/root/)*")
        (fmakunbound 'projectile-project-root))


      (it "should not use projectile when bpr-use-projectile is nil"
        (fset 'projectile-project-root (lambda () "/projects/root/"))
        (let* ((default-directory ".")
                (bpr-use-projectile nil))
          (bpr-spawn "ls")
          (expect 'get-buffer-create
            :to-have-been-called-with
            "*ls (.)*"))
        (fmakunbound 'projectile-project-root))

      (it "should use bpr-process-directory if it's not nil"
        (fset 'projectile-project-root (lambda () "/projects/root/"))
        (let* ((default-directory ".")
                (bpr-process-directory "should/use/this"))
          (bpr-spawn "ls")
          (expect 'get-buffer-create
            :to-have-been-called-with
            "*ls (should/use/this)*"))
        (fmakunbound 'projectile-project-root))

      (it "should spawn process with correct name, buffer and command"
        (with-fake-buffer fake-buffer
          (let* ((default-directory "dev/null"))
            (bpr-spawn "ls")
            (expect 'start-process-shell-command
              :to-have-been-called-with
              "ls (dev/null)" fake-buffer "ls"))))

      (it "shoud not start process if it already exists"
        (fset 'get-process (lambda (name) (when (equal name "ls (/test/)") fake-process)))
        (bpr-spawn "ls")
        (expect 'start-process-shell-command :not :to-have-been-called))

      (it "should erase process buffer"
        (bpr-spawn "ls -la")
        (expect 'erase-buffer :to-have-been-called))

      (it "should not erase process buffer if it's read only"
        (with-fake-buffer fake-buffer
          (let* ((buffer-read-only t))
            (bpr-spawn "ls -la")
            (expect 'erase-buffer :not :to-have-been-called))))

      (it "should not erase process buffer if bpr-erase-process-buffer is nil"
        (let* ((bpr-erase-process-buffer nil))
          (bpr-spawn "ls -la")
          (expect 'erase-buffer :not :to-have-been-called)))

      (it "should call bpr-process-mode on process buffer"
        (fset 'test-mode-func (lambda ()))
        (let* ((bpr-process-mode 'test-mode-func))
          (spy-on 'test-mode-func)
          (bpr-spawn "ls -la")
          (expect 'test-mode-func :to-have-been-called)))

      (it "should display process buffer in case of error"
        (let* (test-sentiel-handler)
          (fset 'set-process-sentinel (lambda (process handler)
                                        (when (eq process fake-process)
                                          (setq test-sentiel-handler handler))))
          (fset 'process-exit-status (lambda (process) (when (eq process fake-process) 3)))
          (bpr-spawn "make build")
          (funcall test-sentiel-handler fake-process)
          (expect 'split-window-vertically :to-have-been-called)
          (expect 'set-window-buffer :to-have-been-called)))

      (it "should not display process buffer in case of error if bpr-open-after-error is nil"
        (let* ((test-sentiel-handler nil)
                (bpr-open-after-error nil))
          (fset 'set-process-sentinel (lambda (process handler)
                                        (when (eq process fake-process)
                                          (setq test-sentiel-handler handler))))
          (fset 'process-exit-status (lambda (process) (when (eq process fake-process) 3)))
          (bpr-spawn "make build")
          (funcall test-sentiel-handler fake-process)
          (expect 'split-window-vertically :not :to-have-been-called)
          (expect 'set-window-buffer :not :to-have-been-called)))

      (it "should not close error buffer after success with default options"
        (let* ((test-sentiel-handler nil))
          (fset 'set-process-sentinel (lambda (process handler)
                                        (when (eq process fake-process)
                                          (setq test-sentiel-handler handler))))
          (fset 'process-exit-status (lambda (process) (when (eq process fake-process) 3)))
          (bpr-spawn "make build")
          (funcall test-sentiel-handler fake-process)
          (expect 'split-window-vertically :to-have-been-called)
          (expect 'set-window-buffer :to-have-been-called)
          (fset 'process-exit-status (lambda (process) (when (eq process fake-process) 0)))
          (fset 'get-buffer-window (lambda (buffer) (when (eq buffer fake-buffer) "I am window")))
          (bpr-spawn "make build")
          (funcall test-sentiel-handler fake-process)
          (expect 'delete-window :not :to-have-been-called)))

      (it "should close error buffer after success when bpr-close-after-success is t"
        (let* ((bpr-close-after-success t)
                (test-sentiel-handler nil))
          (fset 'set-process-sentinel (lambda (process handler)
                                        (when (eq process fake-process)
                                          (setq test-sentiel-handler handler))))
          (fset 'process-exit-status (lambda (process) (when (eq process fake-process) 3)))
          (bpr-spawn "make build")
          (funcall test-sentiel-handler fake-process)
          (expect 'split-window-vertically :to-have-been-called)
          (expect 'set-window-buffer :to-have-been-called)
          (fset 'process-exit-status (lambda (process) (when (eq process fake-process) 0)))
          (fset 'get-buffer-window (lambda (buffer) (when (eq buffer fake-buffer) "I am window")))
          (bpr-spawn "make build")
          (funcall test-sentiel-handler fake-process)
          (expect 'delete-window :to-have-been-called)))

      (it "should colorize process buffer"
        (let* ((bpr-colorize-output t)
                (test-sentiel-handler nil))
          (fset 'set-process-sentinel (lambda (process handler)
                                        (when (eq process fake-process)
                                          (setq test-sentiel-handler handler))))
          (fset 'process-exit-status (lambda (process) (when (eq process fake-process) 3)))
          (fset 'ansi-color-apply-on-region (lambda (begin end) nil))
          (spy-on 'ansi-color-apply-on-region :and-call-through)
          (bpr-spawn "make build")
          (funcall test-sentiel-handler fake-process)
          (expect 'ansi-color-apply-on-region :to-have-been-called)))

      (it "should not colorize process buffer if it's read only"
        (let* ((bpr-colorize-output t)
                (test-sentiel-handler nil))
          (fset 'set-process-sentinel (lambda (process handler)
                                        (when (eq process fake-process)
                                          (setq test-sentiel-handler handler))))
          (fset 'process-exit-status (lambda (process) (when (eq process fake-process) 3)))
          (fset 'ansi-color-apply-on-region (lambda (begin end) nil))
          (spy-on 'ansi-color-apply-on-region :and-call-through)
          (bpr-spawn "make build")
          (with-current-buffer fake-buffer
            (let* ((buffer-read-only t))
              (funcall test-sentiel-handler fake-process)))
          (expect 'ansi-color-apply-on-region :not :to-have-been-called)))

      (it "should call bpr-on-completion function in case of error and success"
        (let* ((completion-flag nil)
                (bpr-on-completion (lambda (p) (setq completion-flag t)))
                (test-sentiel-handler nil))
          (fset 'set-process-sentinel (lambda (process handler)
                                        (when (eq process fake-process)
                                          (setq test-sentiel-handler handler))))
          (fset 'process-exit-status (lambda (process) (when (eq process fake-process) 0)))
          (bpr-spawn "make build")
          (funcall test-sentiel-handler fake-process)
          (expect completion-flag :to-be t)

          (setq completion-flag nil)
          (setq test-sentiel-handler nil)
          (fset 'process-exit-status (lambda (process) (when (eq process fake-process) 3)))
          (bpr-spawn "make build")
          (funcall test-sentiel-handler fake-process)
          (expect completion-flag :to-be t)))

      (it "should call bpr-on-success function in case of success and not call in case of error"
        (let* ((success-flag nil)
                (bpr-on-success (lambda (p) (setq success-flag t)))
                (test-sentiel-handler nil))
          (fset 'set-process-sentinel (lambda (process handler)
                                        (when (eq process fake-process)
                                          (setq test-sentiel-handler handler))))
          (fset 'process-exit-status (lambda (process) (when (eq process fake-process) 0)))
          (bpr-spawn "make build")
          (funcall test-sentiel-handler fake-process)
          (expect success-flag :to-be t)

          (setq success-flag nil)
          (setq test-sentiel-handler nil)
          (fset 'process-exit-status (lambda (process) (when (eq process fake-process) 3)))
          (bpr-spawn "make build")
          (funcall test-sentiel-handler fake-process)
          (expect success-flag :to-be nil)))

      (it "should not call bpr-on-error function in case of success and call in case of error"
        (let* ((error-flag nil)
                (bpr-on-error (lambda (p) (setq error-flag t)))
                (test-sentiel-handler nil))
          (fset 'set-process-sentinel (lambda (process handler)
                                        (when (eq process fake-process)
                                          (setq test-sentiel-handler handler))))
          (fset 'process-exit-status (lambda (process) (when (eq process fake-process) 0)))
          (bpr-spawn "make build")
          (funcall test-sentiel-handler fake-process)
          (expect error-flag :to-be nil)

          (setq test-sentiel-handler nil)
          (fset 'process-exit-status (lambda (process) (when (eq process fake-process) 3)))
          (bpr-spawn "make build")
          (funcall test-sentiel-handler fake-process)
          (expect error-flag :to-be t)))))


  (describe "bpr-open-last-buffer"
    (it "should open last used buffer"
      (with-fake-buffer fake-buffer
        (bpr-spawn "ls")
        (bpr-open-last-buffer)
        (expect 'split-window-vertically :to-have-been-called)
        (expect 'set-window-buffer :to-have-been-called-with nil fake-buffer)))

    (it "should not open last used buffer if it isn't exist"
      (let* ((bpr-last-buffer nil))
        (bpr-open-last-buffer)
        (expect 'split-window-vertically :not :to-have-been-called)
        (expect 'set-window-buffer :not :to-have-been-called)))

    (it "should not open last used buffer if it have been killed"
      (with-fake-buffer fake-buffer
        (bpr-spawn "ls")
        (kill-buffer fake-buffer)
        (bpr-open-last-buffer)
        (expect 'split-window-vertically :not :to-have-been-called)
        (expect 'set-window-buffer :not :to-have-been-called)))))

;;; test-bpr.el ends here
