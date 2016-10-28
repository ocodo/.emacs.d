;; use-rspec -- Initialize rspec
;;; Commentary:
;;  Initialize rspec mode
;;
;;; Code:

(require 'rvm)
(require 'rspec-mode)

(rspec-install-snippets)
(setq rspec-use-rvm t)

;; Use C-x C-q to switch to interactive mode, for example with pry/debugger
;; (add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; Run in iTerm with `rspec-verify-single-iterm'
(defcustom iterm-command
  #'iterm2-nightly-command
  "Function for running command in iTerm.")

(defun rspec-verify-single-iterm ()
  "Run the specified example at point."
  (interactive)
  (rspec-run-single-file-iterm
   (cons
    (rspec-spec-file-for (buffer-file-name))
    (save-restriction
      (widen)
      (number-to-string (line-number-at-pos))))
   (rspec-core-options)))

(defun rspec-run-single-file-iterm (spec-file &rest opts)
  "Run spec on SPEC-FILE with the specified options OPTS."
  (rspec-compile-iterm (rspec-runner-target spec-file) opts))

(defun rspec-fail-remove-trailing-comment (&optional arg)
  (interactive "p")
  (kmacro-exec-ring-item
   '([?\C-s ?# left left ?\C-k ?\C-a])
   arg))

(defun rspec-compile-iterm (target &optional opts)
  "Exectute TARGET with the specified options OPTS in iTerm."
  (setq rspec-last-directory default-directory
        rspec-last-arguments (list target opts))

  (if rspec-use-rvm
      (rvm-activate-corresponding-ruby))

  (let ((default-directory (or (rspec-project-root) default-directory)))
    (apply iterm-command
           (list (format "cd %s; %s"
                         default-directory
                         (mapconcat 'identity
                                    `(,(rspec-runner)
                                      ,(rspec-runner-options opts)
                                      ,target) " "))))))

(defun iterm2-nightly-command (command)
  (shell-command
   (format
    "%s%s %S"
    user-emacs-directory
    "bin/osx_iterm2_nightly"
    command)))

(provide 'use-rspec)
;;; use-rspec.el ends here
