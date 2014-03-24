(defun rails-generate-migration (args)
   "Runs 'script/generate migration ARGS' and opens the new migration in a buffer."
   (interactive
    (list (read-string "generate migration: ")))
   (let
       ((command (format "ruby %sscript/generate migration %s" (rinari-root) args))
        (result nil))
     (message "calling: %s" command)
     (setq result (shell-command-to-string command))
     (if (string-match "^ *create *\\(db/migrate/.*\\.rb\\)$" result)
         (find-file (format "%s%s" (rinari-root) (substring result (match-beginning 1) (match-end 1))))
       (progn
         (message "failure result: %s" result)
         (get-buffer "*Messages*")
         (message "command failed: %s" command)))))


(eval-after-load 'ruby-mode
  '(define-key ruby-mode-map (kbd "C-c g m") 'rails-generate-migration))
