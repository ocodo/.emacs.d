;;; find-file-in-git-repo.el --- Utility to find files in a git repo

;; Copyright 2011 atom smith

;; Author: atom smith
;; URL: http://github.com/re5et/find-file-in-git-repo
;; Version: 0.1.2

;;; Commentary:

;; Using default-directory searches upward for a .git repo directory,
;; then, feeds files into ido-completing-read using git ls-files.

(defun find-file-in-git-repo ()
  (interactive)
  (let* ((repo (find-git-repo default-directory))
         (files (shell-command-to-string (format "cd %s && git ls-files" repo))))
    (find-file
     (concat repo
             (ido-completing-read
              "find in git repo: "
              (remove-if (lambda (x) (string= "" x))
              (split-string files "\n")))))))

(defun find-git-repo (dir)
  (if (string= "/" dir)
      (message "not in a git repo.")
    (if (file-exists-p (expand-file-name ".git/" dir))
        dir
      (find-git-repo (expand-file-name "../" dir)))))

;;; find-file-in-git-repo.el ends here

(provide 'find-file-in-git-repo)
