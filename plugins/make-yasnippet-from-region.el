;;; make-yasnippet-from-region -- make a yasnippet from the current region
;;; Author: Jason Milkins <ocodo@pivotal.io>
;;; Commentary:
;;; Code:
(require 'yasnippet)

(defun make-yas-from-region (b e)
  "Make a parameterless yasnippet from the current region B E."
  (interactive "r")
  (if (region-active-p)
      (progn
        ;; TODO make a new buffer with yas headers
        ;; ask for a name
        (let* ((name (read-from-minibuffer "Name: "))
               (group (if current-prefix-arg
                          (format "\n# group: %s\n" (read-from-minibuffer "Group: "))
                        ""))
               (key (read-from-minibuffer "Key: "))
               (filename (format "%ssnippets/%s/%s" user-emacs-directory major-mode name))
               (snippet (buffer-substring b e))
               (template (format "# -*- mode: snippet -*-
# name: %s%s
# key: %s
# --
%s
"
                                 name
                                 group
                                 key
                                 snippet)))
          (with-temp-buffer
            (insert template)
            (write-file filename)))
        (yas-reload-all))
        (error "An active region is needed to make a snippet")))

;;; make-yasnippet-from-region.el ends here
