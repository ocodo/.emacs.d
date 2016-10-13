;;; make-yasnippet-from-region -- make a yasnippet from the current region
;;; Author: Jason Milkins <ocodo@pivotal.io>
;;; Commentary:
;;; Code:
(require 'yasnippet)

(defun make-yas-from-region (begin end)
  "Make a yasnippet from the current region BEGIN END.

You should use standard snippet formatting in place, e.g. $1,
${1:default value} and so on.  See the yasnippet docs for more info.

You'll be prompted for a name, trigger key and when `prefix-arg' is
specified, a snippet group."
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
               (snippet (buffer-substring begin end))
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

(provide 'make-yasnippet-from-region)

;;; make-yasnippet-from-region.el ends here
