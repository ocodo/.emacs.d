;;; eshell-git-prompt-ocodo --- is the ocodo eshell-git-prompt
;;; Commentary:
;;; Code:

;; ocodo

(require 'em-dirs)
(require 'eshell-git-prompt)

(defconst eshell-git-prompt-ocodo-regexp "^[^$\n]*\\\$ ")

(defun eshell-git-prompt-ocodo ()
  "An eshell git prompt by ocodo."
  (let ((segment-separator "\xe0b0")
        (plusminus         "\x00b1")
        (branch            "\xe0a0")
        (detached          "\x27a6")
        (cross             "\x2718")
        dir git git-bg)
    (setq dir
          (with-face (concat
                      " "
                      (unless (eshell-git-prompt-exit-success-p)
                        (concat cross " "))
                      (abbreviate-file-name (eshell/pwd))
                      " ")
            :background "#2E5C91"))
    (setq git
          (when (eshell-git-prompt--git-root-dir)
            (setq git-bg
                  (if (eshell-git-prompt--collect-status)
                      "#660000" "#006600"))
            (setq eshell-git-prompt-branch-name (eshell-git-prompt--branch-name))
            (with-face
                (concat " "
                        (-if-let (branch-name eshell-git-prompt-branch-name)
                            (concat branch " " branch-name)
                          (concat detached " "(eshell-git-prompt--commit-short-sha)))
                        " ")
              :background git-bg)))
    (concat
     (if git
         (concat dir
                 (with-face segment-separator
                   :background git-bg
                   :foreground "#2E5C91")
                 git
                 (with-face segment-separator
                   :foreground git-bg))
       (concat dir (with-face segment-separator
                     :foreground "#2E5C91")))
     (propertize "$" 'invisible t) " ")))

(add-to-list 'eshell-git-prompt-themes
             '(ocodo
               eshell-git-prompt-ocodo
               eshell-git-prompt-ocodo-regexp))

(provide 'eshell-git-prompt-ocodo)
;;; eshell-git-prompt-ocodo ends here
