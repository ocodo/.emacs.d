;;; ido-yes-or-no.el --- Use Ido to answer yes-or-no questions

;; Copyright 2011 Ryan C. Thompson

;; Author: Ryan C. Thompson
;; URL: https://github.com/DarwinAwardWinner/ido-yes-or-no
;; Package-Version: 20140625.2106
;; Version: 1.2
;; Package-Requires: ()

(require 'ido)

;;;###autoload
(define-minor-mode ido-yes-or-no-mode
  "Use ido for `yes-or-no-p'."
  nil
  :global t
  :group 'ido)

(defun ido-yes-or-no-p (prompt)
  "Ask user a yes-or-no question using ido."
  (let* ((yes-or-no-prompt (concat prompt " "))
         (choices '("yes" "no"))
         (answer (ido-completing-read yes-or-no-prompt choices nil 'require-match)))
    (string= answer "yes")))

(defadvice yes-or-no-p (around use-ido activate)
  (if ido-yes-or-no-mode
      (setq ad-return-value (ido-yes-or-no-p prompt))
    ad-do-it))

(provide 'ido-yes-or-no)
;;; ido-yes-or-no.el ends here
