;;; nyan-prompt.el --- Nyan Cat on the eshell prompt.

;; Author: Javier "PuercoPop" Olaechea <pirata@gmail.com>
;; URL: http://github.com/PuercoPop/nyan-prompt
;; Version: 0.2.0
;; Keywords: nyan, cat, lulz, eshell, rainbow
;; Dependencies ((rx 0))

;;; Commentary:

;; Usage: (add-hook 'eshell-load-hook 'nyan-prompt-enable)

;; Inspired by from Jacek "TeMPOraL" Zlydach nyan-mode, to make Porter happy.


;; Copying is an act of love, please copy.  ♡
;; The xpm taken awesome nyan-mode

;;; Code:

(require 'rx)

(defconst nyan-prompt-dir (file-name-directory
                             (or load-file-name buffer-file-name)))

(defconst nyan-prompt-nyan-cat-image
  (create-image (concat nyan-prompt-dir "img/nyan.xpm")
                'xpm nil :ascent 'center))

(defconst nyan-prompt-nyan-cat-emoticon "~=[,,_,,]:3"
  "ASCII art representing the nyan-cat.")

(defconst nyan-prompt-nyan-cat-string
  (propertize nyan-prompt-nyan-cat-emoticon
              'display nyan-prompt-nyan-cat-image))

;;;###autoload
(defun nyan-prompt-enable ()
  (setq eshell-prompt-function
        (lambda nil
          (concat
           (abbreviate-file-name
            (eshell/pwd))
           (if (= (user-uid) 0)
               " # "
               (concat " " nyan-prompt-nyan-cat-string " "))))

        ;; Match a string of characters (representing the path) then a space,
        ;; the nyan ASCII art and then a space.
        eshell-prompt-regexp
        (rx (and bol
                 (1+ anything)
                 " "
                 "~=[,,_,,]:3"
                 " "))))


(provide 'nyan-prompt)
;;; nyan-prompt.el ends here
