;;; ivy-historian.el --- Persistently store selected minibuffer candidates -*- lexical-binding: t -*-

;; Copyright (C) 2017 PythonNut

;; Author: PythonNut <pythonnut@pythonnut.com>
;; Keywords: convenience, ivy
;; Package-Version: 20210714.56
;; Package-Commit: 852cb4e72c0f78c8dbb2c972bdcb4e7b0108ff4c
;; Version: 20170111
;; URL: https://github.com/PythonNut/historian.el
;; Package-Requires: ((emacs "24.4") (historian "20170111") (ivy "0.8.0") (flx "0.6.1"))

;;; Commentary:

;; ivy-historian.el integrates historian with ivy.

;;; Code:

(require 'historian)
(eval-when-compile (require 'cl-lib))

(defgroup ivy-historian nil
  "Persistently store selected minibuffer candidates"
  :group 'convenience
  :prefix "ivy-historian-")

(defcustom ivy-historian-auto-enable-historian-mode t
  "Whether to enable historian-mode when ivy-historian-mode is enabled."
  :type 'boolean
  :group 'ivy-historian)

(defcustom ivy-historian-freq-boost-factor 100
  "Relative weight of frequency boost.
(Requires some experimenting to get a feel for values)"
  :type 'number
  :group 'ivy-historian)

(defcustom ivy-historian-recent-boost 100
  "Relative weight of recency boost.
(Requires some experimenting to get a feel for values)"
  :type 'number
  :group 'ivy-historian)

(defcustom ivy-historian-recent-decrement 5
  "Decrease in score as item gets less recent.
(Requires some experimenting to get a feel for values)"
  :type 'number
  :group 'ivy-historian)

(defvar ivy-historian--saved-this-command nil)

(defun ivy-historian--nadvice/ivy-read (old-fun &rest args)
  (cl-destructuring-bind (&rest _args &key caller &allow-other-keys) args
    (setq ivy-historian--saved-this-command (or caller this-command))
    (cl-letf* ((old-rfm (symbol-function #'read-from-minibuffer))
               ((symbol-function #'read-from-minibuffer)
                (lambda (&rest args)
                  (historian-push-item this-command
                                       (apply old-rfm args)))))
      (apply old-fun args))))

(defun ivy-historian--nadvice/ivy--flx-sort (old-fun name cands)
  (if (not historian-mode)
      (funcall old-fun name cands)
    (cl-letf*
        ((old-flx-score (symbol-function 'flx-score))
         ((symbol-function 'flx-score)
          (lambda (str query &optional cache)
            (let* ((orig-score
                    (funcall old-flx-score str query cache))
                   (history (gethash ivy-historian--saved-this-command
                                     historian--history-table)))
              (if history
                  (let* ((freq (if (gethash str (cdr history))
                                   (/ (float (gethash str (cdr history) 0))
                                      (let ((total 0))
                                        (maphash
                                         (lambda (_key value)
                                           (cl-incf total value))
                                         (cdr history))
                                        total))
                                 0))
                         (freq-boost (* freq ivy-historian-freq-boost-factor))
                         (recent-index (cl-position str (car history)
                                                    :test #'string=))
                         (recent-boost (if recent-index
                                           (- ivy-historian-recent-boost
                                              (* ivy-historian-recent-decrement
                                                 recent-index))
                                         0)))
                    (cons
                     (+ (or (car orig-score) most-negative-fixnum)
                        freq-boost
                        recent-boost)
                     (cdr orig-score)))
                orig-score)))))
      (funcall old-fun name cands))))

;;;###autoload
(define-minor-mode ivy-historian-mode
  "historian minor mode"
  :init-value nil
  :group 'ivy-historian
  :global t
  (require 'ivy)
  (if ivy-historian-mode
      (progn
        (when ivy-historian-auto-enable-historian-mode
          (historian-mode +1))
        (advice-add 'ivy-read :around
                    #'ivy-historian--nadvice/ivy-read)
        (advice-add 'ivy--flx-sort :around
                    #'ivy-historian--nadvice/ivy--flx-sort))

    (advice-remove 'ivy-read #'ivy-historian--nadvice/ivy-read)
    (advice-remove 'ivy--flx-sort
                   #'ivy-historian--nadvice/ivy--flx-sort)))

(provide 'ivy-historian)

;;; ivy-historian.el ends here
