;;; docker-mode.el --- Support for Dockerfile editing

;; Copyright (C) 2013 Justin Abrahms

;; Author: Justin Abrahms <justin@abrah.ms>
;; Created: 20 October 2013
;; Keywords: docker dockerfile
;; Version: 20131021.1605
;; X-Original-Version: 0.0.1

;; This file is not part of GNU Emacs.

;; This software is released under the MIT license.

;;; Code
(require 'regexp-opt)

(defvar docker-keywords '("FROM" "MAINTAINER" "RUN" "CMD" "EXPOSE" "ENV" "ADD"
                          "ENTRYPOINT" "VOLUME" "USER" "WORKDIR"))

(defvar docker-keywords-regexp
  (concat "^\\s-*" (regexp-opt docker-keywords 'words)))

(defun docker-font-lock-keywords ()
  `((,docker-keywords-regexp . font-lock-keyword-face)))

;;;###autoload
(define-derived-mode docker-mode sh-mode "docker"
  "Major mode for editing Dockerfiles"
  (setq font-lock-defaults '((docker-font-lock-keywords))))

;;;###autoload
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . docker-mode))

(provide 'docker-mode)
;;; docker-mode.el ends here
