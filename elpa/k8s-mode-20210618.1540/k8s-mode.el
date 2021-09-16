;;; k8s-mode.el --- Major mode for Kubernetes configuration file -*- lexical-binding: t -*-

;; Copyright (C) 2019 Giap Tran <txgvnn@gmail.com>

;; Author: Giap Tran <txgvnn@gmail.com>
;; URL: https://github.com/TxGVNN/emacs-k8s-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (yaml-mode "0.0.10"))

;;; Commentary:
;; After open Kubernetes file, you have to M-x k8s-mode to enable this major
;; Put # -*- mode: k8s -*- in first line of file, if you want to autoload.
;;
;; If you're using yas-minor-mode and want to enable on k8s-mode
;; (add-hook 'k8s-mode-hook #'yas-minor-mode)
;;
;; With use-package style
;; (use-package k8s-mode
;;  :ensure t
;;  :config
;;  (setq k8s-search-documentation-browser-function 'browse-url-firefox)
;;  :hook (k8s-mode . yas-minor-mode))

;;; Code:

(require 'yaml-mode)

(defgroup k8s nil
  "Major mode of K8s configuration file."
  :group 'languages
  :prefix "k8s-")

(defcustom k8s-mode-hook nil
  "*Hook run by `k8s-mode'."
  :type 'hook
  :group 'k8s)

(defcustom k8s-mode-lighter "K8s"
  "K8s-mode lighter."
  :type 'string
  :group 'k8s)

(defcustom k8s-indent-offset 2
  "The tab width to use when indenting."
  :type 'integer
  :group 'k8s)

(defvar k8s-keywords
  '("kind"))

(defvar k8s-imenu-generic-expression
  '(("kind: " "^kind:\\(.*\\)" 1)))

(defvar k8s-font-lock-keywords
  `((,(regexp-opt k8s-keywords) . font-lock-builtin-face)
    ,@yaml-font-lock-keywords))

;; Yasnippet
(defconst k8s-dir (file-name-directory (or load-file-name
                                           buffer-file-name)))
(defconst k8s-snip-dir (expand-file-name "snippets" k8s-dir))

;; Completion
(defun k8s-complete-at-point ()
  "Perform keyword completion on word before cursor."
  (let* ((end (point))
         (begin (save-excursion
                  (skip-chars-backward "^ \n\r\t,:")
                  (point))))
    (list begin end k8s-keywords
          :exclusive 'no
          :company-docsig #'identity)))

;; Documents
(defcustom k8s-site-docs-url "https://kubernetes.io/docs/reference/generated/kubernetes-api/"
  "Default kubernetes.io site URL, the URL to use open docs."
  :group 'k8s
  :type 'string)

(defcustom k8s-site-docs-version "v1.21"
  "Default version API."
  :group 'k8s
  :type 'string)

(defcustom k8s-search-documentation-browser-function nil
  "Function to display K8S documentation in a WWW browser.

If non-nil, this shadows the value of `browse-url-browser-function' when
calling `k8s-search-documentation'.  This should be X11 browser as
`browse-url-mozilla`, `browse-url-chromium`"
  :group 'k8s
  :type '(choice (const :tag "default" nil) function)
  :link '(variable-link browse-url-browser-function))

(defun k8s-browse-documentation-url (url)
  "Browse a documentation URL using the configured browser function.

See `k8s-search-documentation-browser-function'."
  (let ((browse-url-browser-function
         (or k8s-search-documentation-browser-function
             browse-url-browser-function)))
    (browse-url url)))

(defsubst k8s-search-web-documentation ()
  "Return Kubernetes docs URL."
  (k8s-browse-documentation-url (concat k8s-site-docs-url k8s-site-docs-version)))

(defun k8s-goto-documents ()
  "Go to Kubernetes documentations."
  (interactive)
  (k8s-search-web-documentation))

;;;###autoload
(define-derived-mode k8s-mode yaml-mode k8s-mode-lighter
  "Major mode for editing Kubernetes configuration file."
  (font-lock-add-keywords nil k8s-font-lock-keywords)
  ;; indentation
  (set (make-local-variable 'yaml-indent-offset) k8s-indent-offset)
  ;; imenu
  (set (make-local-variable 'yaml-imenu-generic-expression) k8s-imenu-generic-expression)
  ;; completion
  (make-local-variable 'completion-at-point-functions)
  (push 'k8s-complete-at-point completion-at-point-functions))

(eval-after-load 'yasnippet
  '(when (file-directory-p k8s-snip-dir) (yas-load-directory k8s-snip-dir)))

(provide 'k8s-mode)

;;; k8s-mode.el ends here
