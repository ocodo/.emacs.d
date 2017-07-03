;;; ssh-config-mode.el --- Mode for fontification of ~/.ssh/config
;;
;; ~harley/share/emacs/pkg/ssh/ssh-config-mode.el ---
;;
;; $Id: ssh-config-mode.el,v 1.14 2012/05/14 05:29:26 harley Exp $
;;

;; Author:    Harley Gorrell <harley@panix.com>
;; URL:       http://www.mahalito.net/~harley/elisp/ssh-config-mode.el
;; Github:    https://raw.github.com/jhgorrell/ssh-config-mode-el/master/ssh-config-mode.el
;; License:   GPL v2
;; Keywords:  ssh, config, emacs
;; Version:   $Revision: 1.14 $
;; Tag:       20170413T0010

;;; Commentary:
;; * Fontifys the ssh config keywords.
;; * keys for skipping from host section to host section.
;; * Add the following to your startup file.
;;   (autoload 'ssh-config-mode "ssh-config-mode" t)
;;   (add-to-list 'auto-mode-alist '("/\\.ssh/config\\'"     . ssh-config-mode))
;;   (add-to-list 'auto-mode-alist '("/sshd?_config\\'"      . ssh-config-mode))
;;   (add-to-list 'auto-mode-alist '("/known_hosts\\'"       . ssh-known-hosts-mode))
;;   (add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
;;   (add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

;;; History:
;; * This keeps checkdoc happy.

;;; Code:
;; (eval-buffer)
;; (progn (delete-trailing-whitespace) (indent-region 0 (point-max)))
;; (byte-compile-file "ssh-config-mode.el" t)
;; (load "ssh-config-mode")

;; We use eval-and-compile so we can use them at compile
;; time and call regexp-opt during compliation.

(eval-and-compile
  (defvar ssh-config-load-file-dir
    (if load-file-name
      (file-name-directory load-file-name)
      default-directory)
    "Where this file was loaded from."))

(eval-and-compile
  (defun ssh-config-read-keywords (&optional file-path)
    ;; (message "ssh-config-read-keywords")
    (let ((file-path
           (or file-path
               (concat
                (file-name-as-directory ssh-config-load-file-dir)
                "ssh-config-keywords.txt"))))
      (with-temp-buffer
        (insert-file-contents file-path)
        (split-string (buffer-string) "\n" t)))))

(eval-and-compile
  (defvar ssh-config-keywords
    (eval-when-compile
      (ssh-config-read-keywords)))
  "A list of keywords allowed in a user ssh config file.")

(eval-and-compile
  (defvar ssh-config-font-lock-keywords
    (eval-when-compile
      `((
         ,(regexp-opt ssh-config-keywords 'words)
         (1 font-lock-keyword-face)
         )))
    "Expressions to hilight in `ssh-config-mode'."))
;; ssh-config-font-lock-keywords

;; Setup
(defvar ssh-config-mode-load-hook nil
  "*Hook to run when `ssh-config-mode' is loaded.")

;;
(defvar ssh-config-mode-hook nil
  "*Hook to setup `ssh-config-mode'.")
(defvar ssh-config-host-regexp "^\\s-*Host\\b"
  "Regexp to match the start of a host entry.")

(defun ssh-config-host-next ()
  "Skip to the next host entry."
  (interactive)
  (search-forward-regexp ssh-config-host-regexp))
(defun ssh-config-host-prev ()
  "Skip to the previous host entry."
  (interactive)
  (search-backward-regexp ssh-config-host-regexp))

;;
(defvar ssh-config-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    (define-key map [C-up]   'ssh-config-host-prev)
    (define-key map [C-down] 'ssh-config-host-next)
    map)
  "The local keymap for `ssh-config-mode'.")

;;
(defvar ssh-config-mode-syntax-table nil)
(unless ssh-config-mode-syntax-table
  (let ((table  (make-syntax-table)))
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    (setq ssh-config-mode-syntax-table table)))

;; These keywords listed here to be fed into regexp-opt.

;;;###autoload
(defun ssh-config-mode ()
  "Major mode for fontifiying ssh config files.

\\{ssh-config-mode-map}"
  (interactive)
  ;;
  (kill-all-local-variables)
  (set-syntax-table ssh-config-mode-syntax-table)
  (setq mode-name "ssh-config"
        major-mode 'ssh-config-mode
        comment-start "#"
        comment-end   "")
  (use-local-map ssh-config-mode-map)
  ;;
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(ssh-config-font-lock-keywords nil t))
  ;;
  (run-hooks 'ssh-config-mode-hook)
  nil)

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("/\\.ssh/config\\'" . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("/sshd?_config\\'" . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("/known_hosts\\'" . ssh-known-hosts-mode))
  (add-to-list 'auto-mode-alist '("/authorized_keys\\'" . ssh-authorized-keys-mode)))

;;;;;

(defvar ssh-known-hosts-mode-hook nil
  "*Hook to setup `ssh-config-mode'.")

(defvar ssh-known-hosts-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    map)
  "The local keymap for `ssh-known-hosts-mode'.")

(defvar ssh-known-hosts-font-lock-keywords
  ;; how to put eval-when-compile without recursive require?
  `((,(concat
       "^"
       ;; marker (optional)
       "\\(?:\\(@[^[:space:]]+\\)\\s-+\\)?"
       ;; hostnames
       "\\([-*a-z0-9.,]+\\||[^[:space:]]+\\)\\s-+"
       ;; public key (fontify just key type)
       "\\(\\(?:ssh\\|ecdsa\\)[^[:space:]]*\\|\\)"
       )
     (1 font-lock-variable-name-face nil t)
     (2 font-lock-function-name-face)
     (3 font-lock-keyword-face)
     )
    ("^[[:space:]]*\\(#.*\\)"
     (1 font-lock-comment-face))
    )
  "Expressions to hilight in `ssh-config-mode'.")
;; ssh-config-font-lock-keywords

;;;###autoload
(defun ssh-known-hosts-mode ()
  "Major mode for fontifiying ssh known_hosts files.
\\{ssh-known-hosts-mode}"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "ssh-known-hosts"
        major-mode 'ssh-known-hosts-mode
        comment-start "#"
        comment-end   "")
  (use-local-map ssh-known-hosts-mode-map)
  ;;
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(ssh-known-hosts-font-lock-keywords))
  ;;
  (run-hooks 'ssh-known-hosts-mode-hook)
  nil)

;;;;;

;;;###autoload (autoload 'ssh-authorized-keys-mode "ssh-config-mode" nil t)
(define-generic-mode ssh-authorized-keys-mode
  '(?\#)
  nil
  (eval-when-compile
    (list
     (list
      (concat
       ;; ignore options
       ;; double quoted string will be fontified by generic mode.
       ;; key type
       "\\(\\(?:ecdsa\\|ssh\\)-[^[:space:]]+\\)\\s-+"
       ;; base64
       "\\([^[:space:]\n]+\\)"
       ;; comment in public key
       "\\(?: \\(.*\\)\\)?"
       "$")
      '(1 font-lock-keyword-face)
      ;; not fontify like known_hosts
      ;; '(2 font-lock-variable-name-face)
      '(3 font-lock-comment-face nil t)
      )))
  ;; Not define `auto-mode-alist' obey the other mode in this elisp.
  nil
  nil)

;; done loading
(run-hooks 'ssh-config-mode-load-hook)
(provide 'ssh-config-mode)

;;; ssh-config-mode.el ends here
