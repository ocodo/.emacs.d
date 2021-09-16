;;; ssh-config-mode.el --- Mode for fontification of ~/.ssh/config
;;
;; ssh-config-mode-el/ssh-config-mode.el ---
;;
;; $Id: ssh-config-mode.el,v 1.14 2012/05/14 05:29:26 harley Exp $
;;

;; Author:    Harley Gorrell <harley@panix.com>
;; URL:       https://github.com/jhgorrell/ssh-config-mode-el
;; Github:    https://raw.github.com/jhgorrell/ssh-config-mode-el/master/ssh-config-mode.el
;; License:   GPL v3+ (https://www.gnu.org/licenses/gpl-3.0.txt)
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
;;   (add-to-list 'auto-mode-alist '("/knownhosts\\'"       . ssh-known-hosts-mode))
;;   (add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
;;   (add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

;;; History:
;; * This keeps checkdoc happy.

;;; License
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
;; (eval-buffer)
;; (progn (delete-trailing-whitespace) (indent-region 0 (point-max)))
;; (byte-compile-file "ssh-config-mode.el" t)
;; (load "ssh-config-mode")

;; We use eval-and-compile so we can use them at compile
;; time and call regexp-opt during compliation.

(eval-and-compile
  (defun ssh-config-ssh-config-keywords-path ()
    "Find the path to 'ssh-config-keywords.txt'.
It should be next to 'ssh-config-mode.el'.
When testing add '.' to load-path so you find the local copy."
    (let ((path (locate-library "ssh-config-keywords.txt" nil)))
      ;;(message "ssh-config-keywords.txt is %s" path)
      path)))

(eval-and-compile
  (defun ssh-config-read-keywords (&optional file-path)
    "Read the list of ssh keywords, returning them as a list."
    ;; (message "ssh-config-read-keywords")
    (let ((file-path
           (or file-path
               (ssh-config-ssh-config-keywords-path))))
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

(defvar ssh-config-match-regexp "^\\s-*Match\\b"
  "Regexp to match the start of a match entry.")

;;
(defvar ssh-config-hostname-regexp
  "[-_.a-zA-Z0-9]+"
  "Regexp to match one hostname.  (rfc1123 2.1).")

(defcustom ssh-config-mode-indent 2
  "The width of indentation to use.
By default it's set to 2 as that is what man page
ssh_config(5) shows it as."
  :type 'integer
  :group 'ssh-config-mode)

(defun ssh-config-host-next ()
  "Skip to the next host entry."
  (interactive "^")
  (search-forward-regexp ssh-config-host-regexp))

(defun ssh-config-host-prev ()
  "Skip to the previous host entry."
  (interactive "^")
  (search-backward-regexp ssh-config-host-regexp))

(defun ssh-config-in-host-block-p ()
  "Are we inside a Host block?"
  (save-excursion
    (search-backward-regexp ssh-config-host-regexp nil t)))

(defun ssh-config-in-match-block-p ()
  "Are we inside a Match block?"
  (save-excursion
    (search-backward-regexp ssh-config-match-regexp nil t)))

(defun ssh-config-compute-indent ()
  "Compute the target indent for the current line.
Comments right above a 'Host' are considered to be about that Host."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Start of file and "Host" and "Match" should be at 0
     ((or (looking-at ssh-config-host-regexp)
	  (looking-at ssh-config-match-regexp)
          (and (not (ssh-config-in-host-block-p))
	       (not (ssh-config-in-match-block-p))))
      0)
     ;; Comment line
     ((looking-at "\\s-*#")
      ;; comments right before a "Host" or "Match" should be at 0
      (while (looking-at "\\s-*#")
        (forward-line))
      (if (or (looking-at ssh-config-host-regexp)
	      (looking-at ssh-config-match-regexp))
        0
        ssh-config-mode-indent))
     ;; default.
     (t
      ssh-config-mode-indent))))

(defun ssh-config-indent-line ()
  "Indent lines in the SSH config file."
  (interactive)
  (let ((target (ssh-config-compute-indent)))
    (save-excursion
      (indent-line-to target))))

;;
(defvar ssh-config-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    (define-key map [C-down]    'ssh-config-host-next)
    (define-key map [C-up]      'ssh-config-host-prev)
    ;;
    (define-key map "\C-c}"     'ssh-config-host-next)
    (define-key map "\C-c{"     'ssh-config-host-prev)
    ;;
    (define-key map (kbd "TAB") 'indent-for-tab-command)
    map)
  "The local keymap for `ssh-config-mode'.")

;;
(defvar ssh-config-mode-syntax-table nil)
(unless ssh-config-mode-syntax-table
  (let ((table  (make-syntax-table)))
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    (setq ssh-config-mode-syntax-table table)))

(defvar ssh-config-imenu-generic-expression
  `(("Hosts"
     ,(concat ssh-config-host-regexp "\\s-+\\(" ssh-config-hostname-regexp "\\)") 1)
    ("Matches"
     ,(concat ssh-config-match-regexp "\\s-+\\(.*\\)") 1))
  "Value for `imenu-generic-expression' in `ssh-config-mode'.
Only show the first hostname in the menu.")

(defun ssh-config-completion-at-point ()
  "Function used for `completion-at-point-functions' in `ssh-config-mode'."
  (interactive)
  (let* (
         (bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds)))
    (list start end ssh-config-keywords . nil )))

;;;###autoload
(define-derived-mode ssh-config-mode
  fundamental-mode
  "ssh-config"
  "Major mode for fontifiying ssh config files.

\\{ssh-config-mode-map}"
  (setq
   comment-start "#"
   comment-end   "")
  (make-local-variable 'font-lock-defaults)
  (setq
   font-lock-defaults '(ssh-config-font-lock-keywords nil t))
  ;;
  (setq-local indent-line-function 'ssh-config-indent-line)
  (setq-local imenu-generic-expression ssh-config-imenu-generic-expression)
  (add-hook 'completion-at-point-functions 'ssh-config-completion-at-point nil 'local))

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

(defvar ssh-known-hosts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `ssh-known-hosts-mode'.
Just sets the comment syntax.")

;;;;;

;; host.example.com,1.1.1.1
;; |1|hash=|hash=
(defvar ssh-known-hosts-regex-hashed
  "\\(?:|[0-9]+|[-0-9A-Za-z=|/+]+\\)"
  "Regex for matching hashed addresses.")

(defvar ssh-known-hosts-regex-ipv4
  "\\(?:[0-9]+.[0-9]+.[0-9]+.[0-9]+\\)"
  "Regex for matching ipv4 addresses.")

(defvar ssh-known-hosts-regex-ipv6
  "\\(?:[0-9a-f:]+\\(?:%[a-z0-9]+\\)?\\)"
  "Regex for matching ipv6 addresses.")

(defvar ssh-known-hosts-regex-ip
  (concat
   "\\(?:"
   ssh-known-hosts-regex-ipv4
   "\\|"
   ssh-known-hosts-regex-ipv6
   "\\)")
  "Regex for matching ip addresses.")

(defvar ssh-known-hosts-regex-ipv6
  "\\(?:[0-9a-f:]+\\(?:%[a-z0-9]+\\)\\)"
  "Regex for matching ipv6 addresses.")

;; This is more specfic than "ssh-config-hostname-regexp"; merge them?
(defvar ssh-known-hosts-regex-hostname
  "\\(?:\\(?:[a-zA-Z0-9_][-a-zA-Z0-9_]*[.]\\)*[a-zA-Z_][-a-zA-Z0-9_]*\\)"
  "Regex for matching hostnames.
We permit underscores.")

;; :2222
(defvar ssh-known-hosts-regex-port
  "\\(?:[0-9]+\\)"
  "Regex for matching an port.")

(defvar ssh-known-hosts-regex-host
  (concat
   "\\(?:"
   ssh-known-hosts-regex-hashed
   "\\|"
   ssh-known-hosts-regex-ip
   "\\|"
   ssh-known-hosts-regex-hostname
   "\\)"))

;; NOTE: font-lock-studio might be of help when making changes.
(defvar ssh-known-hosts-font-lock-keywords
  ;; We want to match lines like the following:
  ;; Short list in ./tests/known_hosts_short
  ;; Full list in ./tests/known_hosts
  ;; More test data in: openssh-portable/regress/unittests/hostkeys/testdata/known_hosts
  `(
    (,(concat
       "^"
       ;; @marker (optional):
       "\\(@[-a-z]+ +\\|\\)"

       ;; hostname:
       "\\("

       ;; |1|hash=|hash=
       ssh-known-hosts-regex-hashed
       "\\|"

       ;; hostname-only
       ssh-known-hosts-regex-hostname
       "\\|"

       ;; ip-only
       ssh-known-hosts-regex-ip
       "\\|"

       ;; hostname "," ip
       "\\(?:" ssh-known-hosts-regex-hostname "," ssh-known-hosts-regex-ip "\\)"
       "\\|"

       ;; [host-or-ip]:222
       "\\(?:\\[" ssh-known-hosts-regex-host "\\]:" ssh-known-hosts-regex-port "\\)"
       "\\|"

       ;; We arent matching ports, but they should be the same.
       ;; [ssh.github.com]:443,[192.1.2.3]:443
       "\\(?:"
       "\\[" ssh-known-hosts-regex-hostname "\\]:" ssh-known-hosts-regex-port ","
       "\\[" ssh-known-hosts-regex-ip       "\\]:" ssh-known-hosts-regex-port "\\)"

       "\\)"
       "[ \t]+"

       ;; key type:
       ;; ssh-rsa, ecdsa-sha2-nistp384, ...
       "\\(\\(?:ecdsa\\|ssh\\)[-0-9A-Za-z]*\\)"
       "[ \t]+"

       ;; public key:
       ;; base64data==
       "\\(AA[0-9A-Za-z/+]+=*\\)"
       )
     (1 font-lock-warning-face)
     (2 font-lock-function-name-face)
     (3 font-lock-keyword-face)
     (4 font-lock-string-face)
     ))
  "Expressions to hilight in `ssh-known-hosts-mode'.
We want to try and be a good match, so misformatted ones stand out.
So we dont just match .* for the hostname.")

;;;###autoload
(defun ssh-known-hosts-mode ()
  "Major mode for fontifiying ssh known_hosts files.
\\{ssh-known-hosts-mode}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table ssh-known-hosts-mode-syntax-table)
  (setq
   mode-name "ssh-known-hosts"
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
       "\\([0-9A-Za-z+/]+=*\\)"
       ;; comment in public key
       "\\(?: \\(.*\\)\\)?"
       "$")
      '(1 font-lock-keyword-face)
      '(2 font-lock-string-face)
      '(3 font-lock-comment-face nil t)
      )))
  ;; Not define `auto-mode-alist' obey the other mode in this elisp.
  nil
  nil)

;; done loading
(run-hooks 'ssh-config-mode-load-hook)
(provide 'ssh-config-mode)

;;; ssh-config-mode.el ends here
