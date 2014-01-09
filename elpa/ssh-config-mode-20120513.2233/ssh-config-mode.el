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
;; Version: 20120513.2233
;; X-Original-Version:   $Revision: 1.14 $

;;; Commentary:
;; * Fontifys the ssh config keywords.
;; * keys for skipping from host section to host section.
;; * Add the following to your startup file.
;;   (autoload 'ssh-config-mode "ssh-config-mode" t)
;;   (add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
;;   (add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))
;;   (add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

;;; History:
;; * This keeps checkdoc happy.

;;; Code:

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
;; (from ssh-v4)
(eval-and-compile

  (defvar ssh-config-words-ssh
    '("AddressFamily" "BatchMode" "BindAddress"
      "ChallengeResponseAuthentication" "CheckHostIP" "Cipher"
      "Ciphers" "ClearAllForwardings" "Compression"
      "CompressionLevel" "ConnectionAttempts" "ConnectTimeout"
      "ControlMaster" "ControlPath" "DynamicForward" "EscapeChar"
      "ForwardAgent" "ForwardX11" "ForwardX11Trusted"
      "GatewayPorts" "GlobalKnownHostsFile" "GSSAPIAuthentication"
      "GSSAPIDelegateCredentials" "HashKnownHosts"
      "Host" "HostbasedAuthentication" "HostKeyAlgorithms"
      "HostKeyAlias" "HostName"
      "IdentityFile" "IdentitiesOnly"
      "KbdInteractiveDevices" "LocalForward" "LogLevel" "MACs"
      "NoHostAuthenticationForLocalhost" "NumberOfPasswordPrompts"
      "PasswordAuthentication" "Port" "PreferredAuthentications"
      "Protocol" "ProxyCommand" "PubkeyAuthentication"
      "RemoteForward" "RhostsRSAAuthentication"
      "RSAAuthentication" "SendEnv" "ServerAliveInterval"
      "ServerAliveCountMax" "SmartcardDevice"
      "StrictHostKeyChecking" "TCPKeepAlive" "UsePrivilegedPort"
      "User" "UserKnownHostsFile" "VerifyHostKeyDNS"
      "XAuthLocation")
    "A list of keywords allowed in a user ssh config file.")

  (defvar ssh-config-words-sshd
    '("AcceptEnv" "AddressFamily" "AllowGroups"
      "AllowTcpForwarding" "AllowUsers" "AuthorizedKeysFile"
      "Banner" "ChallengeResponseAuthentication" "Ciphers"
      "ClientAliveInterval" "ClientAliveCountMax" "Compression"
      "DenyGroups" "DenyUsers" "GatewayPorts"
      "GSSAPIAuthentication" "GSSAPICleanupCredentials"
      "HostbasedAuthentication" "HostKey" "IgnoreRhosts"
      "IgnoreUserKnownHosts" "KerberosAuthentication"
      "KerberosGetAFSToken" "KerberosOrLocalPasswd"
      "KerberosTicketCleanup" "KeyRegenerationInterval"
      "ListenAddress" "LoginGraceTime" "LogLevel" "MACs"
      "MaxAuthTries" "MaxStartups" "PasswordAuthentication"
      "PermitEmptyPasswords" "PermitRootLogin"
      "PermitUserEnvironment" "PidFile" "Port" "PrintLastLog"
      "PrintMotd" "Protocol" "PubkeyAuthentication"
      "RhostsRSAAuthentication" "RSAAuthentication"
      "ServerKeyBits" "StrictModes" "Subsystem" "SyslogFacility"
      "TCPKeepAlive" "UseDNS" "UseLogin" "UsePrivilegeSeparation"
      "X11DisplayOffset" "X11Forwarding" "X11UseLocalhost"
      "XAuthLocation")
    "A list of keywords allowed in a system sshd config file.")
  nil)

(defvar ssh-config-font-lock-keywords
  ;; how to put eval-when-compile without recursive require?
  (eval-when-compile
    `((,(regexp-opt (append ssh-config-words-ssh ssh-config-words-sshd) 'words)
       (1 font-lock-keyword-face))))
  "Expressions to hilight in `ssh-config-mode'.")
;; ssh-config-font-lock-keywords

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
  (setq font-lock-defaults '(ssh-config-font-lock-keywords))
  ;;
  (run-hooks 'ssh-config-mode-hook)
  nil)

;;;;;

(defvar ssh-config-mode-hook nil
  "*Hook to setup `ssh-config-mode'.")

(defvar ssh-known-hosts-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    map)
  "The local keymap for `ssh-known-hosts-mode'.")

(defvar ssh-known-hosts-font-lock-keywords
  ;; how to put eval-when-compile without recursive require?
  '(("^\\([-a-z0-9.,]+\\)\\s-+\\(ssh-\\sw*\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-keyword-face)))
  "Expressions to hilight in `ssh-config-mode'.")
;; ssh-config-font-lock-keywords

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

;; done loading
(run-hooks 'ssh-config-mode-load-hook)
(provide 'ssh-config-mode)

;;; ssh-config-mode.el ends here
