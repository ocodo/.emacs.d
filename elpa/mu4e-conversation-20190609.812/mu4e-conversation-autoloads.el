;;; mu4e-conversation-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mu4e-conversation" "mu4e-conversation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from mu4e-conversation.el

(put 'global-mu4e-conversation-mode 'globalized-minor-mode t)

(defvar global-mu4e-conversation-mode nil "\
Non-nil if Global Mu4e-Conversation mode is enabled.
See the `global-mu4e-conversation-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-mu4e-conversation-mode'.")

(custom-autoload 'global-mu4e-conversation-mode "mu4e-conversation" nil)

(autoload 'global-mu4e-conversation-mode "mu4e-conversation" "\
Toggle Mu4e-Conversation mode in all buffers.
With prefix ARG, enable Global Mu4e-Conversation mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Mu4e-Conversation mode is enabled in all buffers where
`mu4e-conversation--turn-on' would do it.
See `mu4e-conversation-mode' for more information on Mu4e-Conversation mode.

\(fn &optional ARG)" t nil)

(autoload 'mu4e-conversation "mu4e-conversation" "\


\(fn &optional MSG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mu4e-conversation" '("mu4e-conversation-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mu4e-conversation-autoloads.el ends here
