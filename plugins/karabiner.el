;;; karabiner.el --- run karabiner (an osx keyboard hacking tool) command line tool from emacs

;;; Author: Jason Milkins
;;; Version: 1.0

;;; Commentary:
;;  an Emacs wrapper for OSX Karabiner - https://pqrs.org/osx/karabiner
;;

;;; Code:
(defgroup karabiner nil
  "Run Karabiner commands from Emacs."
  :group 'tools)

(defcustom karabiner-executable-name
  "karabiner"
  "Name of the Karabiner command line executable."
  :type 'file
  :group 'karabiner)

(defcustom karabiner-executable-folder
  "/Applications/Karabiner.app/Contents/Library/bin/"
  "Name of the Karabiner command line executable."
  :type 'directory
  :group 'karabiner)

(defun karabiner-reloadxml ()
  "Reload karabiner private.xml.
Use non-blocking async-shell-command."
  (interactive)
  (karabiner-exec "reloadxml" nil))

(defun karabiner-list-profiles ()
  "List karabiner profiles."
  (interactive)
  (karabiner-exec "list" nil))

(defun karabiner-select-profile (profile)
  "Select a karabiner PROFILE."
  (interactive
   (list (completing-read "Select Keyboard Profile: " (karabiner-list-to-sequence))))
  (karabiner-exec-to-string "select" (string-to-number profile)))

(defun karabiner-add-profile (profile)
  "Add a karabiner PROFILE."
  (interactive "sProfile Name: ")
  (karabiner-exec-to-string "append" profile))

(defun karabiner-delete-profile (profile)
  "Delete a karabiner PROFILE."
  (interactive
   (list (completing-read "Delete Keyboard Profile: " (karabiner-list-to-sequence))))
  (karabiner-exec-to-string "delete" (string-to-number profile)))

(defun karabiner-set (identifier)
  "Change karabiner setting IDENTIFIER."
  (interactive
   (list (completing-read "Delete Keyboard Profile: " (karabiner-list-to-sequence))))
  (karabiner-exec-to-string "delete" (string-to-number profile)))



(defun karabiner-list-to-sequence ()
  "Get profile list as a string."
  (split-string (karabiner-exec-to-string "list" nil) "\n" t "\s"))

(defun karabiner-exec-to-string (command args)
  "Run the karabiner executable with COMMAND and ARGS.
Return output to string.
ARGS is simply a string containing all args."
  (shell-command-to-string
   (format "%s/%s %s %s"
           karabiner-executable-folder
           karabiner-executable-name
           command args)))

(defun karabiner-exec (command args)
  "Run the karabiner executable with COMMAND and ARGS.
Return output to async shell command buffer.
ARGS is simply a string containing all args."
  (async-shell-command
   (format "%s%s %s %s"
           karabiner-executable-folder
           karabiner-executable-name
           command args)))

(provide 'karabiner)
;;; karabiner.el ends here
