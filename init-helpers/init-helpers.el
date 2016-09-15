;;; Code:

(require 'quick-init-helpers)

(defun ocodo-active-config-directory ()
  "Where active package configurations are kept."
  (format "%s_activated/" user-emacs-directory))

(defun load-local-init ()
  "Load local init if found."
  (let ((local-init (concat user-emacs-directory "local/init.el")))
    (when (file-readable-p local-init)
      (load-file local-init))))

(defun load-use-file (name)
  "Load a use file NAME expect an error if it doesn't map to an existing file."
  (let (file)
    (setq file (concat (ocodo-active-config-directory) name))
    (unless (or (equal name ".") (equal name ".."))
      (message "Using config: %s" file)
      (if (file-exists-p file)
          (load-file file)
        (message "Warning: %s doesn't exist" file)))))

;; Optional use file handling
(defun load-optional-use-file (name)
  "Check for existence of a use script NAME, and load if found."
  (let (file)
    (setq file (format "%suse-%s.el" (ocodo-active-config-directory) (symbol-name name)))
    (if (file-readable-p file)
        (progn
          (load-file file)
          (message "Optional use-file: %s, was loaded" name))
      (message "Optional use-file: %s, not found" name))))

(defun set-window-system-font ()
  "Set the window system font."
  ;; Default Font for different window systems
  (when (window-system)
    ;; Mac OS X
    (when (eq system-type 'darwin)
      ;;
      ;;(set-face-font 'default "Monaco")
      ;;(set-face-font 'default "Menlo")
      ;;(set-face-font 'default "Source Code Pro")
      ;;(set-face-font 'default "Source Code Pro Light")
      ;;(set-face-font 'default "Inconsolata")
      ;;(set-face-font 'default "Bitstream Vera Sans Mono")

      ;; ================================================
      ;; a copy of SauceCodePro is kept in emacs.d/fonts
      (set-face-attribute 'default nil
                          :font "SauceCodePro Nerd Font"
                          :weight 'light)
      ;;
      )
    ;; Sample Text for font viewing
    '("ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      "abcdefghijklmnopqrstuvwxyz"
      "1234567890!@#$%^&*()-=_+[]\{}|;':<>?,./")

    ;; Windows whatever...
    (when (eq system-type 'windows-nt)
      (set-face-font 'default "Consolas"))

    ;; GNU Linux (Droid or Vera)
    (when (eq system-type 'gnu/linux)
      ;; for quick swapping.
      ;; (set-face-font 'default "Bitstream Vera Sans Mono")
      ;; (set-face-font 'default "Droid Sans Mono")
      (set-face-font 'default "DejaVu Sans Mono"))))

(provide 'init-helpers)

;;; init-helpers.el ends here
