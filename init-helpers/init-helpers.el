(require 'quick-init-helpers)

(defun load-local-init ()
  "Load local init if found."
  (let ((local-init (concat user-emacs-directory "local/init.el")))
    (when (file-readable-p local-init)
      (load-file local-init))))

(defun load-mode-init (name)
  "Load a mode-init file NAME expect an error if it doesn't map to an existing file."
  (let (file)
    (setq file (format "%smodes-init/init-%s.el" user-emacs-directory name))
    (if (file-exists-p file)
        (load-file file)
      (message "Warning: %s doesn't exist" file))))

(defun load-mode-init-file (name)
  "Load a mode-init file NAME expect an error if it doesn't map to an existing file."
  (let (file)
    (setq file (concat user-emacs-directory "modes-init/" name))
    (unless (or (equal name ".") (equal name ".."))
      (message "Loading Initializer: %s" file)
      (if (file-exists-p file)
          (load-file file)
        (message "Warning: %s doesn't exist" file)))))

;; Optional modes-init handling
(defun load-optional-mode-init (name)
  "Check for existence of a mode init script NAME, and load if found."
  (let (file)
    (setq file (format "%smodes-init/init-%s.el" user-emacs-directory (symbol-name name)))
    (if (file-readable-p file)
        (progn
          (load-file file)
          (message "Optional mode init: %s, was loaded" name))
      (message "Optional mode init: %s, not found" name))))

(defun set-window-system-font ()
  "Set the window system font."
  ;; Default Font for different window systems
  (when (window-system)
    ;; Mac OS X
    (when (eq system-type 'darwin)
      ;;(set-face-font 'default "Monaco")
      ;;(set-face-font 'default "Source Code Pro")
      ;;(set-face-font 'default "Source Code Pro Light")
      ;;(set-face-font 'default "Inconsolata")
      ;;(set-face-font 'default "Bitstream Vera Sans Mono")
      (set-face-font 'default "Menlo"))
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
