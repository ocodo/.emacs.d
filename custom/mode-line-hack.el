;; Mode line setup - forked from http://amitp.blogspot.com.au/2011/08/emacs-custom-mode-line.html
(setq-default
 mode-line-format
 '(; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ; emacsclient [default -- keep?]
   mode-line-client
   "  "
   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " *-* " 'face 'mode-line-modified-face))
          (t "      ")))
   "    "
   ; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ; narrow [default -- keep?]
   " %n "
   ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   "    "
   ; nyan-mode uses nyan cat as an alternative to %p
   (:eval (when nyan-mode (list (nyan-create))))
   ))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil          :foreground "gray60" :background "#1B141F" :inverse-video nil :box '(:line-width 4 :color "#1B141F" :style nil))
(set-face-attribute 'mode-line-inactive nil :foreground "gray80" :background "#1B141F" :inverse-video nil :box '(:line-width 4 :color "#1B141F" :style nil))

(set-face-attribute 'mode-line-read-only-face nil  :inherit 'mode-line-face          :foreground "#4271ae" :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil   :inherit 'mode-line-face          :foreground "#FF0000" :background "#999" :box '(:line-width 2 :color "#880000"))
(set-face-attribute 'mode-line-folder-face nil     :inherit 'mode-line-face          :foreground "gray50")
(set-face-attribute 'mode-line-filename-face nil   :inherit 'mode-line-face          :foreground "#3467a0" :weight 'bold)
(set-face-attribute 'mode-line-position-face nil   :inherit 'mode-line-face                                :family "Menlo" :height 100)
(set-face-attribute 'mode-line-mode-face nil       :inherit 'mode-line-face          :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil :inherit 'mode-line-mode-face     :foreground "gray40" :height 90)
(set-face-attribute 'mode-line-process-face nil    :inherit 'mode-line-face          :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil      :inherit 'mode-line-position-face :foreground "black" :background "#db6000")

(provide 'mode-line-hack)
