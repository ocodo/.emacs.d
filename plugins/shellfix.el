;; Shell fixes by - Oleksandr Manzyuk - github.com/manzyuk/dotfiles/.emacs.d/init.el
;; Assembled into module by Jasonm23 - github.com/jasonm23
;; Use - place in load-path

;;;###autoload
(defun regexp-alternatives (regexps)
  (mapconcat (lambda (regexp) (concat "\\(" regexp "\\)")) regexps "\\|"))

;;;###autoload
(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")

;;;###autoload
(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward
            non-sgr-control-sequence-regexp end t)
      (replace-match ""))))

;;;###autoload
(defun filter-non-sgr-control-sequences-in-output (ignored)
  (let ((start-marker
         (or comint-last-output-start
             (point-min-marker)))
        (end-marker
         (process-mark
          (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker
     end-marker)))

;;;###autoload
(defun ansi-color-get-bold-color (color)
  (or (cdr (assoc color ansi-color-bold-colors))
      color))

;;;###autoload
(defun ansi-color-boldify-face (face)
  (if (consp face)
      (let* ((property   (car face))
             (color      (cdr face))
             (bold-color (ansi-color-get-bold-color color)))
        (ansi-color-make-face property bold-color))
    face))

;;;###autoload
(eval-after-load "ansi-color"
  '(progn
     ;; Copied from `ansi-color.el' and modified to display bold faces
     ;; using slighly different, brigher colors.
     (defun ansi-color-get-face (escape-seq)
       (let ((i 0)
             f val)
         (while (string-match ansi-color-parameter-regexp escape-seq i)
           (setq i (match-end 0)
                 val (ansi-color-get-face-1
                      (string-to-number (match-string 1 escape-seq) 10)))
           (cond ((not val))
                 ((eq val 'default)
                  (setq f (list val)))
                 (t
                  (unless (member val f)
                    (push val f)))))
         ;; Use brighter colors for bold faces.
         (when (member 'bold f)
           (setq f (mapcar 'ansi-color-boldify-face f)))
         f))
     ;; Copied from `ansi-color.el' and modified to support so called
     ;; high intensity colors.
     (defun ansi-color-make-color-map ()
       (let ((ansi-color-map (make-vector 110 nil))
             (index 0))
         ;; miscellaneous attributes
         (mapc
          (function (lambda (e)
                      (aset ansi-color-map index e)
                      (setq index (1+ index)) ))
          ansi-color-faces-vector)
         ;; foreground attributes
         (setq index 30)
         (mapc
          (function (lambda (e)
                      (aset ansi-color-map index
                            (ansi-color-make-face 'foreground e))
                      (setq index (1+ index)) ))
          ansi-color-names-vector)
         ;; background attributes
         (setq index 40)
         (mapc
          (function (lambda (e)
                      (aset ansi-color-map index
                            (ansi-color-make-face 'background e))
                      (setq index (1+ index)) ))
          ansi-color-names-vector)
         ;; foreground attributes -- high intensity
         (setq index 90)
         (mapc
          (function (lambda (e)
                      (aset ansi-color-map index
                            (ansi-color-make-face 'foreground e))
                      (setq index (1+ index)) ))
          ansi-color-names-vector)
         ;; background attributes -- high intensity
         (setq index 100)
         (mapc
          (function (lambda (e)
                      (aset ansi-color-map index
                            (ansi-color-make-face 'background e))
                      (setq index (1+ index)) ))
          ansi-color-names-vector)
         ansi-color-map))))

;;;###autoload
(defun ansi-color-generate-color-map ()
  (setq ansi-color-map (ansi-color-make-color-map)))

;; More reliable shell directory tracking.  Unfortunately, Unix-only.
;; From https://github.com/nelhage/elisp/blob/master/dot-emacs.

;;;###autoload
(defun inferior-process-cwd (buffer)
  (let ((proc (get-buffer-process buffer)))
    (when proc
      (let ((pid (process-id proc)))
        (file-symlink-p (format "/proc/%d/cwd" pid))))))

;;;###autoload
(defun shell-mode-chdir (text)
  (let ((cwd (inferior-process-cwd (current-buffer))))
    (when cwd (cd cwd)))
  text)

;;;###autoload
(defun turn-on-directory-tracking ()
  (shell-dirtrack-mode 0)
  (if (memq system-type (list 'ms-dos 'windows-nt 'cygwin))
      ;; On non-Unix systems, track directory by watching the prompt.
      (dirtrack-mode 1)
    (add-hook 'comint-preoutput-filter-functions 'shell-mode-chdir)))

;;;###autoload
(when load-file-name

  (message "Loading Shell Fixes")
  
  (setq non-sgr-control-sequence-regexp
        (regexp-alternatives
         '(;; icon name escape sequences
           "33\\][0-2];.*?07"
           ;;
           "\\][0-2];"
           ;;
           ""
           ;;
           "\\[J"         
           ;; non-SGR CSI escape sequences
           "33\\[\\??[0-9;]*[^0-9;m]"
           ;; noop
           "1233\\[2K33\\[1F"
           )))

  (setq shell-font-lock-keywords nil)

  ;; Define colors a la the default gnome-terminal color theme.
  (setq color0  "#000000"
        color1  "#CC0000"
        color2  "#4E9A06"
        color3  "#C4A000"
        color4  "#3465A4"
        color5  "#75507B"
        color6  "#06989A"
        color7  "#D3D7CF"
        color8  "#555753"
        color9  "#ef2929"
        color10 "#8ae234"
        color11 "#fce94f"
        color12 "#729fcf"
        color13 "#ad7fa8"
        color14 "#34e2e2"
        color15 "#eeeeec")

  (setq ansi-color-black        color0
        ansi-color-bold-black   color8
        ansi-color-red          color1
        ansi-color-bold-red     color9
        ansi-color-green        color2
        ansi-color-bold-green   color10
        ansi-color-yellow       color3
        ansi-color-bold-yellow  color11
        ansi-color-blue         color4
        ansi-color-bold-blue    color12
        ansi-color-magenta      color5
        ansi-color-bold-magenta color13
        ansi-color-cyan         color6
        ansi-color-bold-cyan    color14
        ansi-color-white        color7
        ansi-color-bold-white   color15)

  (setq ansi-color-names-vector
        (vector ansi-color-black
                ansi-color-red
                ansi-color-green
                ansi-color-yellow
                ansi-color-blue
                ansi-color-magenta
                ansi-color-cyan
                ansi-color-white))

  (setq ansi-color-bold-colors
        `((,ansi-color-black   . ,ansi-color-bold-black  )
          (,ansi-color-red     . ,ansi-color-bold-red    )
          (,ansi-color-green   . ,ansi-color-bold-green  )
          (,ansi-color-yellow  . ,ansi-color-bold-yellow )
          (,ansi-color-blue    . ,ansi-color-bold-blue   )
          (,ansi-color-magenta . ,ansi-color-bold-magenta)
          (,ansi-color-cyan    . ,ansi-color-bold-cyan   )
          (,ansi-color-white   . ,ansi-color-bold-white  )))

  (add-hook 'comint-output-filter-functions 'filter-non-sgr-control-sequences-in-output)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'shell-mode-hook 'ansi-color-generate-color-map)
  (add-hook 'shell-mode-hook 'turn-on-directory-tracking))

(provide 'shellfix)
