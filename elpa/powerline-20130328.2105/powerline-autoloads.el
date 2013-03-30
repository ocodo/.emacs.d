;;; powerline-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (powerline-nano-theme powerline-default-theme powerline-center-theme
;;;;;;  powerline-fill powerline-raw defpowerline powerline-concat
;;;;;;  powerline-mouse powerline-hud) "powerline" "powerline.el"
;;;;;;  (20821 16024 0 0))
;;; Generated autoloads from powerline.el

(autoload 'powerline-hud "powerline" "\


\(fn FACE1 FACE2 &optional WIDTH)" nil nil)

(autoload 'powerline-mouse "powerline" "\


\(fn CLICK-GROUP CLICK-TYPE STRING)" nil nil)

(autoload 'powerline-concat "powerline" "\
Concatonate STRINGS and pad sides by spaces.

\(fn &rest STRINGS)" nil nil)

(autoload 'defpowerline "powerline" "\


\(fn NAME BODY)" nil t)

(autoload 'powerline-raw "powerline" "\


\(fn STR &optional FACE PAD)" nil nil)

(autoload 'powerline-fill "powerline" "\


\(fn FACE RESERVE)" nil nil)

(defpowerline powerline-major-mode (propertize (format-mode-line mode-name) 'mouse-face 'mode-line-highlight 'help-echo "Major mode\nmouse-1: Display major mode menu\nmouse-2: Show help for major mode\nmouse-3: Toggle minor modes" 'local-map (let ((map (make-sparse-keymap))) (define-key map [mode-line down-mouse-1] `(menu-item ,(purecopy "Menu Bar") ignore :filter (lambda (_) (mouse-menu-major-mode-map)))) (define-key map [mode-line mouse-2] 'describe-mode) (define-key map [mode-line down-mouse-3] mode-line-mode-menu) map)))

(defpowerline powerline-minor-modes (mapconcat (lambda (mm) (propertize mm 'mouse-face 'mode-line-highlight 'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes" 'local-map (let ((map (make-sparse-keymap))) (define-key map [mode-line down-mouse-1] (powerline-mouse 'minor 'menu mm)) (define-key map [mode-line mouse-2] (powerline-mouse 'minor 'help mm)) (define-key map [mode-line down-mouse-3] (powerline-mouse 'minor 'menu mm)) (define-key map [header-line down-mouse-3] (powerline-mouse 'minor 'menu mm)) map))) (split-string (format-mode-line minor-mode-alist)) " "))

(defpowerline powerline-narrow (let (real-point-min real-point-max) (save-excursion (save-restriction (widen) (setq real-point-min (point-min) real-point-max (point-max)))) (when (or (/= real-point-min (point-min)) (/= real-point-max (point-max))) (propertize "Narrow" 'mouse-face 'mode-line-highlight 'help-echo "mouse-1: Remove narrowing from the current buffer" 'local-map (make-mode-line-mouse-map 'mouse-1 'mode-line-widen)))))

(defpowerline powerline-vc (when (and (buffer-file-name (current-buffer)) vc-mode) (format-mode-line '(vc-mode vc-mode))))

(defpowerline powerline-buffer-size (propertize (if powerline-buffer-size-suffix "%I" "%i") 'mouse-face 'mode-line-highlight 'local-map (make-mode-line-mouse-map 'mouse-1 (lambda nil (interactive) (setq powerline-buffer-size-suffix (not powerline-buffer-size-suffix)) (redraw-modeline)))))

(defpowerline powerline-buffer-id (format-mode-line mode-line-buffer-identification))

(defpowerline powerline-process (cond ((listp mode-line-process) (format-mode-line mode-line-process)) (t mode-line-process)))

(defpowerline powerline-which-func (format-mode-line `(:propertize which-func-current local-map ,which-func-keymap face which-func mouse-face mode-line-highlight help-echo "mouse-1: go to beginning\nmouse-2: toggle rest visibility\nmouse-3: go to end")))

(autoload 'powerline-center-theme "powerline" "\
Setup a default mode-line with major and minor modes centered.

\(fn)" t nil)

(fset 'powerline-default-center 'powerline-center-theme)

(autoload 'powerline-default-theme "powerline" "\
Setup a default mode-line.

\(fn)" t nil)

(fset 'powerline-default 'powerline-default-theme)

(autoload 'powerline-nano-theme "powerline" "\
Setup a nano-like mode-line.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("powerline-pkg.el") (20821 16024 267374
;;;;;;  0))

;;;***

(provide 'powerline-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; powerline-autoloads.el ends here
