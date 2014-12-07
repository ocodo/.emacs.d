fingers-mode is a global minor mode that introduces key bindings for
navigation and text manipulation. It relies on modal editing to reduce the
usage of modifiers like Control and Meta. It introduces a new keymap to
trigger commands without the need for modifiers and can easily be toggled to
fall back to inserting text as usually in Emacs.

More information: https://github.com/fgeller/fingers.el/

(require 'thingatpt)


Helpers for bindings


(defvar fingers-keyboard-layout-mapper 'identity "Mapping function from Workman to a different keyboard layout")
(defvar fingers-region-specifiers
  '((char . ?v)
    (char-and-whitespace . ?V)
    (line . ?g)
    (line-rest . ?G)
    (word . ?h)
    (word-and-whitespace . ?H)
    (symbol . ?t)
    (symbol-and-whitespace . ?T)
    (between-whitespace . ?c)
    (with-surrounding-whitespace . ?C)
    (inside-pair . ?s)
    (with-pair . ?a)
    (with-pair-and-whitespace . ?A))
  "Mapping from region type to identifier key")

(defun fingers-region-specifier (type)
  (cdr (assoc type fingers-region-specifiers)))

(defun fingers-pass-events (kbd-string)
  "Helper to pass keyboard events through to shadowed maps. Based on `boon-push-events'"
  (setq unread-command-events
        (append (kbd kbd-string) unread-command-events)))

(defmacro fingers-pass-events-command (kbd-string)
  `(lambda ()
     (interactive)
     (fingers-pass-events ,kbd-string)))

(defun fingers-clear-keymap (keymap)
  (let (loop)
    (setq loop 0)
    (while (<= loop ?z)
      (define-key map (char-to-string loop) nil)
      (setq loop (1+ loop)))))

(defun fingers-define-keys (layout-mapper map bindings)
  "Defines bindings in MAP as defined in BINDINGS"
  (fingers-clear-keymap map)
  (dolist (binding bindings)
    (let* ((key (cond ((symbolp (car binding)) (symbol-name (car binding)))
		      ((numberp (car binding)) (number-to-string (car binding)))
		      (t (error (format "unexpected key: %s" (car binding))))))
	   (target (cdr binding))
	   (mapped-sequence (funcall layout-mapper key)))
      (message "Defining binding for [%s] to target [%s]" mapped-sequence target)
      (define-key map (kbd mapped-sequence) target))))

(defun fingers-meta ()
  (interactive)
  (let* ((next-key (read-char "M-"))
	 (next-key-sequence (concat "M-" (string next-key))))
    (fingers-pass-events next-key-sequence)))

(defun fingers-meta-control ()
  (interactive)
  (let* ((next-key (read-char "C-M-"))
	 (next-key-sequence (concat "C-M-" (string next-key))))
    (fingers-pass-events next-key-sequence)))


Helpers for navigation

(defun fingers-move-to-next-word-occurrence ()
  (interactive)
  (fingers-beginning-of-word)
  (forward-word)
  (search-forward-regexp (concat "\\<" (regexp-quote (thing-at-point 'word)) "\\>"))
  (fingers-beginning-of-word))

(defun fingers-move-to-next-symbol-occurrence ()
  (interactive)
  (fingers-beginning-of-symbol)
  (forward-symbol 1)
  (search-forward-regexp (concat "\\_<" (regexp-quote (thing-at-point 'symbol)) "\\_>"))
  (fingers-beginning-of-symbol))

(defun fingers-move-to-previous-word-occurrence ()
  (interactive)
  (fingers-beginning-of-word)
  (search-backward-regexp (concat "\\<" (regexp-quote (thing-at-point 'word)) "\\>")))

(defun fingers-move-to-previous-symbol-occurrence ()
  (interactive)
  (fingers-beginning-of-symbol)
  (search-backward-regexp (concat "\\_<" (regexp-quote (thing-at-point 'symbol)) "\\_>")))


Helpers for manipulation

(defun fingers-open-line-below ()
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

(defun fingers-replace-char ()
  (interactive)
  (let ((char-to-insert (read-char "Replace with: ")))
    (delete-char 1)
    (insert char-to-insert)
    (backward-char 1)))

(defun fingers-copy-current-region (&optional kill)
  (cond (kill (kill-region (point) (mark)))
	(t (kill-ring-save (point) (mark)))))

(defun fingers-duplicate-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (kill-ring-save (point) (mark))
    (open-line 1)
    (forward-char 1)
    (yank)))

(defun fingers-find-next-left-pair-start ()
  (save-excursion
    (search-backward-regexp "\\((\\|{\\|\\[\\|<\\|'\\|\"\\|`\\)" (point-min) t)
    (let ((start-char (buffer-substring-no-properties (point) (1+ (point)))))
      (cond ((string= "(" start-char) '("(" . ")"))
	    ((string= "[" start-char) '("[" . "]"))
	    ((string= "{" start-char) '("{" . "}"))
	    ((string= "<" start-char) '("<" . ">"))
	    ((string= "'" start-char) '("'" . "'"))
	    ((string= "\"" start-char) '("\"" . "\""))
	    ((string= "`" start-char) '("`" . "`"))))))

(defun fingers-dispatch-with-pair (target &optional default)
  (let* ((last-key-seq (this-single-command-keys))
	 (last-key (elt last-key-seq (1- (length last-key-seq))))
	 (next-key (read-char "Pair start character: "))
	 (inner-most-pair (fingers-find-next-left-pair-start)))
    (cond ((= next-key ?\() (funcall target "(" ")"))
          ((= next-key ?\{) (funcall target "{" "}"))
          ((= next-key ?\[) (funcall target "[" "]"))
          ((= next-key ?\<) (funcall target "<" ">"))
          ((= next-key ?\') (funcall target "'" "'"))
          ((= next-key ?\") (funcall target "\"" "\""))
	  ((= next-key ?\`) (funcall target "`" "`"))
	  ((and (= next-key last-key) inner-most-pair)
	   (funcall target (car inner-most-pair) (cdr inner-most-pair)))
          (t
	   (when default (funcall default))
	   (fingers-pass-events (string next-key))))))

(defun fingers-move-point-to-balanced-start (start end)
  (fingers-move-point-to-balanced t start end))

(defun fingers-move-point-to-balanced-end (start end)
  (fingers-move-point-to-balanced nil start end))

(defun fingers-move-point-to-balanced (look-for-start start end)
  (let ((counter 1))
    (while (> counter 0)
      (if look-for-start (backward-char 1) (forward-char 1))
      (cond ((looking-at (regexp-quote (if look-for-start end start))) (setq counter (1+ counter)))
            ((looking-at (regexp-quote (if look-for-start start end))) (setq counter (1- counter)))))))

(defun fingers-move-point-to-pair-start-simple (pair)
  (message "looking for simple [%s]" pair)
  (backward-char 1)
  (while (not (looking-at (regexp-quote pair)))
    (backward-char 1)))

(defun fingers-move-point-to-pair-end-simple (pair)
  (forward-char 1)
  (while (not (looking-at (regexp-quote pair)))
    (forward-char 1)))

(defun fingers-move-point-to-pair-starting-string (start end)
  (if (string= start end)
      (fingers-move-point-to-pair-start-simple start)
    (fingers-move-point-to-balanced-start start end)))

(defun fingers-move-point-to-pair-ending-string (start end)
  (if (string= start end)
      (fingers-move-point-to-pair-end-simple start)
    (fingers-move-point-to-balanced-end start end)))

(defun fingers-looking-at-symbol-p ()
  (looking-at "\\_<"))

(defun fingers-beginning-of-symbol ()
  (while (not (fingers-looking-at-symbol-p))
    (left-char 1)))

(defun fingers-looking-at-word-p ()
  (looking-at "\\<"))

(defun fingers-beginning-of-word ()
  (while (not (fingers-looking-at-word-p))
    (left-char 1)))

(defun fingers-set-mark-before-whitespace-and-return ()
  (let ((start-position (point)))
    (skip-chars-backward " \t")
    (set-mark (point))
    (goto-char start-position)))

(defun fingers-skip-whitespace-forward ()
  (skip-chars-forward " \t\n"))

(defun fingers-skip-whitespace-backward ()
  (skip-chars-backward " \t\n"))


mark


(defun fingers-mark ()
  (interactive)
  (let ((next-key (read-char "Mark: ")))
    (cond
     ((= next-key (fingers-region-specifier 'char)) (fingers-mark-char))
     ((= next-key (fingers-region-specifier 'char-and-whitespace)) (fingers-mark-char-and-whitespace))
     ((= next-key (fingers-region-specifier 'line)) (fingers-mark-whole-line))
     ((= next-key (fingers-region-specifier 'line-rest)) (fingers-mark-until-end-of-line))
     ((= next-key (fingers-region-specifier 'word)) (fingers-mark-word))
     ((= next-key (fingers-region-specifier 'word-and-whitespace)) (fingers-mark-word-and-whitespace))
     ((= next-key (fingers-region-specifier 'symbol)) (fingers-mark-symbol))
     ((= next-key (fingers-region-specifier 'symbol-and-whitespace)) (fingers-mark-symbol-and-whitespace))
     ((= next-key (fingers-region-specifier 'between-whitespace)) (fingers-mark-between-whitespace))
     ((= next-key (fingers-region-specifier 'with-surrounding-whitespace)) (fingers-mark-with-surrounding-whitespace))
     ((= next-key (fingers-region-specifier 'inside-pair)) (fingers-mark-inside-pair))
     ((= next-key (fingers-region-specifier 'with-pair)) (fingers-mark-with-pair))
     ((= next-key (fingers-region-specifier 'with-pair-and-whitespace)) (fingers-mark-with-pair-and-whitespace))
     (t (set-mark (point))
	(fingers-pass-events (string next-key))))))

(defun fingers-mark-char ()
  (set-mark (point))
  (forward-char 1))

(defun fingers-mark-char-and-whitespace ()
  (fingers-set-mark-before-whitespace-and-return)
  (forward-char 1)
  (fingers-skip-whitespace-forward))

(defun fingers-mark-word ()
  (unless (fingers-looking-at-word-p) (fingers-beginning-of-word))
  (set-mark (point))
  (forward-word))

(defun fingers-mark-word-and-whitespace ()
  (unless (fingers-looking-at-word-p) (fingers-beginning-of-word))
  (fingers-set-mark-before-whitespace-and-return)
  (forward-word)
  (fingers-skip-whitespace-forward))

(defun fingers-mark-symbol ()
  (unless (fingers-looking-at-symbol-p) (fingers-beginning-of-symbol))
  (set-mark (point))
  (forward-symbol 1))

(defun fingers-mark-symbol-and-whitespace ()
  (unless (fingers-looking-at-symbol-p) (fingers-beginning-of-symbol))
  (fingers-set-mark-before-whitespace-and-return)
  (forward-symbol 1)
  (fingers-skip-whitespace-forward))

(defun fingers-mark-between-whitespace ()
  (search-backward-regexp "[ \t\n]" (point-min) t)
  (fingers-skip-whitespace-forward)
  (set-mark (point))
  (search-forward-regexp "[ \t\n]" (point-max) t)
  (fingers-skip-whitespace-backward))

(defun fingers-mark-with-surrounding-whitespace ()
  (search-backward-regexp "[ \t\n]" (point-min) t)
  (let ((non-whitespace-position (save-excursion
				   (fingers-skip-whitespace-forward)
				   (point))))
    (fingers-skip-whitespace-backward)
    (set-mark (point))
    (goto-char non-whitespace-position)
    (search-forward-regexp "[ \t\n]" (point-max) t)
    (fingers-skip-whitespace-forward)))

(defun fingers-mark-until-end-of-line ()
  (set-mark (point))
  (end-of-line))

(defun fingers-mark-whole-line ()
  (beginning-of-line)
  (set-mark (point))
  (end-of-line))

(defun fingers-mark-inside-pair ()
  (fingers-dispatch-with-pair 'fingers-mark-inside-pair-strings
                              (lambda () (set-mark (point)))))

(defun fingers-mark-inside-pair-strings (start end)
  (fingers-move-point-to-pair-starting-string start end)
  (forward-char 1)
  (set-mark (point))
  (backward-char 1)
  (fingers-move-point-to-pair-ending-string start end))

(defun fingers-mark-with-pair ()
  (fingers-dispatch-with-pair 'fingers-mark-with-pair-strings))

(defun fingers-mark-with-pair-strings (start end)
  (fingers-move-point-to-pair-starting-string start end)
  (set-mark (point))
  (fingers-move-point-to-pair-ending-string start end)
  (forward-char 1))

(defun fingers-mark-with-pair-strings-and-whitespace (start end)
  (fingers-move-point-to-pair-starting-string start end)
  (let ((starting-position (point)))
    (skip-chars-backward " \t")
    (set-mark (point))
    (goto-char starting-position))
  (fingers-move-point-to-pair-ending-string start end)
  (forward-char 1)
  (skip-chars-forward " \t"))

(defun fingers-mark-with-pair-and-whitespace ()
  (fingers-dispatch-with-pair 'fingers-mark-with-pair-strings-and-whitespace))


kill & copy


(defun fingers-kill ()
  (interactive)
  (fingers-copy 'kill))

(defun fingers-copy (&optional kill)
  (interactive)
  (cond ((region-active-p) (fingers-copy-current-region kill))
	(t (let ((next-key (read-char "Kill: ")))
	     (cond
	      ((= next-key (fingers-region-specifier 'char)) (fingers-copy-char kill))
	      ((= next-key (fingers-region-specifier 'char-and-whitespace)) (fingers-copy-char-and-whitespace kill))
	      ((= next-key (fingers-region-specifier 'line)) (fingers-copy-whole-line kill))
	      ((= next-key (fingers-region-specifier 'line-rest)) (fingers-copy-until-end-of-line kill))
	      ((= next-key (fingers-region-specifier 'word)) (fingers-copy-word kill))
	      ((= next-key (fingers-region-specifier 'word-and-whitespace)) (fingers-copy-word-and-whitespace kill))
	      ((= next-key (fingers-region-specifier 'symbol)) (fingers-copy-symbol kill))
	      ((= next-key (fingers-region-specifier 'symbol-and-whitespace)) (fingers-copy-symbol-and-whitespace kill))
	      ((= next-key (fingers-region-specifier 'between-whitespace)) (fingers-copy-between-whitespace kill))
	      ((= next-key (fingers-region-specifier 'with-surrounding-whitespace)) (fingers-copy-with-surrounding-whitespace kill))
	      ((= next-key (fingers-region-specifier 'inside-pair)) (fingers-copy-inside-pair kill))
	      ((= next-key (fingers-region-specifier 'with-pair)) (fingers-copy-with-pair kill))
	      ((= next-key (fingers-region-specifier 'with-pair-and-whitespace)) (fingers-copy-with-pair-and-whitespace kill))
	      (t (set-mark (point))
		 (call-interactively (key-binding (kbd (string next-key))))
		 (fingers-copy-current-region kill)))))))

(defun fingers-copy-char (&optional kill)
  (fingers-mark-char)
  (fingers-copy-current-region kill))

(defun fingers-copy-char-and-whitespace (&optional kill)
  (fingers-mark-char-and-whitespace)
  (fingers-copy-current-region kill))

(defun fingers-copy-word (&optional kill)
  (fingers-mark-word)
  (fingers-copy-current-region kill))

(defun fingers-copy-word-and-whitespace (&optional kill)
  (fingers-mark-word-and-whitespace)
  (fingers-copy-current-region kill))

(defun fingers-copy-symbol (&optional kill)
  (fingers-mark-symbol)
  (fingers-copy-current-region kill))

(defun fingers-copy-symbol-and-whitespace (&optional kill)
  (fingers-mark-symbol-and-whitespace)
  (fingers-copy-current-region kill))

(defun fingers-copy-between-whitespace (&optional kill)
  (fingers-mark-between-whitespace)
  (fingers-copy-current-region kill))

(defun fingers-copy-with-surrounding-whitespace (&optional kill)
  (fingers-mark-with-surrounding-whitespace)
  (fingers-copy-current-region kill))

(defun fingers-copy-until-end-of-line (&optional kill)
  (fingers-mark-until-end-of-line)
  (fingers-copy-current-region kill))

(defun fingers-copy-whole-line (&optional kill)
  (fingers-mark-whole-line)
  (fingers-copy-current-region kill)
  (delete-char 1))

(defun fingers-copy-inside-pair (&optional kill)
  (fingers-mark-inside-pair)
  (fingers-copy-current-region kill))

(defun fingers-copy-with-pair (&optional kill)
  (fingers-mark-with-pair)
  (fingers-copy-current-region kill))

(defun fingers-copy-with-pair-and-whitespace (&optional kill)
  (fingers-mark-with-pair-and-whitespace)
  (fingers-copy-current-region kill))


enclose


(defun fingers-enclose-in-pair ()
  (interactive)
  (unless (region-active-p) (fingers-mark))
  (fingers-dispatch-with-pair 'fingers-enclose-in-pair-strings))

(defun fingers-enclose-in-pair-strings (start end)
  (let* ((mark-position (mark))
         (point-position (point))
         (start-position (min mark-position point-position))
         (end-position (max mark-position point-position)))
    (goto-char end-position)
    (insert end)
    (goto-char start-position)
    (insert start)
    (goto-char (+ end-position (length end)))))


remove enclosing pair


(defun fingers-remove-enclosing-pair ()
  (interactive)
  (fingers-dispatch-with-pair 'fingers-remove-enclosing-pair-strings))

(defun fingers-remove-enclosing-pair-strings (start end)
  (fingers-mark-inside-pair-strings start end)
  (let ((start-position (mark)))
    (delete-char (length end))
    (goto-char start-position)
    (delete-char (- (length start)))))


Keymaps


(defun fingers-mode-clean-map ()
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    map))

(defvar fingers-mode-map (fingers-mode-clean-map))
(defvar fingers-mode-x-map (fingers-mode-clean-map))
(defvar fingers-mode-c-map (fingers-mode-clean-map))

(defvar fingers-command-bindings
    `(
      ;; left hand -- manipulation
      ;;
      ;; q d r w b
      ;; a s h t g
      ;; z x m c v

      ;; top row
      (d . fingers-duplicate-line)
      (r . query-replace)
      (R . query-replace-regexp)
      (w . join-line)
      (b . open-line)

      ;; home row
      (a . fingers-enclose-in-pair)
      (s . fingers-remove-enclosing-pair)
      (h . yank)
      (H . yank-pop)
      (t . fingers-kill)
      (T . fingers-copy)
      (g . fingers-meta)
      (G . fingers-meta-control)

      ;; bottom row
      (z . repeat)
      (x . ,fingers-mode-x-map)
      (m . kmacro-start-macro)
      (M . kmacro-end-macro)
      (c . ,fingers-mode-c-map)
      (v . fingers-replace-char)

      ;; right hand -- navigation
      ;;
      ;; j f u p ; [
      ;; y n e o i '
      ;; k l , . /

      ;; top row
      (j . apropos)
      (J . ,(fingers-pass-events-command "C-h"))
      (fn . point-to-register)
      (ff . jump-to-register)
      (ue . isearch-forward)
      (uu . isearch-repeat-forward)
      (uh . fingers-move-to-next-word-occurrence)
      (ut . fingers-move-to-next-symbol-occurrence)
      (uo . isearch-occur)
      (po . isearch-backward)
      (pp . isearch-repeat-backward)
      (ph . fingers-move-to-previous-word-occurrence)
      (pt . fingers-move-to-previous-symbol-occurrence)

      ;; home row
      (y . beginning-of-line)
      (Y . beginning-of-buffer)
      (n . left-char)
      (N . backward-word)
      (e . next-line)
      (E . scroll-up-command)
      (o . previous-line)
      (O . scroll-down-command)
      (i . right-char)
      (I . forward-word)
      (,(intern "'") . end-of-line)
      (,(intern "\"") . end-of-buffer)

      ;; bottom row
      (k . grep)
      (/ . undo)

      (SPC . fingers-mark)
      )
    "Main bindings in `fingers-mode-map'")

(defvar fingers-x-bindings
  `(
    (b . switch-to-buffer)
    (c . save-buffers-kill-terminal)
    (e . eval-last-sexp)
    (f . find-file)
    (h . mark-whole-buffer)
    (k . kill-buffer)
    (me . eshell)
    (o . other-window)
    (s . ,(fingers-pass-events-command "C-x C-s"))
    (S . save-some-buffers)
    (vd . vc-diff)
    (vD . vc-root-diff)
    (ve . vc-version-ediff)
    (vg . vc-annotate)
    (vl . vc-print-log)
    (vu . vc-revert)
    (v~ . vc-revision-other-window)
    (x . execute-extended-command)
    (0 . delete-window)
    (1 . delete-other-windows)
    (2 . split-window-below)
    (3 . split-window-right)
    (50 . delete-frame)
    (52 . make-frame-command)
    )
  "Bindings for `fingers-mode-x-map'")

(defvar fingers-c-bindings
  `(
    (b . ,(fingers-pass-events-command "C-c C-b"))
    (c . ,(fingers-pass-events-command "C-c C-c"))
    (f . ,(fingers-pass-events-command "C-c C-f"))
    (k . ,(fingers-pass-events-command "C-c C-k"))
    (l . ,(fingers-pass-events-command "C-c C-l"))
    (p . ,(fingers-pass-events-command "C-c C-p"))
    (q . ,(fingers-pass-events-command "C-c C-q"))
    (s . ,(fingers-pass-events-command "C-c C-s"))
    (t . ,(fingers-pass-events-command "C-c C-t"))
    (xa . ,(fingers-pass-events-command "C-c C-x C-a"))
    (! . ,(fingers-pass-events-command "C-c !"))
    (,(intern "'") . ,(fingers-pass-events-command "C-c '"))
    )
  "Bindings for `fingers-mode-c-map'")


Main command mode map

(defun fingers-reset-bindings ()
  (fingers-define-keys fingers-keyboard-layout-mapper
		       fingers-mode-map
		       fingers-command-bindings)
  (fingers-define-keys 'identity
		       fingers-mode-x-map
		       fingers-x-bindings)
  (fingers-define-keys 'identity
		       fingers-mode-c-map
		       fingers-c-bindings))

(fingers-reset-bindings)


Mode management

(defvar fingers-mode-excluded-major-modes '()
  "List of major-modes for which fingers-mode should not be activated.")

(defun fingers-mode-maybe-activate ()
  (unless (or (minibufferp)
	      (member major-mode fingers-mode-excluded-major-modes))
    (fingers-mode 1)))

###autoload
(define-minor-mode fingers-mode
  "Minor mode that introduces key bindings for navigation and
text manipulation. It relies on modal editing to reduce the usage
of modifiers like Control and Meta. It introduces a new keymap to
trigger commands without the need for modifiers and can easily be
toggled to fall back to inserting text as usually in Emacs.

Available bindings:

\\{fingers-mode-map}
The x prefix has the following bindings:

\\{fingers-mode-x-map}
The c prefix has the following bindings:

\\{fingers-mode-c-map}
"
  nil " fingers" fingers-mode-map :group 'fingers)

###autoload
(define-globalized-minor-mode global-fingers-mode
  fingers-mode
  fingers-mode-maybe-activate
  :group 'fingers)

(provide 'fingers)

fingers.el ends here
