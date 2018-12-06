;;; -*- lexical-binding: t -*-

;;;
;;; Keymap buffer
;;;

(require 'cl)
(require 'gnu-apl-util)
(require 'gnu-apl-network)


(defcustom gnu-apl-keyboard-simplified-mouse-action-mode t
  "Defines the action to be performed on mouse over the symbol in
keyboard help. Possible variants:
nil - tooltip shows help on possible actions,
mouse 1 to open help window, mouse 3 to insert symbol
t - inspired by Dyalog APL IDE toolbar, tooltip shows symbol
help, mouse 1 to insert symbol, mouse 2 to open help window"
  :type 'boolean
  :group 'gnu-apl)


(defvar *gnu-apl-keymap-buffer-name* "*gnu-apl keymap*")

(defvar gnu-apl-keymap-template
  "╔════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦═════════╗
║ ~∇ ║ !∇ ║ @∇ ║ #∇ ║ $∇ ║ %∇ ║ ^∇ ║ &∇ ║ *∇ ║ (∇ ║ )∇ ║ _∇ ║ +∇ ║         ║
║ `∇ ║ 1∇ ║ 2∇ ║ 3∇ ║ 4∇ ║ 5∇ ║ 6∇ ║ 7∇ ║ 8∇ ║ 9∇ ║ 0∇ ║ -∇ ║ =∇ ║ BACKSP  ║
╠════╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦══════╣
║       ║ Q∇ ║ W∇ ║ E∇ ║ R∇ ║ T∇ ║ Y∇ ║ U∇ ║ I∇ ║ O∇ ║ P∇ ║ {∇ ║ }∇ ║  |∇  ║
║  TAB  ║ q∇ ║ w∇ ║ e∇ ║ r∇ ║ t∇ ║ y∇ ║ u∇ ║ i∇ ║ o∇ ║ p∇ ║ [∇ ║ ]∇ ║  \\∇  ║
╠═══════╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩══════╣
║ (CAPS   ║ A∇ ║ S∇ ║ D∇ ║ F∇ ║ G∇ ║ H∇ ║ J∇ ║ K∇ ║ L∇ ║ :∇ ║ \"∇ ║         ║
║  LOCK)  ║ a∇ ║ s∇ ║ d∇ ║ f∇ ║ g∇ ║ h∇ ║ j∇ ║ k∇ ║ l∇ ║ ;∇ ║ '∇ ║ RETURN  ║
╠═════════╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═════════╣
║             ║ Z∇ ║ X∇ ║ C∇ ║ V∇ ║ B∇ ║ N∇ ║ M∇ ║ <∇ ║ >∇ ║ ?∇ ║          ║
║  SHIFT      ║ z∇ ║ x∇ ║ c∇ ║ v∇ ║ b∇ ║ n∇ ║ m∇ ║ ,∇ ║ .∇ ║ /∇ ║  SHIFT   ║
╚═════════════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩══════════╝"
  "APL keyboard layout template. It is based on
GNU APL keyboard layout: http://commons.wikimedia.org/wiki/File:GNU_APL_keyboard_layout.png
This variable could be redefined to match another physical layout.
In order for changes to take effect the buffer needs to be recreated.")



(defun gnu-apl-keymap-mode-kill-buffer ()
  "Close the buffer displaying the keymap."
  (interactive)
  (let ((buffer (get-buffer *gnu-apl-keymap-buffer-name*)))
    (when buffer
      (delete-windows-on buffer)
      (kill-buffer buffer))))

(defvar gnu-apl-keymap-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'gnu-apl-keymap-mode-kill-buffer)
    map)
  "Keymap for keymap mode buffers")

(define-derived-mode gnu-apl-keymap-mode fundamental-mode "GNU-APL-Keymap"
  "Major mode for displaying the keymap help."
  (use-local-map gnu-apl-keymap-mode-map)
  (read-only-mode 1)
  (setq truncate-lines t))

(defun gnu-apl--find-function-content (name)
  (let* ((content (gnu-apl--send-network-command-and-read (format "fn:%s" name)))
         (result (car content)))
    (cond ((string= result "function-content")
           (cdr content))
          ((string= result "undefined")
           nil)
          ((string= result "symbol is not a function")
           nil)
          (t
           (error "Error getting function: %s" (car content))))))

(defun gnu-apl--remove-leading-space (string)
  (if (and (plusp (length string)) (eql (aref string 0) (aref " " 0)))
      (subseq string 1)
    string))

(defun gnu-apl--find-documentation-for-defined-function (name)
  (let ((content (gnu-apl--find-function-content name)))
    (when content
      (let ((header (car content))
            (lines (cdr content)))
        (list header
              (loop for row in lines
                    for trim-row = (gnu-apl--trim-spaces row)
                    while (and (>= (length trim-row) 2) (string= (subseq trim-row 0 2) "⍝⍝"))
                    collect (gnu-apl--remove-leading-space (subseq trim-row 2))))))))

(defun gnu-apl--get-doc-for-symbol (string)
  (loop for e in gnu-apl--symbol-doc
        for name = (car e)
        when (or (and (stringp name)
                      (string= string name))
                 (and (listp name)
                      (cl-find string name :test #'string=)))
        return e
        finally (return nil)))

(defun gnu-apl--get-full-docstring-for-native-symbol (string full-text-p)
  (let ((doc (gnu-apl--get-doc-for-symbol string))
        (format-short 
         (if full-text-p "\n%s\n\n" "\n%s\n")))
    (when doc
      (with-temp-buffer
        (loop for e in (second doc)
              for first = t then nil
              unless first
              do (insert "\n")
              do (progn
                   (insert (format "%s: %s" (first e) (second e)))
                   (insert (format format-short (third e)))
                   (let ((long (fourth e)))
                     (when long
                       (insert (format "%s\n" long)))))
              when full-text-p
              do (insert "\n===================================\n"))
        (buffer-string)))))


(defun gnu-apl--remove-local-variable-name (name)
  (let ((pos (position ?\; name)))
    (if pos
        (gnu-apl--trim-spaces (subseq name 0 pos))
      name)))

(defun gnu-apl--get-full-docstring-for-function-symbol (string)
  (let ((content (gnu-apl--find-documentation-for-defined-function string)))
    (when content
      (with-temp-buffer
        (insert (format "Function: %s\n\n" (gnu-apl--remove-local-variable-name (car content))))
        (loop for row in (cadr content)
              for first = t then nil
              unless first do (insert "\n")
              do (insert row))
        (buffer-string)))))

(defun gnu-apl--get-full-docstring-for-symbol (string full-text-p)
  "Get the documentation for the symbol or function STRING.
When FULL-TEXT is t format the output string suitable for separate
buffer. Otherwise try to make it short to fit into the tooltip."
  (or (gnu-apl--get-full-docstring-for-native-symbol string full-text-p)
      (gnu-apl--get-full-docstring-for-function-symbol string)))

(defvar *gnu-apl-documentation-buffer-name* "*gnu-apl documentation*")

(defun gnu-apl-close-documentation-buffer ()
  "Closes the active documentation window"
  (interactive)
  (quit-window))

(defvar gnu-apl-documentation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'gnu-apl-close-documentation-buffer)
    map)
  "Keymap for keymap mode buffers")

(define-derived-mode gnu-apl-documentation-mode fundamental-mode "GNU-APL-Documentation"
  "Major mode for displaying GNU APL documentation"
  (use-local-map gnu-apl-documentation-mode-map))

(defun gnu-apl--name-at-point ()
  (let ((symbol-chars "[a-zA-Z0-9_∆⍙¯]"))
    (if (looking-at symbol-chars)
        (buffer-substring (save-excursion (loop while (and (> (point) (point-min))
                                                           (string-match symbol-chars
                                                                         (buffer-substring (1- (point))
                                                                                           (point))))
                                                do (backward-char 1)
                                                finally (return (point))))
                          (save-excursion (loop while (< (point) (point-max))
                                                do (forward-char 1)
                                                while (looking-at symbol-chars)
                                                finally (return (point)))))
      (let ((ch (char-after (point))))
        (when (and ch
                   (member (char-to-string ch)
                           (mapcan #'(lambda (v) (let ((m (car v)))
                                                   (if (listp m) (copy-seq m) (list m))))
                                   gnu-apl--symbol-doc)))
          (char-to-string ch))))))

(defun gnu-apl-show-help-for-symbol (symbol)
  "Open the help window for SYMBOL."
  (interactive (list (let ((default-sym (gnu-apl--name-at-point)))
                       (read-string (if default-sym
                                        (format "Symbol (default '%s'): " default-sym)
                                      "Symbol: ")
                                    nil nil default-sym t))))
  (when (or (null symbol) (string= symbol ""))
    (error "Symbol is empty"))
  (let ((string (gnu-apl--get-full-docstring-for-symbol symbol t)))
    (unless string
      (user-error "No documentation available for %s" symbol))
    (let ((buffer (get-buffer-create *gnu-apl-documentation-buffer-name*)))
      (with-current-buffer buffer
        (read-only-mode 0)
        (delete-region (point-min) (point-max))
        (insert string)
        (goto-char (point-min))
        (add-text-properties (point-min) (point-max) '(face gnu-apl-help))
        (gnu-apl-documentation-mode)
        (read-only-mode 1))
      (pop-to-buffer buffer))))

(defun gnu-apl--make-clickable (string keymap)
  (let ((help-echo-string (concat "mouse-1: Show documentation for " string "\n"
                                  "mouse-3: Insert " string " in GNU APL buffer"))
        (description
         (gnu-apl--get-full-docstring-for-symbol string
                                                 (not gnu-apl-keyboard-simplified-mouse-action-mode))))
    (cond ((and gnu-apl-keyboard-simplified-mouse-action-mode
                description)
           (setf help-echo-string description))           
          (gnu-apl-keyboard-simplified-mouse-action-mode
           (setf help-echo-string "No documentation available")))
  (propertize string
              'mouse-face 'highlight
              'help-echo help-echo-string
              'gnu-apl-insert string
              'keymap keymap
              )))

(defun gnu-apl-mouse-insert-from-keymap (event)
  "In the keymap buffer, insert the symbol that was clicked."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (unless (windowp window)
      (error "Can't find window"))
    (let* ((string (with-current-buffer (window-buffer window)
                     (get-text-property pos 'gnu-apl-insert)))
           (session (gnu-apl--get-interactive-session))
           (interactive-session-windows
            (get-buffer-window-list session nil 'visible)))
      (with-current-buffer session
        (insert string))
      ;; after we have inserted the special character,
      ;; it is reasonable to switch focus back to the interactive
      ;; APL session to continue typing.
      ;; NOTE: if there are more than 1 visible windows
      ;; with the same interactive session, the first
      ;; one will be activated.
      (when interactive-session-windows
        (select-window (car interactive-session-windows))
        ;; advance point after the inserted string
        (goto-char (+ (point) (length string)))))))

(defun gnu-apl-symbol-insert-from-keymap ()
  "Send a symbol from the keymap buffer to the current GNU APL interpreter."
  (interactive)
  (let ((string (get-text-property (point) 'gnu-apl-insert))
        (session (gnu-apl--get-interactive-session)))
    (with-current-buffer session
      (insert string))))

(defun gnu-apl-mouse-help-from-keymap (event)
  "In the keymap buffer, describe the symbol that was clicked."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (unless (windowp window)
      (error "Can't find window"))
    (let ((string (with-current-buffer (window-buffer window)
                    (get-text-property pos 'gnu-apl-insert))))
      (gnu-apl-show-help-for-symbol string))))

(defun gnu-apl-symbol-help-from-keymap ()
  "Describe a symbol in the keymap buffer."
  (interactive)
  (let ((string (get-text-property (point) 'gnu-apl-insert)))
    (gnu-apl-show-help-for-symbol string)))

(defun gnu-apl--make-help-property-keymap ()
  (let ((map (make-sparse-keymap)))  
    (cond (gnu-apl-keyboard-simplified-mouse-action-mode
           (define-key map [mouse-1] 'gnu-apl-mouse-insert-from-keymap)
           (define-key map [down-mouse-2] 'gnu-apl-mouse-help-from-keymap))
          (t
           (define-key map [down-mouse-1] 'gnu-apl-mouse-help-from-keymap)
           (define-key map [mouse-3] 'gnu-apl-mouse-insert-from-keymap)))
    (define-key map (kbd "?") 'gnu-apl-symbol-help-from-keymap)
    (define-key map (kbd "RET") 'gnu-apl-symbol-insert-from-keymap)
    map))

(defun gnu-apl--make-readable-keymap ()
  ;; Ensure that the buffer is recreated
  (let ((old-buffer (get-buffer *gnu-apl-keymap-buffer-name*)))
    (when old-buffer
      (kill-buffer old-buffer)))
  ;; Recreate the buffer according to the active keymap.
  (let ((buffer (get-buffer-create *gnu-apl-keymap-buffer-name*))
        (keymap (gnu-apl--make-help-property-keymap)))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      (insert gnu-apl-keymap-template)
      (goto-char (point-min))
      (while (search-forward-regexp "\\(.\\)∇" nil t)
        (let* ((key (match-string 1))
               (found (cl-find key gnu-apl--symbols :key #'third :test #'equal))
               (found-nonspecial (cl-find key gnu-apl--symbol-doc :key #'first :test #'equal))
               (result-string (if found (save-match-data (gnu-apl--make-clickable (second found) keymap)) " "))
               (nonspecial-string (if found-nonspecial (gnu-apl--make-clickable key keymap) key)))
          (replace-match (concat nonspecial-string result-string) t t)))
      (add-text-properties (point-min) (point-max) (list 'face 'gnu-apl-kbd-help-screen))
      (gnu-apl-keymap-mode))
    buffer))

(defun gnu-apl-show-keyboard (&optional arg)
  "When arg is nil, toggle the display of the keyboard help.
If positive, always show the buffer, if negative close the buffer
if it is open."
  (interactive "P")
  (let ((keyboard-help (get-buffer *gnu-apl-keymap-buffer-name*)))
    (if (and keyboard-help (get-buffer-window keyboard-help))
        ;; The buffer is displayed. Maybe close it.
        (when (or (null arg) (minusp arg))
          (gnu-apl-keymap-mode-kill-buffer))
      ;; The buffer is not displayed, check if it's supposed to be displayed
      (when (or (null arg) (plusp arg))
        (let* ((buffer (or (when nil ; Make sure the buffer is always created
                             (get-buffer *gnu-apl-keymap-buffer-name*))
                           (gnu-apl--make-readable-keymap)))
               (window (split-window nil)))
          (set-window-buffer window buffer)
          (fit-window-to-buffer window))))))

(defvar gnu-apl--function-regexp
  (regexp-opt (mapcan #'(lambda (v)
                          (let ((name (car v)))
                            (if (listp name)
                                (copy-seq name)
                              (list name))))
                      gnu-apl--symbol-doc)))

;;;
;;;  Eldoc integration
;;;

(defun gnu-apl--is-point-on-argument-value ()
  (save-excursion
    (if (> (point) (point-min))
        ;; There is stuff to the left of point, check what that stuff is
        (progn
          (backward-char 1)
          (loop while (and (> (point) (point-min))
                           (cl-find (char-after (point)) " \t"))
                do (backward-char 1))
          (let ((symbol (char-after (point))))
            (and (not (string-match gnu-apl--function-regexp (char-to-string symbol)))
                 (not (cl-find symbol " \t\n[(")))))
      ;; No stuff to the left of point, that means the function is monadic
      nil)))

(defun gnu-apl--eldoc-data ()
  (if (looking-at (concat "\\(" gnu-apl--function-regexp "\\)"))
      ;; The cursor is placed on a built-in function
      (let* ((symbol (match-string 1))
             (doc (gnu-apl--get-doc-for-symbol symbol)))
        (unless doc
          (error "doc should not be null"))
        ;; We have a documentation entry. Now we need to figure out if the call
        ;; is monadic or dyadic. It can be done by searching backwards until we hit
        ;; a non-space character or the beginning of the line.
        (let ((p (cl-find (if (gnu-apl--is-point-on-argument-value) "Dyadic" "Monadic") (second doc)
                          :key #'car :test #'string=)))
          (when p
            (format "%s: %s: %s" (first p) (second p) (third p)))))
    ;; ELSE: We're not on a built-in function, check if we're on a user-defined function
    (gnu-apl--when-let (name (gnu-apl--name-at-point))
      (gnu-apl--when-let (function-docs (gnu-apl--find-documentation-for-defined-function name))
        (when (second function-docs)
          (gnu-apl--when-let (header (gnu-apl--parse-function-header (car function-docs)))
            (format "%s: %s" header (car (second function-docs)))))))))

;;;
;;;  Help search
;;;

(defvar *gnu-apl-apropos-symbol-buffer-name* "*gnu-apl apropos symbol*")

(defvar gnu-apl-documentation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'gnu-apl-apropos-kill-buffer)
    map)
  "Keymap for keymap mode buffers")

(define-derived-mode gnu-apl-documentation-search-mode fundamental-mode "GNU-APL-Documentation"
  "Major mode for displaying GNU APL documentation search results."
  (use-local-map gnu-apl-documentation-mode-map))

(defun gnu-apl-documentation-search-kill-buffer ()
  "Close the current active documentation buffer."
  (interactive)
  (let ((buffer (get-buffer *gnu-apl-apropos-symbol-buffer-name*)))
    (when buffer
      (delete-windows-on buffer)
      (kill-buffer buffer))))

(defun gnu-apl--open-apropos-results (result)
  (let ((buffer (gnu-apl--open-new-buffer *gnu-apl-apropos-symbol-buffer-name*)))
    (with-current-buffer buffer
      (dolist (s result)
        (let* ((doc (car s))
               (symname-aliases (car doc))
               (name (if (listp symname-aliases) (car symname-aliases) symname-aliases)))
          (insert-button (cadr s)
                         'action #'(lambda (event) (gnu-apl-show-help-for-symbol name))
                         'follow-link t))
        (insert "\n"))
      (add-text-properties (point-min) (point-max) '(face gnu-apl-help))
      (gnu-apl-documentation-search-mode)
      (read-only-mode 1))
    (pop-to-buffer buffer)))

(defun gnu-apl-apropos-symbol (regexp)
  "Search for documentation symbols where the documentation matches REGEX."
  (interactive "MApropos symbol: ")
  (let ((result (loop for doc-entry in gnu-apl--symbol-doc
                      append (loop for e in (second doc-entry)
                                   when (or (and (second e) (string-match regexp (second e)))
                                            (and (third e) (string-match regexp (third e))))
                                   collect (list doc-entry
                                                 (let ((symname-aliases (first doc-entry)))
                                                   (format "%s: %s: %s: %s"
                                                           (if (listp symname-aliases)
                                                               (car symname-aliases)
                                                             symname-aliases)
                                                           (first e) (second e) (third e))))))))
    (if result
        (gnu-apl--open-apropos-results result)
      (message "No match"))))

(provide 'gnu-apl-documentation)
