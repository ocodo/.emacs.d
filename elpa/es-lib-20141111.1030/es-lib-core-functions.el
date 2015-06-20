;;; es-lib-core-functions.el --- Random functions
;;; Version: 0.4
;;; Author: sabof
;;; URL: https://github.com/sabof/es-lib


;;; Commentary:

;; The project is hosted at https://github.com/sabof/es-lib
;; The latest version, and all the relevant information can be found there.

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'cl-lib)
(require 'es-lib-core-macros)

(defun es-disable-keys (map &rest keylist)
  (declare (indent 1))
  (cl-dolist (key keylist)
    (define-key map key nil)))
(put 'es-disable-keys 'common-lisp-indent-function
     '(4 &body))

;;;###autoload
(defun es-kill-buffer-dont-ask (&optional buffer)
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (setf (buffer-modified-p buffer) nil)
  (let (kill-buffer-query-functions)
    (kill-buffer buffer)))

(defun es-buffer-name-list ()
  "Will omit special and tag buffers."
  (cl-remove-if (lambda (name)
                  (string-match-p "^ \\|^tags$\\|^TAGS$" name))
                (mapcar 'buffer-name (buffer-list))))

(defun es-unsaved-buffer-list ()
  (cl-remove-if-not
   (lambda (buf)
     (and (buffer-modified-p buf)
          (buffer-file-name buf)))
   (buffer-list)))

(cl-defun es-ido-completing-read-alist (prompt alist &rest additional-ido-args)
  "Each member can also be a string"
  (require 'ido)
  (setq alist (mapcar (lambda (it) (if (consp it) it (cons it it)))
                      alist))
  (let (( selection (apply 'ido-completing-read prompt
                           (mapcar 'car alist) additional-ido-args)))
    (when selection
      (cdr (cl-find selection alist :key 'car :test 'equal)))))

(defun es-buffer-mode (buffer-or-name)
  (with-current-buffer (get-buffer buffer-or-name)
    major-mode))

(defun es-mapbuffer (function buffer-list)
  "Perform FUNCTION inside a buffer with each member of BUFFER-LIST as current.
FUNCTION does not accept arguments"
  (save-excursion
    (mapcar (lambda (buf)
              (set-buffer buf)
              (funcall function))
            buffer-list)))

(defun es-buffers-where-local-variable-is (var-sym value)
  (cl-remove-if-not (lambda (buf)
                      (with-current-buffer buf
                        (equal (symbol-value var-sym)
                               value)))
                    (buffer-list)))

(defun es-string-remove-properties (string)
  (with-temp-buffer
    (insert string)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun es-replace-regexp-prog (regexp replacement &optional from to)
  "By default acts on the whole buffer."
  (cl-assert (or (es-neither from to) (and from to)))
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (save-restriction
        (when (and from to)
          (narrow-to-region from to))
        (while (re-search-forward regexp nil t)
          (replace-match replacement t nil))))))

(defun es-replace-prog (original replacement &optional from to)
  "By default acts on the whole buffer."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (save-restriction
        (when (and from to)
          (narrow-to-region from to))
        (while (search-forward original nil t)
          (replace-match replacement t nil))))))

;;;###autoload
(defun es-find-function-bound-to (key-sequence)
  (interactive "kFind function bound to: ")
  (unless (equal key-sequence "")
    (let (( symbol (key-binding key-sequence)))
      (if (fboundp symbol)
          (progn
            (ring-insert find-tag-marker-ring (point-marker))
            (find-function symbol))
        (user-error "Key sequence unbound")))))

(defun es-add-at-eol (thing)
  "Insert THING at end of line.
If the line is empty, insert at the end of next line."
  (save-excursion
    (if (es-line-empty-p)
        (progn
          (forward-line 1)
          (goto-char (es-visible-end-of-line))
          (insert thing))
        (progn
          (goto-char (es-visible-end-of-line))
          (insert thing)))))

(defun es-add-semicolon-at-eol ()
  (interactive)
  (es-add-at-eol ";")
  (when (and (bound-and-true-p aai-mode)
             (fboundp 'aai-indent-line-maybe))
    (aai-indent-line-maybe)))

(defun es-add-comma-at-eol ()
  (interactive)
  (es-add-at-eol ","))

(defun es-buffers-with-mode (mode)
  (cl-remove mode (buffer-list)
             :key 'es-buffer-mode
             :test-not 'eq))

;;;###autoload
(defun es-push-line ()
  "beginning-of-line + open line."
  (interactive)
  (cond ( (> (line-number-at-pos) 1)
          (goto-char
           (min (save-excursion
                  (forward-line -1)
                  (point))
                (save-excursion
                  (line-move-visual -1)
                  (point))))
          (call-interactively
           (key-binding (kbd "C-e")))
          (end-of-line)
          (call-interactively
           (or (key-binding (kbd "<return>"))
               (key-binding (kbd "\r"))
               (key-binding (kbd "RET"))
               'newline)))
        ( t (beginning-of-line)
            (newline)
            (backward-char)
            (when (fboundp 'aai-indent-line-maybe)
              (aai-indent-line-maybe)))))

;;;###autoload
(defun es-jump-line ()
  "end-of-line + newline."
  (interactive)
  (goto-char (es-total-line-end-position))
  (call-interactively
   (or (key-binding (kbd "<return>"))
       (key-binding (kbd "\r"))
       (key-binding (kbd "RET"))
       'newline)))

;;;###autoload
(defun es-new-empty-buffer ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled")))

(cl-defun es-define-keys (keymap &rest bindings)
  "Syntax example:
\(es-define-keys fundamental-mode-map
  (kbd \"h\") 'backward-char
  (kbd \"l\") 'forward-char\)
 Returns the keymap in the end."
  (declare (indent 1))
  (setq keymap (or keymap (current-local-map)))
  (while bindings
    (define-key keymap (pop bindings) (pop bindings)))
  keymap)
(put 'es-define-keys 'common-lisp-indent-function
     '(4 &body))

;;;###autoload
(defvar es-highlighter-colors
  '("DeepPink" "cyan" "MediumPurple1" "SpringGreen1"
    "DarkOrange" "HotPink1" "RoyalBlue1" "OliveDrab"))
(defvar-local es-highlighter-color-index 0)

(cl-defun es-highlighter ()
  "Like `highlight-symbol-at-point', but will also (un)highlight a phrase if the \
region is active."
  (interactive)
  (with-no-warnings                  ; for "Warning: reference to free variable"
    (require 'hi-lock)
    (unless hi-lock-mode
      (hi-lock-mode))
    (let* ((phrase (if (region-active-p)
                       (regexp-quote (buffer-substring (point) (mark)))
                     (concat "\\_<"
                             (regexp-quote
                              (symbol-name
                               (or (symbol-at-point)
                                   (cl-return-from es-highlighter))))
                             "\\_>")))
           (pattern (cl-find-if (lambda (element)
                                  (equal (cl-first element) phrase))
                                hi-lock-interactive-patterns)))
      (if pattern
          (hi-lock-unface-buffer phrase)
        (let ((color (nth es-highlighter-color-index
                          es-highlighter-colors)))
          (if color ;; wrap
              (cl-incf es-highlighter-color-index)
            (setq es-highlighter-color-index 1
                  color (car highlight-symbol-colors)))
          (setq color `((background-color . ,color)
                        (foreground-color . "black")))
          (hi-lock-set-pattern phrase color)
          )))))

;;;###autoload
(defun es-mouse-copy-symbol (event)
  (interactive "e")
  (save-excursion
    (save-window-excursion
      (mouse-select-window event)
      (mouse-set-point event)
      (when (thing-at-point 'symbol)
        (kill-new (thing-at-point 'symbol))))))

;;;###autoload
(defun es-mouse-yank-replace-symbol (event)
  (interactive "e")
  (save-excursion
    (save-window-excursion
      (mouse-select-window event)
      (mouse-set-point event)
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (when bounds
          (cl-destructuring-bind
              (start . end)
              bounds
            (delete-region start end)
            (deactivate-mark)
            (yank)))))))

(defun es-next-match-pos (regex)
  (save-excursion
    (when (re-search-forward regex nil t)
      (point))))

;;;###autoload
(defun es-c-expand-region ()
  "A simplee version of expand-region for c-like languages.
Marks the symbol on first call, then marks the statement."
  (interactive)
  (let* (( post-scriptum
           (lambda ()
             (when (and (equal (point) (line-end-position))
                        (not (equal (point) (point-max))))
               (forward-char))
             (exchange-point-and-mark)))
         ( mark-statement-internal
           (lambda ()
             (back-to-indentation)
             (set-mark (point))
             (ignore-errors
               (while (equal (line-number-at-pos (point))
                             (line-number-at-pos (mark)))
                 (forward-sexp)))
             (when (equal (char-after (point))
                          (aref ";" 0))
               (forward-char))
             (funcall post-scriptum)))
         ( mark-colon-internal
           (lambda ()
             (let (next-opening-bracket next-colon)
               (back-to-indentation)
               (set-mark (point))
               (setq next-opening-bracket (es-next-match-pos "{")
                     next-colon (es-next-match-pos ";"))
               (cond ( (and next-opening-bracket next-colon)
                       (if (< next-opening-bracket next-colon)
                           (progn (goto-char next-opening-bracket)
                                  (backward-char)
                                  (forward-sexp))
                         (goto-char next-colon)))
                     ( next-opening-bracket
                       (progn (goto-char next-opening-bracket)
                              (backward-char)
                              (forward-sexp)))
                     ( t (goto-char next-colon)))
               (when (equal (char-after (point))
                            (aref ";" 0))
                 (forward-char))
               (funcall post-scriptum))))
         ( select-line-internal
           (lambda ()
             (back-to-indentation)
             (set-mark (point))
             (let (( next-colon
                     (save-excursion
                       (when (search-forward ";" nil t)
                         (point)))))
               (goto-char
                (if next-colon
                    (min next-colon (line-end-position))
                  (line-end-position))))
             (funcall post-scriptum))))
    (cond ( (and (looking-at "\"") (not (eq last-command this-command)))
            (mark-sexp))
          ( (not (eq last-command this-command))
            (es-mark-symbol-at-point))
          ( (member (char-to-string (char-before (line-end-position)))
                    (list "{" "(" "["))
            (funcall mark-statement-internal))
          ( (member (char-before (es-visible-end-of-line))
                    '( ?: ?, ) )
            (funcall mark-colon-internal))
          ( t (funcall select-line-internal)))))

;;;###autoload
(defun es-comment-dwim (&optional arg)
  (interactive "P")
  (cond ( (use-region-p)
          (comment-or-uncomment-region (region-beginning)
                                       (region-end)
                                       arg)
          (indent-region (region-beginning)
                         (region-end)))
        ( (es-line-empty-p)
          (cond ( (memq major-mode
                        '(lisp-mode lisp-interaction-mode emacs-lisp-mode))
                  (insert ";; "))
                ( (memq major-mode '(php-mode c-mode js2-mode js-mode))
                  (insert "// "))
                ( (eq major-mode 'css-mode)
                  (insert "/*  */")
                  (forward-char -3))
                ( t (insert comment-start)
                    (save-excursion
                      (insert comment-end)))))
        ( t (comment-or-uncomment-region (line-beginning-position)
                                         (line-end-position)
                                         arg)
            (indent-according-to-mode))))

;;;###autoload
(cl-defun es-ido-like-helm (&optional this-mode-only)
  "Choose from a concatenated list of buffers and recent files."
  (interactive "P")
  (require 'recentf)
  (when (window-dedicated-p)
    (message "This is a dedicated window")
    (cl-return-from es-ido-like-helm))
  (let* (( f:parent-dir
           (lambda (name)
             (file-name-nondirectory
              (directory-file-name
               (file-name-directory name)))))
         ( f:make-recentf-map
           (lambda (item)
             (cons (propertize
                    (concat (file-name-nondirectory item)
                            "<" (funcall f:parent-dir item) ">")
                    'face 'font-lock-keyword-face)
                   item)))
         ( buffer-list (es-buffer-name-list))
         ( recentf-map (mapcar f:make-recentf-map recentf-list))
         ( merged-list (append buffer-list recentf-map))
         ( no-duplicates
           (cl-remove-duplicates
            merged-list
            :key (lambda (thing)
                   (if (stringp thing)
                       (or (buffer-file-name (get-buffer thing))
                           (symbol-name (cl-gensym)))
                     (cdr thing)))
            :test 'equal
            :from-end t))
         ( junk-less
           (cl-remove-if
            (lambda (item)
              (member item (list (buffer-name)
                                 "Map_Sym.txt")))
            no-duplicates)))
    junk-less))

(defun es-ido-files-and-buffers (prompt list)
  (interactive)
  (let (( ido-result
          (es-ido-completing-read-alist prompt list nil t))
        ( buffer-list (es-buffer-name-list)))
    (when ido-result
      (if (member ido-result buffer-list)
          (switch-to-buffer ido-result)
        (find-file ido-result)))))

;;;###autoload
(cl-defun es-ido-like-helm (&optional this-mode-only)
  "Choose from a concatenated list of buffers and recent files."
  (interactive "P")
  (require 'recentf)
  (when (window-dedicated-p)
    (message "This is a dedicated window")
    (cl-return-from es-ido-like-helm))
  (let* (( f:parent-dir
           (lambda (name)
             (file-name-nondirectory
              (directory-file-name
               (file-name-directory name)))))
         ( f:make-recentf-map
           (lambda (item)
             (cons (propertize
                    (concat (file-name-nondirectory item)
                            "<" (funcall f:parent-dir item) ">")
                    'face 'font-lock-keyword-face)
                   item)))
         ( buffer-list (es-buffer-name-list))
         ( recentf-map (mapcar f:make-recentf-map recentf-list))
         ( merged-list (append buffer-list recentf-map))
         ( no-duplicates
           (cl-remove-duplicates
            merged-list
            :key (lambda (thing)
                   (if (stringp thing)
                       (or (buffer-file-name (get-buffer thing))
                           (symbol-name (cl-gensym)))
                     (cdr thing)))
            :test 'equal
            :from-end t))
         ( junk-less
           (cl-remove-if
            (lambda (item)
              (member item (list (buffer-name)
                                 "Map_Sym.txt")))
            no-duplicates))
         ( mode-filter
           (if this-mode-only
               (let (( extension
                       (file-name-extension
                        (or (buffer-file-name)
                            ""))))
                 (cl-remove-if-not
                  (lambda (maybe-cons)
                    (if (consp maybe-cons)
                        (when extension
                          (equal (file-name-extension
                                  (cdr maybe-cons))
                                 extension))
                      (eq (es-buffer-mode maybe-cons)
                          major-mode)))
                  junk-less))
             junk-less)))
    (es-ido-files-and-buffers "Choose existing: "
                              mode-filter)))

(defun es-find-duplicates (list)
  "Multiple duplicates will be listed muliple times.
The \"originals\" won't be included."
  (let ((singles (cl-remove-duplicates list :test 'equal))
        (clone (cl-copy-list list)))
    (while singles
      (setq clone (cl-remove (pop singles) clone :test 'equal :count 1)))
    clone))

(defun es-next-visible-character-at-pos (&optional position)
  (save-excursion
    (when position (goto-char position))
    (skip-chars-forward " \t\n")
    (char-after)))

(defun es-kill-dead-shells ()
  (mapc 'es-kill-buffer-dont-ask
        (cl-remove-if-not
         (lambda (buf)
           (and (eq (es-buffer-mode buf) 'shell-mode)
                (not (get-buffer-process buf))))
         (buffer-list))))

;;;###autoload
(cl-defun es-manage-unsaved-buffers()
  "Similar to what happends when emacs is about to quit."
  (interactive)
  (save-excursion
    (save-window-excursion
      (mapc (lambda (buf)
              (switch-to-buffer buf)
              (cl-case
                  (read-char "cNext(n) Save(s) Save All(!) Edit(e) Kill(k)? ")
                ( ?!
                  (cl-dolist (buf (es-unsaved-buffer-list))
                    (with-current-buffer buf
                      (save-buffer)))
                  (cl-return-from es-manage-unsaved-buffers))
                ( ?s (save-buffer))
                ( ?k (es-kill-buffer-dont-ask))
                ( ?e (recursive-edit))))
            ( or (es-unsaved-buffer-list)
                 (progn
                   (message "All buffers are saved")
                   (cl-return-from es-manage-unsaved-buffers))))
      (message "Done"))))

;;;###autoload
(defun es-query-replace-symbol-at-point ()
  (interactive)
  (let* (( original
           (if (region-active-p)
               (buffer-substring-no-properties
                (region-beginning)
                (region-end))
               (symbol-name
                (symbol-at-point))))
         ( replace-what
           (if (region-active-p)
               (regexp-quote original)
               (concat "\\_<"
                       (regexp-quote original)
                       "\\_>")))
         ( replacement
           (read-from-minibuffer
            (format "Replace \"%s\" with: " original)
            original)))
    (save-excursion
      (query-replace-regexp
       replace-what
       replacement
       nil (line-beginning-position)
       (point-max)))
    (when (save-excursion
            (re-search-backward
             replace-what
             nil t))
      (save-excursion
        (query-replace-regexp
         replace-what
         replacement
         nil (point-min) (line-beginning-position))))))

(defun es-mode-keymap (mode-sym)
  (symbol-value (intern (concat (symbol-name mode-sym) "-map"))))

(defun es-toggle-true-false-maybe ()
  (save-excursion
    (let (( replace
            (lambda (new)
              (es-mark-symbol-at-point)
              (delete-region (point) (mark))
              (insert new)
              t)))
      (cond ( (eq (symbol-at-point) 'true)
              (funcall replace "false"))
            ( (eq (symbol-at-point) 'false)
              (funcall replace "true"))

            ( (equal (word-at-point) "FIXED")
              (funcall replace "FIXME"))
            ( (equal (word-at-point) "FIXME")
              (funcall replace "FIXED"))

            ( (eq (symbol-at-point) 't)
              (funcall replace "nil"))
            ( (equal (word-at-point) "nil")
              (funcall replace "t"))
            ( t nil)))))

;;;###autoload
(cl-defun es-ack-replace-symbol
    (from-symbol-or-string
     to-symbol-or-string
     &key
     directory
     auto-save
     finish-func
     silent)
  "Repalace symbol at point, or region contents in multiple
files."
  (interactive (list nil nil))
  (require 'ack-and-a-half)
  (require 'wgrep)
  (require 'wgrep-ack)
  (let ((ack-and-a-half-arguments (list "-Q"))
        was-symbol)
    ;; Argument processing
    (if (called-interactively-p 'any)
        (progn (setq from-symbol-or-string
                     (or (es-active-region-string)
                         (when (symbol-at-point)
                           (setq was-symbol t)
                           (symbol-name (symbol-at-point)))
                         (let (sym)
                           (when (setq sym (read-string
                                            "Ack Replace which symbol: "))
                             (setq was-symbol t)
                             sym))
                         (cl-return-from es-ack-replace-symbol)))
               (setq to-symbol-or-string
                     (read-string
                      (format
                       "Ack Replace %s with: "
                       from-symbol-or-string)
                      from-symbol-or-string))
               (setq directory (ack-and-a-half-read-dir)))
        (progn (when (symbolp from-symbol-or-string)
                 (setq was-symbol t)
                 (setq from-symbol-or-string
                       (symbol-name from-symbol-or-string)))
               (when (symbolp to-symbol-or-string)
                 (setq to-symbol-or-string
                       (symbol-name to-symbol-or-string)))))
    (and (buffer-modified-p)
         (y-or-n-p "Save current buffer? ")
         (save-buffer))
    (ack-and-a-half from-symbol-or-string nil directory)
    (cl-dolist (window (window-list))
      (when (eq (es-buffer-mode
                 (window-buffer window))
                'ack-and-a-half-mode)
        (select-window window)))
    (set (make-local-variable 'compilation-finish-functions)
         (list `(lambda (buffer status)
                  (with-current-buffer buffer
                    (ignore-errors
                      (compilation-next-error 1))
                    (wgrep-change-to-wgrep-mode)
                    (es-replace-regexp-prog
                     ,(if was-symbol
                          (format
                           "\\_<%s\\_>"
                           (regexp-quote
                            from-symbol-or-string))
                          (regexp-quote
                           from-symbol-or-string))
                     ,to-symbol-or-string
                     (point)
                     (point-max))
                    (when ,auto-save
                      (let ((wgrep-auto-save-buffer t))
                        (wgrep-finish-edit)
                        (quit-window)))
                    (funcall (or ,finish-func 'ignore))))))))

;;;###autoload
(defun es-ack-pin-folder (folder)
  "Set ack root directory for one buffer only.
Ack won't prompt for a directory name in that buffer."
  (interactive
   (list (read-directory-name "Directory for ack: ")))
  (setq-local ack-and-a-half-root-directory-functions
              (list `(lambda () ,folder)))
  (setq-local ack-and-a-half-prompt-for-directory nil)
  (message "Ack directory set to: %s" folder))

(defun es-windows-with-buffer (buffer-or-name)
  "In all frames."
  (let ((buffer (window-normalize-buffer buffer-or-name)))
    (cl-remove-if-not
     (lambda (window)
       (eq (window-buffer window) buffer))
     (cl-loop for frame in (frame-list)
              append (window-list frame)))))

(defun es-random-member (list)
  (nth (random (length list)) list))

(defun es-pop-to-buffer-vertically (buf)
  (let ((split-height-threshold 0)
        (split-width-threshold 0))
    (pop-to-buffer buf)))

(cl-defun es-fixup-whitespace (&optional before after)
  "Fixup white space between objects around point.
Leave one space or none, according to the context.

An improvment over the built-in fixup-whitespace.
You might want to do \(defalias 'fixup-whitespace 'es-fixup-whitespace\)"
  (interactive "*")
  (delete-horizontal-space)
  (cl-macrolet ((sp-member (arg)`(member ,arg pair)))
    (let* (( pair (list (or (char-before) before)
                        (or (char-after) after)))
           ( operators '(?< ?> ?+ ?- ?/ ?* ?= ))
           ( insert-space
             (catch 'insert-space
               ;; All Langs
               (when (or (and (sp-member ?\") (in-string-p))
                         (equal pair '(  ?>  ?<  ))
                         (sp-member ?\s)
                         (sp-member ?\n)
                         ;; (eq '(   ?\(   ?\(   ) pair)
                         (member (cl-first pair)
                                 '(   ?\(   ))
                         (member (cl-second pair)
                                 '(   ?\)   ?\n   )))
                 (throw 'insert-space nil))
               ;; C Family
               (when (memq major-mode '(coffee-mode php-mode  c-mode  js2-mode  js-mode  css-mode))
                 (when (or (member (cl-first pair) operators)
                           (member (cl-second pair) operators))
                   (throw 'insert-space t))
                 (when (equal pair '( ?\{ ?\} ))
                   (throw 'insert-space nil))
                 (when (equal (cl-first pair) ?\;)
                   (throw 'insert-space t))
                 (when (or (equal (cl-second pair) ?\;)
                           (equal pair '(?\} ?\}))
                           (and (in-string-p) (sp-member ?\'))
                           (member (cl-second pair) '(?\n  ?\)   ?\(   ?,   ?:   nil)))
                   (throw 'insert-space nil)))
               (when (memq major-mode '(js2-mode js-mode))
                 (when (equal (cl-second pair) ?. )
                   (throw 'insert-space nil)))
               ;; Lisp Family
               (when (memq major-mode '(lisp-mode emacs-lisp-mode lisp-interaction-mode))
                 (when (or (equal pair '(  ?'  ?\(  )))
                   (throw 'insert-space nil))
                 (when (or (equal pair '(  ?\)  ?\(  ))
                           (and (sp-member ?\)) (not (eq (char-before) (char-after))))
                           (and (sp-member ?\() (not (eq (char-before) (char-after)))))
                   (throw 'insert-space t)))
               ;; Markup
               (when (memq major-mode '(html-mode nxml-mode php-mode web-mode))
                 (when (or (sp-member ?<)
                           (sp-member ?>))
                   (throw 'insert-space nil)))
               t)))
      (when insert-space
        (insert ?\s))
      insert-space)))

(defun es-reset-feature (feature)
  (when (featurep feature)
    (ignore-errors
      (unload-feature feature t)))
  (require feature))

(defun es-var-documentation (sym)
  "Get variable documentation, or nil if there isn't one."
  (ignore-errors
    (documentation-property
     sym 'variable-documentation t)))

(defun es-color-list-to-hex (color-list)
  (apply 'format "#%02X%02X%02X" color-list))

(defun es-color-normalize-hex (hex-string)
  (if (string-match-p "^#" hex-string)
      (upcase
       (if (= (length hex-string) 4)
           (apply 'concat "#"
                  (mapcar
                   (lambda (pair)
                     (make-string
                      2 (string-to-char
                         (substring
                          hex-string (car pair) (cdr pair)))))
                   '((1 . 2) (2 . 3) (3 . 4))))
           hex-string))
      hex-string))

(defun es-color-hex-to-list (hex-color)
  (let ((hex-color (es-color-normalize-hex hex-color)))
    (list (string-to-number (substring hex-color 1 3) 16)
          (string-to-number (substring hex-color 3 5) 16)
          (string-to-number (substring hex-color 5 7) 16))))

(defun es-color-emacs-color-to-hex (color)
  (let ((color-values (color-values color)))
    (if color-values
        (apply 'format "#%02x%02x%02x"
               (mapcar (lambda (c) (lsh c -8))
                       color-values))
        "#888888")))

(defun es-color-random-hex ()
  (es-color-list-to-hex (mapcar 'random (make-list 3 255))))

(defun es-disable-buffer-scrolling ()
  (es-buffer-local-set-keys
    [remap smooth-scroll-up] 'ignore
    [remap smooth-scroll-down] 'ignore
    [remap cua-scroll-up] 'ignore
    [remap cua-scroll-down] 'ignore
    [remap scroll-up-command] 'ignore
    [remap scroll-down-command] 'ignore
    (kbd "<down-mouse-1>") 'ignore
    (kbd "<drag-mouse-1>") 'ignore
    (kbd "<mouse-5>") 'ignore
    (kbd "<mouse-4>") 'ignore
    (kbd "<next>") 'ignore
    (kbd "<prior>") 'ignore)
  (add-hook 'post-command-hook (lambda () (set-window-start (selected-window) (point-min)))
            t t))

(defun es-figlet-fonts ()
  (cddr (directory-files "/usr/share/figlet")))

(defvar es-figlet-font-history nil)
(defvar es-figlet-phrase-history nil)

(defun es-figlet-insert (words &optional font additional-opts)
  "Insert a figlet-formatted phrase at point:
 _____ _       _      _
|  ___(_) __ _| | ___| |_
| |_  | |/ _` | |/ _ \\ __|
|  _| | | (_| | |  __/ |_
|_|   |_|\\__, |_|\\___|\\__|
         |___/"
  (interactive
   (list
    (read-string "Phrase: " nil 'es-figlet-phrase-history)
    (completing-read "Font: " (es-figlet-fonts)
                     nil t nil 'es-figlet-font-history)))
  (insert (shell-command-to-string
           (format "figlet %s %s %s"
                   (or additional-opts "")
                   (if (and font (not (equal font "")))
                       (concat "-f " font)
                       "")
                   (shell-quote-argument words)))))

(defun es-replace-in-string-multiple (string alist)
  "For each member of ALIST, replace all occurances of car with cdr.
car is a literal string, not a regular expression."
  (cl-dolist (pair alist)
    (setq string (replace-regexp-in-string (regexp-quote
                                            (car pair))
                                           (cdr pair)
                                           string)))
  string)

(defun es-full-window-list ()
  "Return all windows from all frames"
  (cl-loop for frame in (remove terminal-frame (frame-list))
           nconcing (window-list frame)))

(defun es-virtualize-overlay (ov)
  (prog1 (append (list (overlay-start ov) (overlay-end ov))
                 (overlay-properties ov))
    (delete-overlay ov)))

(defun es-realize-overlay (ov-spec)
  (cl-destructuring-bind
      (start end &rest props)
      ov-spec
    (let ((ov (make-overlay start end)))
      (while props (overlay-put ov (pop props) (pop props)))
      ov)))

(defun es-preserve-overlay (ov)
  (let (props start end)
    (unless (overlay-get ov 'invisible)
      (setq props (append (list 'invisible nil) props))
      (overlay-put ov 'invisible t))
    (when (overlay-get ov 'evaporate)
      (setq props (append (list 'evaporate t) props))
      (overlay-put ov 'evaporate nil))
    (setq start (overlay-start ov))
    (setq end (overlay-end ov))
    (move-overlay ov (point-min) (point-min))
    (append (list ov start end) props)))

(defun es-restore-overlay (ov-spec)
  (cl-destructuring-bind
      (ov start end &rest props)
      ov-spec
    (move-overlay ov start end)
    (while props
      (overlay-put ov (pop props) (pop props)))
    ))

(cl-defun es-set-window-body-width (window width)
  (interactive (list nil (read-number "New width: " 80)))
  (unless window
    (setq window (selected-window)))
  (let (( delta (- width (window-body-width window))))
    (window-resize window delta t)))

(cl-defun es-set-window-body-height (window height)
  (interactive (list nil (read-number "New height: " 30)))
  (unless window
    (setq window (selected-window)))
  (let (( delta (- height (window-body-height))))
    (window-resize window delta)))

(provide 'es-lib-core-functions)
;; es-lib-core-functions.el ends here
