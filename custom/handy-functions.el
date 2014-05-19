(require 's)

(defun prepend-existing-to-exec-path (path)
  "If path exists, prepend it to exec-path"
  (when (file-exists-p path)
    (setq exec-path (append '(path) exec-path))))

;; Optional modes-init handling
(defun load-optional-mode-init (name)
  "Check for existence of a mode init script, and load if found"
  (let (file)
    (setq file (format "%smodes-init/init-%s.el" user-emacs-directory name))
    (when (file-readable-p file)
      (load-file file))))

(defun optional-mode-inits (names)
  "Processes a list of optional mode init names. The convention
used, is to store an optional init file in .emacs.d/modes-init/
named as init-{name} (replacing {name} with the name you wish to
use, usually the name of the mode/feature being initialized.)

For example, for paradox, a github token is required, which you
shouldn't keep in a public git repository with the rest of your
emacs config. So we'd add modes-init/init-paradox.el to
.gitignore.

To avoid issues when we want to load the init script, we use
load-optional-mode-init to check that the script exists, before
trying to run it."

  (mapcar 'load-optional-mode-init names))

(defun load-mode-init (name)
  "load a mode-init file expect an error if it doesn't map to an
existing file"
  (let (file)
    (setq file (format "%smodes-init/init-%s.el" user-emacs-directory name))
    (load-file file)))

(defun process-mode-inits (names)
  "Process a list of mandatory mode init names, convention is as
above"
  (mapcar 'load-mode-init names))

;; Handy functions, add little helpers in here.
(defun align-number-right (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end ".* \\([0-9]+\\).*" -1 1 nil))

(defun insert-random-in-range (start end)
  (interactive "nRange start:\nnRange end:")
  (insert (format "%i" (random-in-range start end))))

(defun insert-random-radian ()
  "insert a radian value from 0 to 6.28318 (2PI : 360 deg)"
  (interactive)
  (insert (format "%s" (* (/ float-pi 180) (random 361)))))

(defun fraction-radian (denominator)
  (interactive "nDenomiator:")
  (insert (format "%s" (/ (* float-pi 2) denominator))))

(defun random-in-range (start end)
  (random t)
  (+ start (random (+ 1 (- end start)))))

(defun degrees-to-radians (degrees)
  "Insert radians for degrees"
  (interactive "ndegrees:")
  (insert (format "%s" (* (/ float-pi 180) degrees))))

(defun now-is ()
  "Insert current time."
  (interactive)
  (insert (format-time-string "%l:%M%P(%z) %Y-%m-%d")))

(defun utc-seconds ()
  (interactive)
  (insert (format-time-string "%s")))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a cleanup operations on a buffer, tabs to spaces, re-indent, trim whitespace"
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun open-line-below ()
  (interactive)
  (save-excursion
    (end-of-line)
    (newline)))

(defun open-line-above ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (newline)
    (forward-line -1)))

(global-set-key (kbd "C-S-o") 'open-line-above)
(global-set-key (kbd "C-o") 'open-line-below)

;; Originally swiped from rejeep's emacs.d rejeep-defuns.el.
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and (region-active-p) (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if (region-active-p)
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun shell-command-on-region-replace (start end command)
  "Run shell-command-on-region replacing the selected region with
stdout, or inserting at the current point. Note, if the mark is still active (even though the region isn't visible.) it will be used. TODO is in place to fix this, but it's low on my list of things."
  (interactive (let (string)
                 (unless (mark)
                   (error "The mark is not set now, so there is no region"))
                 (setq string (read-from-minibuffer "Shell region | replace: "
                                                    nil nil nil
                                                    'shell-command-history))
                 (list (region-beginning) (region-end)
                       string)))
  (shell-command-on-region start end command t t))

(defun directory-of-library (library-name)
  "open directory with dired which contains the given library"
  (interactive "M")
  (dired (file-name-as-directory
          (file-name-directory (find-library-name library-name)))))

(defun delete-this-file ()
  (interactive)
  (or (buffer-file-name) (error "no file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun switch-to-scratch ()
  "switch to scratch, grab the region if it's active"
  (interactive)
  (let ((contents
         (and (region-active-p)
              (buffer-substring (region-beginning)
                                (region-end)))))
    (switch-to-buffer "*scratch*")
    (if contents
        (progn
          (goto-char (buffer-end 1))
          (insert contents)))))

(defun ca-with-comment (str)
  (format "%s%s%s" comment-start str comment-end))

(defun ca-all-asscs (assoc_list query)
  "returns a list of all corresponding values (like rassoc)"
  (cond
   ((null assoc_list) nil)
   (t
    (if (equal (cdr (car assoc_list)) query)
        (cons (car (car assoc_list)) (ca-all-asscs (cdr assoc_list) query))
      (ca-all-asscs (cdr assoc_list) query)))))

(defun shell-command-on-buffer-file ()
  "prompts for a command and executes that command on to the associated
 file of current buffer. if no buffer is associated gives an error"
  (interactive)
  (or (buffer-file-name) (error "no file is associated file to this buffer"))
  (let* ((my-cmd (read-shell-command "Command to run: "))
         (cmd-to-run (concat my-cmd " " (buffer-file-name))))
    (shell-command cmd-to-run)))

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

;; A bunch of "on point or region" do something commands, mostly
;; (all?) using the 's' library.  Possibly worth extracting to it's
;; own feature.
(require 's)

(defun snake-case-at-point-or-region ()
  "snake_case the current word or text selection."
  (interactive)
  (operate-on-point-or-region 's-snake-case))

(defun dasherise-at-point-or-region ()
  "dasherise-the-current CamelCase or snake_case word or text selection."
  (interactive)
  (operate-on-point-or-region 's-dashed-words))

(defun upper-camelcase-at-point-or-region ()
  "UpperCamelCaseTheCurrent dashed-or-snake_case_words or any words in text selection."
  (interactive)
  (operate-on-point-or-region 's-upper-camel-case))

(defun lower-camelcase-at-point-or-region ()
  "lowerCamelCaseTheCurrent dashed or snake_case word or any words in text selection."
  (interactive)
  (operate-on-point-or-region 's-lower-camel-case))

(defun humanize-at-point-or-region ()
  "Humanize variable names, insert spaces instead of - or _ or un-CamelCase humps to spaced words."
  (interactive)
  (operate-on-point-or-region 's-capitalized-words))

(defun titleized-at-point-or-region ()
  "Convert dashed, underscored or (both styles of) CamelCase,
  or spaced words in region, Title Case Words."
  (interactive)
  (operate-on-point-or-region 's-titleized-words))

(defun operate-on-point-or-region (fn)
  "Get the current unspaced string at point, or the current region, if selected, and replace it with the return value of fn - an ordinary defun."
  (let (pos1 pos2 meat excerpt)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'symbol))
            pos2 (cdr (bounds-of-thing-at-point 'symbol))))
    (setq excerpt (buffer-substring-no-properties pos1 pos2))
    (setq meat (funcall fn excerpt))
    (delete-region pos1 pos2)
    (insert  meat)))

(defun yank-repeat (&optional arg)
  "Repeat yank with M- number, (normal opertation of M- number
yank is to get that numbered item from the kill ring)"
  (interactive "*p")
  (dotimes (string-to-int arg) (yank)))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key (kbd "C-a") 'smart-beginning-of-line)

(defun find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the default-directory.
looking for a file with name file-to-find.  Returns the path to it
or nil if not found."
  (cl-labels
      ((find-file-r (path)
                    (let* ((parent (file-name-directory path))
                           (possible-file (concat parent file-to-find)))
                      (cond
                       ((file-exists-p possible-file) possible-file) ; Found
                       ;; The parent of ~ is nil and the parent of / is itself.
                       ;; Thus the terminating condition for not finding the file
                       ;; accounts for both.
                       ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
                       (t (find-file-r (directory-file-name parent))))))) ; Continue
    (find-file-r default-directory)))

(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region."
  (interactive "r")
  (save-excursion
    ;; split <foo><bar> or </foo><bar>, but not <foo></foo>
    (goto-char begin)
    (while (search-forward-regexp ">[ \t]*<[^/]" end t)
      (backward-char 2) (insert "\n") (incf end))
    ;; split <foo/></foo> and </foo></foo>
    (goto-char begin)
    (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
      (backward-char) (insert "\n") (incf end))
    ;; put xml namespace decls on newline
    (goto-char begin)
    (while (search-forward-regexp "\\(<\\([a-zA-Z][-:A-Za-z0-9]*\\)\\|['\"]\\) \\(xmlns[=:]\\)" end t)
      (goto-char (match-end 0))
      (backward-char 6) (insert "\n") (incf end))
    (indent-region begin end nil)))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (when window-system
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)) ))

(defun copy-region-to-osx-clipboard ()
  "Copy current region to the OS X Clipboard"
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "pbcopy"))

(defun copy-buffer-to-osx-clipboard ()
  "Copy contents of current buffer to the OS X Clipboard"
  (interactive)
  (shell-command-on-region
   (point-min)
   (point-max)
   "pbcopy"))

(defun search-backward-wrapped-string (wrap_start wrap_end)
  "Search for a string behind point which is wrapped in two
strings, wrap_start and wrap_end.

if wrap_end and wrap_start are equal, we first position the point
at the beginning of the first wrap_end match, before the initial
point.

The string found between the two wrappers is returned.

This is useful for naive finding of symbols previously defined in the
buffer."
  (save-excursion
    (when (equal wrap_start wrap_end)
      (search-backward wrap-end))
    (let* ((start_match
            (+ (search-backward wrap_start)
               (length wrap_start)))
           (end_match 0))
      (goto-char start_match)
      (setq end_match (- (search-forward wrap_end) 1))
      (buffer-substring-no-properties start_match end_match))))


;; Change a string to a ruby symbol, note: naive operation
(defun ruby-make-symbol-at-point ()
  "Dirt simple, just prefix current word with a colon"
  (interactive)
  (operate-on-point-or-region 'ruby-prepend-colon))

(defun ruby-make-interpolated-string-at-point-or-region ()
  "Simple conversion of string/reigion to ruby interpolated string"
  (interactive)
  (operate-on-point-or-region 'ruby-interpolated-string))

(defun ruby-interpolated-string (s) ""
  (format "#{%s}" s))

(defun ruby-prepend-colon (s) ""
  (format ":%s" s))

(defun pcre-regexp-from-list-of-words (words)
  "insert a pcre regexp to match a list of words"
  (interactive "sList of words for regexp: ")
  (insert
   (pcre-to-elisp
    (regexp-opt (split-string words)))))

(global-set-key (kbd "C-c R") 'pcre-regexp-from-list-of-words)

(eval-after-load 'ruby-mode
  '(define-key ruby-mode-map (kbd "C-c :") 'ruby-make-symbol-at-point))

(eval-after-load 'ruby-mode
  '(define-key ruby-mode-map (kbd "C-c #") 'ruby-make-interpolated-string-at-point-or-region))

(provide 'handy-functions)
