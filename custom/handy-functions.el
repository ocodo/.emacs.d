;;; handy-functions --- a collection of functions I'm too lazy to organize properly...
;;; Commentary:
;;
;;  A collection of miscellaneous functions, which are either
;;  candidates to migrate to a minor mode, or will languish here in
;;  perpetuity.
;;
;;  Sorted in no particular order.
;;
;;; License:
;;  GPL3
;;
;;; Commentary:
;;
;; Some handy functions, homemade, pilfered, re-jigged, squeezed,
;; shuffled... do what thou wilt.
;;
;; List of defuns alphabetically...
;; (each should have it's DOCSTRING in place, do C-h f {name} for more info)
;;
;; - align-number-right
;; - cleanup-buffer
;; - copy-buffer-to-osx-clipboard
;; - copy-region-to-osx-clipboard

;; - delete-this-buffer-and-file
;; - directory-of-library
;; - duplicate-current-line-or-region
;; - eval-and-replace
;; - increase-default-font-height
;; - fraction-radian
;; - indent-buffer
;; - insert-random-in-range
;; - insert-random-radian
;; - join-line-from-below
;; - join-line-or-lines-in-region
;; - kill-whole-word
;; - magit-just-amend
;; - markdown-codefence-region
;; - my-dired-find-file
;; - now-is
;; - open-line-above
;; - open-line-below
;; - open-opsmanager
;; - pcre-regexp-from-list-of-words
;; - pretty-print-xml-region
;; - reload-current-chrome-tab-osx
;; - rename-this-buffer-and-file
;; - ruby-make-interpolated-string-at-point-or-region
;; - ruby-toggle-symbol-at-point
;; - sass-hex-color-to-var
;; - set-default-font-height
;; - shell-command-on-buffer-file
;; - shell-command-on-region-replace
;; - smart-beginning-of-line
;; - switch-to-scratch
;; - titleized-at-point-or-region
;; - toggle-fullscreen
;; - toggle-mode-line-on-off
;; - toggle-window-split
;; - untabify-buffer
;; - utc-seconds
;; - yank-repeat

;; String mangling/conversion
;; - lower-camelcase-at-point-or-region
;; - dasherise-at-point-or-region
;; - snake-case-at-point-or-region
;; - humanize-at-point-or-region
;; - upper-camelcase-at-point-or-region
;;
;; Non interactive
;;
;; - get-osx-display-resolution
;; - operate-on-point-or-region
;; - prepend-existing-to-exec-path
;; - random-in-range
;; - ruby-interpolated-string
;; - ruby-prepend-colon
;; - ruby-toggle-symbol-name
;; - search-backward-wrapped-string
;;
;; Global keys ...
;;
;; - C-S-o    open-line-above
;; - C-o      open-line-below
;; - C-a      smart-beginning-of-line
;; - C-c R    pcre-regexp-from-list-of-words
;; - ESC M-d  kill-whole-word
;; - C-c M-+  increase-default-font-height
;; - C-c =    set-default-font-height
;;
;;; Code:

(require 's)
(require 'dash)
(require 'find-func)

(defvar saved-mode-line
  nil
  "Save register for mode-line.")

(defun toggle-mode-line-on-off ()
  "Toggle the modeline off and on.
Uses `saved-mode-line' as a register.

Note this only affects the current buffer,
and it doesn't seem to work wth key bindings."
  (interactive)
  (if (eq mode-line-format nil)
      (setq mode-line-format saved-mode-line)
    (progn
      (setq saved-mode-line mode-line-format)
      (setq mode-line-format nil))))

(defun join-line-from-below ()
  "Join line from below."
  (interactive)
  (forward-line 1)
  (delete-indentation))

(defun prepend-existing-to-exec-path (path)
  "If PATH exists, prepend it to `exec-path'."
  (when (file-exists-p path)
    (setq exec-path (append '(path) exec-path))))

(defun align-number-right (begin end)
  "Align region to equal signs from BEGIN to END."
  (interactive "r")
  (align-regexp begin end ".* \\([0-9]+\\).*" -1 1 nil))

(defun insert-random-in-range (start end)
  "Insert a random number within the range of START and END."
  (interactive "nRange start:\nnRange end:")
  (insert (format "%i" (random-in-range start end))))

(defun insert-random-radian ()
  "Insert a radian value from 0 to 6.28318 (2PI : 360 deg)."
  (interactive)
  (insert (format "%s" (* (/ float-pi 180) (random 361)))))

(defun fraction-radian (denominator)
  "Fraction DENOMINATOR of circle to radians."
  (interactive "nDenomiator:")
  (insert (format "%s" (/ (* float-pi 2) denominator))))

(defun random-in-range (start end)
  "Return a random number in range START to END."
  (random t)
  (+ start (random (+ 1 (- end start)))))

(defun now-is ()
  "Insert current time."
  (interactive)
  (insert (format-time-string "%l:%M%P(%z) %Y-%m-%d")))

(defun utc-seconds ()
  "Insert UTC seconds."
  (interactive)
  (insert (format-time-string "%s")))

(defun untabify-buffer ()
  "Untabify the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a cleanup operations on a buffer, tabs to spaces, re-indent, trim whitespace."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun toggle-window-split ()
  "Toggle the current window split."
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
  "Open a newline below the current point."
  (interactive)
  (save-excursion
    (end-of-line)
    (newline)))

(defun open-line-above ()
  "Open a newline above the current point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (newline)
    (forward-line -1)))

(global-set-key (kbd "C-S-o") 'open-line-above)
(global-set-key (kbd "C-o") 'open-line-below)

;; Originally swiped from rejeep's emacs.d rejeep-defuns.el.
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times."
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
  "Run `shell-command-on-region' replacing the selected region.  START END COMMAND."
  (interactive (let (string)
                 (unless (mark)
                   (error "The mark is not set now, so there is no region"))
                 (setq string (read-from-minibuffer "Shell region | replace: "
                                                    nil nil nil
                                                    'shell-command-history))
                 (list (region-beginning) (region-end)
                       string)))
  (shell-command-on-region start end command t t))

(defun directory-of-library (libraryname)
  "Open directory with dired which contain the given LIBRARYNAME."
  (interactive "M")
  (dired (file-name-as-directory
          (file-name-directory (find-library-name libraryname)))))

(defun join-line-or-lines-in-region ()
  "Join this line or the lines in the selected region."
  (interactive)
  (cond ((region-active-p)
         (let ((min (line-number-at-pos (region-beginning))))
           (goto-char (region-end))
           (while (> (line-number-at-pos) min)
             (join-line))))
        (t (call-interactively 'join-line))))

(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name))
        (read-file-name-function 'read-file-name-default))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun delete-this-buffer-and-file (force)
  "Delete the file connected to this buffer and kill it, FORCE is universal argument."
  (interactive "P")
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "'%s' is not a file buffer" name)
      (when (or force (yes-or-no-p (format  "Delete '%s', Are you sure? " filename)))
        (delete-file filename)
        (kill-buffer buffer)
        (message "Deleted '%s'" filename)))))

(defun switch-to-scratch ()
  "Switch to scratch, grab the region if it's active."
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

(defun eval-and-replace-prin1 ()
  "Replace the preceding sexp with its value using prin1."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (insert (format "%s" (eval (read (current-kill 0)))))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(require 'magit)
(defun magit-just-amend ()
  "Just git commit --amend."
  (interactive)
  (save-window-excursion
    (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
    (magit-refresh)))

(defun shell-command-on-buffer-file ()
  "Run a shell command, using the file of current buffer as input.
Return an error if no buffer file."
  (interactive)
  (or (buffer-file-name) (error "There is no file associated with this buffer"))
  (let* ((my-cmd (read-shell-command "Command to run: "))
         (cmd-to-run (concat my-cmd " " (buffer-file-name))))
    (shell-command cmd-to-run)))

(defun snake-case-at-point-or-region ()
  "Snake_case the current word or text selection."
  (interactive)
  (operate-on-point-or-region 's-snake-case))

(defun dasherise-at-point-or-region ()
  "Dasherise-the-current CamelCase or snake_case word or text selection."
  (interactive)
  (operate-on-point-or-region 's-dashed-words))

(defun upper-camelcase-at-point-or-region ()
  "UpperCamelCaseTheCurrent dashed-or-snake_case_words or any words in text selection."
  (interactive)
  (operate-on-point-or-region 's-upper-camel-case))

(defun lower-camelcase-at-point-or-region ()
  "LowerCamelCaseTheCurrent dashed or snake_case word or any words in text selection."
  (interactive)
  (operate-on-point-or-region 's-lower-camel-case))

(defun humanize-at-point-or-region ()
  "Humanize variable names, insert spaces instead of - or _ or un-CamelCase humps to spaced words."
  (interactive)
  (operate-on-point-or-region 's-capitalized-words))

(defun titleized-at-point-or-region ()
  "Convert snaked, dashed, underscored, camelcase, or spaced words in region to Title Case."
  (interactive)
  (operate-on-point-or-region 's-titleized-words))

(defun operate-on-point-or-region (fn)
  "Get the current unspaced string at point.
Replace with the return value of the function FN"
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
  "Repeat yank n times ARG."
  (interactive "*p")
  (dotimes (string-to-int arg) (yank)))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or `beginning-of-line'."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key (kbd "C-a") 'smart-beginning-of-line)

(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region BEGIN END."
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
  "Toggle full screen."
  (interactive)
  (when window-system
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)) ))

(defun copy-region-to-osx-clipboard ()
  "Copy contents of the current region to the OS X Clipboard."
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "pbcopy"))

(defun copy-buffer-to-osx-clipboard ()
  "Copy contents of the current buffer to the OS X Clipboard."
  (interactive)
  (shell-command-on-region
   (point-min)
   (point-max)
   "pbcopy"))

(defun search-backward-wrapped-string (wrap_start wrap_end)
  "Search for a string backwards from the current point.

Use the strings WRAP_START and WRAP_END, to match the start and
end of the string.

if WRAP_END and WRAP_START are equal, we first position the point
at the beginning of the first WRAP_END match, before the initial
point.

The string found between the two wrappers is returned.

This is useful for naive finding of symbols previously defined in
the buffer."
  (save-excursion
    (when (equal wrap_start wrap_end)
      (search-backward wrap_end))
    (let* ((start_match
            (+ (search-backward wrap_start)
               (length wrap_start)))
           (end_match 0))
      (goto-char start_match)
      (setq end_match (- (search-forward wrap_end) 1))
      (buffer-substring-no-properties start_match end_match))))

;; Change a string to a ruby symbol, note: naive operation
(defun ruby-toggle-symbol-at-point ()
  "Dirt simple, just prefix current word with a colon."
  (interactive)
  (operate-on-point-or-region 'ruby-toggle-symbol-name))

(defun ruby-make-interpolated-string-at-point-or-region ()
  "Simple conversion of string/reigion to ruby interpolated string."
  (interactive)
  (operate-on-point-or-region 'ruby-interpolated-string))

(defun ruby-interpolated-string (s)
  "Make a ruby interpolated string entry S is a string."
  (format "#{%s}" s))

(defun ruby-prepend-colon (s)
  "Prepend a colon on the provided string S."
  (format ":%s" s))

(defun ruby-toggle-symbol-name (s)
  "Toggle colon prefix on string S."
  (if (s-matches? "^:.*" s)
      (s-replace ":" "" s)
    (ruby-prepend-colon s)))

(defun pcre-regexp-from-list-of-words (words)
  "Insert a pcre regexp to match a list of WORDS."
  (interactive "sList of words for regexp: ")
  (insert
   (pcre-to-elisp
    (regexp-opt (split-string words)))))

(global-set-key (kbd "C-c R") 'pcre-regexp-from-list-of-words)

(eval-after-load 'ruby-mode
  '(define-key ruby-mode-map (kbd "C-c :") 'ruby-toggle-symbol-at-point))

(eval-after-load 'ruby-mode
  '(define-key ruby-mode-map (kbd "C-c #") 'ruby-make-interpolated-string-at-point-or-region))

(defun kill-whole-word ()
  "Kill the current word at point."
  (interactive)
  (backward-word)
  (kill-word 1))

(global-set-key (kbd "ESC M-d") 'kill-whole-word)

(defun sass-hex-color-to-var ()
  "Find a hex color, and replace it with a newly created variable name.
Place the created variable at the top of the file.  Name it based
on the property being set, and its CSS selector, and set its
css-value to the hex color found."
  (interactive)
  (let
      (css-value
       css-property
       css-value-position
       variable-name
       variable-definition
       indent-level
       (css-selector ""))
    (save-excursion
      ;; search for a hex color
      (re-search-forward
       (rx bol (0+ blank)
           ;; CSS Property name
           (group (? "-") (regex "[_A-z]") (1+ (regex "[_0-9A-z-]")))
           (* blank) ":" (* blank) (* (regex "[A-z,0-9.% ]"))
           ;; Hex color
           (group "#" (** 3 6 (any hex-digit))) ";" eol))

      (setq css-value-position (match-beginning 2))
      (setq css-property (match-string-no-properties 1))
      (setq css-value  (match-string-no-properties 2))

      (move-end-of-line 1)
      (back-to-indentation)
      (setq indent-level (current-column))
      (while (< 0 indent-level)
        (re-search-backward
         (rx bol (* blank) (? "&") (? (any "." "#"))
             (group (any "_" alpha) (* (any "_" "-" "," " " ":" alphanumeric)))
             (* blank) "{"))
        (move-end-of-line 1)
        (back-to-indentation)
        (when (> indent-level (current-column))
          (setq indent-level (current-column))
          (setq css-selector
                (format "%s_%s" (match-string-no-properties 1) css-selector))))

      (setq variable-name
            (replace-regexp-in-string
             (rx (>= 2 "_")) "_"
             (replace-regexp-in-string
              (rx (any "&" ":" "-" "," " "))
              "_"
              (format "$%s%s" css-selector css-property))))

      (setq variable-definition (format "%s: %s;" variable-name css-value)))
    (goto-char css-value-position)

    (re-search-forward
     (rx "#" (** 3 6 (any hex-digit)) (0+ blank) ";" ))
    (replace-match (format "%s;" variable-name) t)

    (goto-char 0) (newline) (goto-char 0)
    (insert variable-definition)))

(defun markdown-codefence-region (start end)
  "Enclose the region (START END) in a GFM code-fence, ie. enclosed in three backticks."
  (interactive "r")
  (if (and start end)
      (progn
        (goto-char end)
        (insert "\n```\n\n")
        (delete-blank-lines) ; keep only a single blank line below
        (goto-char start)
        (insert "\n\n```\n")
        (forward-line -2)
        (delete-blank-lines) ; keep only a single blank line above
        (goto-char end)
        (forward-line))
    (message "markdown-codefence-region requires a region")))

(defun reload-current-chrome-tab-osx ()
  "Run a simple applescript to reload the current Google Chrome tab.

OSX specific of course."
  (interactive)
  (shell-command "echo 'tell application \"Google Chrome\"
                             reload active tab of window 1
                        end tell' | osascript" nil nil)
  (message "refreshed active Google Chrome tab"))

(defun open-opsmanager ()
  "Open OpsManager in dired."
  (interactive)
  (find-file "~/workspace/OpsManager"))

(defun get-osx-display-resolution ()
  "Get the current display resolution in OSX."
  (--map (s-split "x" it)
         (--filter (not (string= it ""))
                   (s-split "\n" (shell-command-to-string
                                  "system_profiler SPDisplaysDataType |\
                                   grep Resolution |\
                                   sed -e 's/Resolution: //' -e 's/ //g'")))))

(defun increase-default-font-height (m)
  "Adjust the default font :height by 10, universal argument is M (to set by multiples)."
  (interactive "p")
  (let ((new-height (+ (* m 10) (face-attribute 'default :height))))
    (set-face-attribute 'default nil :height new-height)
    (message "Default font height set to %i" new-height)))

(defun set-default-font-height (p)
  "Set the default font :height P (prefix arg) or enter in minibuffer."
  (interactive "P")
  (unless p
    (setq p (string-to-number (read-from-minibuffer
                               (format "Set default font height (currently %s): "
                                       (face-attribute 'default :height))))))
  (set-face-attribute 'default nil :height  p)
  (message "Default font height set to %s" p))

(global-set-key (kbd "C-c M-+") 'increase-default-font-height)
(global-set-key (kbd "C-c =") 'set-default-font-height)

(defun set-pivotal-api-key-from-dotfile ()
  "Set the `pivotal-api-token' from the setting in ~/.pivotal_api_key if it exists."
  (interactive)
  (if (file-exists-p "~/.pivotal_api_key")
      (progn
        (setq pivotal-api-token (substring-no-properties (with-temp-buffer (insert-file-contents "~/.pivotal_api_key") (buffer-string)) 5 37))
        (message "Pivotal api token was set to: %S" pivotal-api-token))
    (message "~/.pivotal_api_key was not found")))

(provide 'handy-functions)
;;; handy-functions.el ends here
