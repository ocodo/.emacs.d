;;; php-font-lock.el --- Functions that deal with PHP font locking and syntax

;; Version: 1.0
;; Created: 11-23-2011
;; Copyright © 2011 Michael Dwyer
;; Author(s): 
;; Michael Dwyer <mdwyer@ehtech.in>

;;; *****
;;; About
;;; *****

;; php-font-lock.el is a part of the php+-mode suite and contains functions
;; that are used by the syntax and font-lock engines. 

;; ********************************************************************

;; ************
;; REQUIREMENTS
;; ************
(require 'php-const)
(require 'php-structure)

;; *********
;; FUNCTIONS
;; *********

(defun php-setup-font-locking ()
  (make-local-variable 'font-lock-defaults)
  
  (setq font-lock-defaults
        '((php-font-lock-keywords-1
           php-font-lock-keywords-2
           php-font-lock-keywords-3)
          nil                   ; KEYWORDS-ONLY
          t                     ; CASE-FOLD
          (("_" . "w"))         ; SYNTAX-ALIST
          nil                   ; SYNTAX-BEGIN
          (font-lock-syntactic-keywords
           . (("_" (0 "w"))
              ("\\`\\(?:\\(?1:[^<]\\)\\|\\(?1:<\\)[^?]\\)" (1 "<"))
              ("\\(\\?\\)>" (1 "<")) ("\\(<\\)\\?" (1 ">"))
              ("#" (0 "<")) ("#.*\\(\n\\)" (1 ">"))
              ("\\(<\\)<<\\([\"']?\\)[a-zA-Z_-ÿ][a-zA-Z0-9_-ÿ]*\\(\\2\\)\n"
               (1 "<") (2 ".") (3 "."))
              ("^[a-zA-Z_-ÿ][a-zA-Z0-9_-ÿ]*\\([;\n]\\)" (1 ">"))))
          (font-lock-syntactic-face-function . php-syntactic-face-function)))

  (add-hook 'font-lock-extend-region-functions 'php-font-lock-extend-region nil 
            t)

  (setq font-lock-maximum-decoration t
        case-fold-search t              ; PHP vars are case-sensitive
        imenu-generic-expression php-imenu-generic-expression))

(defun php-font-lock-extend-region (&optional verbose)
  (let ((verbose (or (and (boundp 'php-verbose) php-verbose) verbose))
        changed)
    (when verbose
      (message "Called with bounds %s - %s" font-lock-beg font-lock-end))
    (dolist (bound-sym '(font-lock-beg font-lock-end))
      (let ((bound (symbol-value bound-sym))
            (comp (if (eq bound-sym 'font-lock-beg) 'min 'max)))
        (when verbose
          (message "Checking bound %s" bound))
        (let ((text-begin (php-in-text-structp nil bound)))
          (when text-begin
            (when verbose
              (message "In text: %s" text-begin))
            (let* ((new-bound-test  (if (eq bound-sym 'font-lock-beg)
                                        text-begin
                                      (save-excursion
                                        (goto-char text-begin)
                                        (php-skip-this-text-struct)
                                        (min (point-max) (point)))))
                   (new-bound (funcall comp new-bound-test bound)))
              (when (not (= bound new-bound))
                (when verbose
                  (message "Setting %s to %s" bound-sym new-bound))
                (setf (symbol-value bound-sym) new-bound changed t)))))))
    (when verbose
      (message "Returning changed = %s" changed))
    changed))

(defun php-syntactic-face-function (parse-state &optional verbose)
  (let ((verbose (or (and (boundp 'php-verbose) php-verbose) verbose)))
    (when verbose
      (message "%s: (%s - %s)(%s - %s) - %s" (point)
               (when (char-before) (char-to-string (char-before)))
               (syntax-after (1- (point)))
               (when (char-after) (char-to-string (char-after))) 
               (syntax-after (point))
               parse-state))
    (save-match-data
      (save-excursion
        (when (and (equal (syntax-after (line-end-position)) '(12))
                   (looking-at doc-end-tag-re)
                   (goto-char (elt parse-state 8))
                   (not (looking-at-p (concat "<<<\\(['\"]?\\)" 
                                              (match-string-no-properties 1)
                                              "\\1\n"))))
          (when verbose
            (message "Removing comment status from %s - %s" 
                     (match-beginning 0) (match-end 0)))
          (put-text-property (match-beginning 0) (match-end 0) 'syntax-table 
                             '(2))))
      (save-excursion
        (let ((face (cond ((and (elt parse-state 4)
                                (looking-at
                                 (concat (substring doc-begin-tag-re 1))))
                           (when verbose
                             (message "Looking at doc begin tag."))
                           (let* ((doc-end-re 
                                   (concat "^" (match-string-no-properties 2) 
                                           "[\n;]"))
                                  (doc-end 
                                   (save-excursion
                                     (re-search-forward doc-end-re nil t))))
                             (when (integerp doc-end)
                               (when verbose
                                 (message "Searched for %s" doc-end-re)
                                 (message (concat "Found true doc end tag (%s) "
                                                  "at %s - %s")
                                          (match-string-no-properties 0)
                                          (match-beginning 0) (match-end 0))
                                 (message "Setting syntax in between to word."))
                               (put-text-property (point) (- (match-end 0) 2)
                                                  'syntax-table '(2))
                               (when verbose
                                 (message (concat "Setting syntax of end to "
                                                  "comment end.")))
                               (put-text-property (- (match-end 0) 2) 
                                                  (1- (match-end 0))
                                                  'syntax-table '(12))))
                           font-lock-doc-face)
                          ((and (elt parse-state 4)
                                (eq (php-get-text-type 
                                     (+ (point)
                                        (cond ((looking-at-p "\\?>") 2)
                                              ((looking-at-p ">") 1)
                                              (t 0))))
                                    'bare-html))
                           (put-text-property (1- (point)) (1+ (point))
                                              'face 
                                              'font-lock-preprocessor-face)
                           (put-text-property (1- (point)) (1+ (point))
                                              'fontified t)
                           (php-syntactic-face-function-process-bare-html))
                          ((and (elt parse-state 4)
                                (or (looking-at "\\(?1:<\\?\\(php\\|=\\)?\\)")
                                    (and (looking-back-p "<")
                                         (looking-at 
                                          "\\(?1:\\?\\(php\\|=\\)?\\)"))
                                    (and (looking-back-p "\\?")
                                         (looking-at ">"))))
                           (let* ((script-tag (match-string-no-properties 1))
                                  (script-tag-begin (match-beginning 1))
                                  (script-tag-end (match-end 1)))
                             (when script-tag
                               (when (string= (substring script-tag 0 1) "?")
                                 (setf script-tag (concat "<" script-tag)
                                       script-tag-begin (1- script-tag-begin)))
                               (when verbose
                                 (message (concat "Looking at script tag: %s "
                                                  "Setting face and syntax at "
                                                  "%s.")
                                          script-tag
                                          script-tag-begin))
                               (put-text-property script-tag-begin 
                                                  script-tag-end 'face 
                                                  'font-lock-preprocessor-face)
                               (put-text-property script-tag-begin 
                                                  script-tag-end 'fontified t)
                               (when (= ?> (char-before script-tag-end))
                                 (put-text-property (1- script-tag-end)
                                                    script-tag-end
                                                    'syntax-table '(11)))
                               (when (= ?< (char-after script-tag-begin))
                                 (put-text-property script-tag-begin
                                                    (1+ script-tag-begin)
                                                    'syntax-table '(12)))))
                           (unless (or (looking-at-p (concat ws-re "*\\'"))
                                       (looking-at-p "<\\?\\(php\\|=\\)?")
                                       (and (looking-back-p "<")
                                            (looking-at-p "\\?\\(php\\|=\\)?"))
                                       (and (looking-back-p "\\?")
                                            (looking-at-p ">")))
                             font-lock-doc-face))
                          ((elt parse-state 4)
                           (let ((comment-char (char-before)))
                             (when verbose (message "Default comment"))
                             (when (re-search-forward "\\?>" 
                                                      (line-end-position) t)
                               (when verbose
                                 (message "Found script end tag in comment."))
                               (put-text-property (1- (match-beginning 0))
                                                  (match-beginning 0)
                                                  'syntax-table 
                                                  (if (char-equal comment-char
                                                                  ?#)
                                                      '(12) '(2097164)))
                               (put-text-property (match-beginning 0) 
                                                  (match-end 0) 'face
                                                  'font-lock-preprocessor-face)
                               (put-text-property (match-beginning 0)
                                                  (match-end 0) 'fontified t))
                             font-lock-comment-face))
                          ((elt parse-state 3) 
                           (when verbose
                             (message "Default string"))
                           (php-syntactic-face-function-process-string)))))
          (when verbose
            (message "Returning %s at %s" face (point)))
          face)))))

(defun php-syntactic-face-function-process-string ()
  (when (or (char-equal (char-before) ?\"))
    (let* ((string-end (save-excursion (php-skip-this-text-struct)))
           (text-begin (when string-end (point)))
           (bump 0))
      (when (wholenump text-begin)
        (when verbose
          (message "Processing string at %s." text-begin))
        (put-text-property (1- text-begin) (point) 'face 
                           'font-lock-string-face)
        (put-text-property (1- text-begin) (point) 'fontified t)
        (remove-text-properties (point) (1- string-end) '(face nil))
        (put-text-property (1- string-end) string-end 'face 
                           'font-lock-string-face)
        (put-text-property (1- string-end) string-end 'fontified t) 
        (while (re-search-forward "{?\\$" string-end t)
           (let ((code-begin (match-beginning 0)))
             (when verbose
               (message "Setting text from %s -> %s" text-begin code-begin))
            (put-text-property text-begin code-begin 'face 
                               'font-lock-string-face)
            (put-text-property text-begin code-begin 'fontified t)
            (let ((code-end 
                   (or (if (looking-back-p "{\\$")
                           (catch 'done
                             (while (re-search-forward "[\"'}]" string-end t)
                               (let* ((this-quote 
                                       (match-string-no-properties 0))
                                      (code-end (- (point) 
                                                   (if (string= this-quote 
                                                                "}") 
                                                       0 1))))
                                 (if (string= this-quote "}")
                                     (throw 'done (point))
                                   (let* ((string-begin (match-beginning 0))
                                          (s-end (or (re-search-forward 
                                                      this-quote string-end t)
                                                     string-end)))
                                     (put-text-property 
                                      string-begin s-end 'face 
                                      'font-lock-string-face)
                                     (put-text-property string-begin 
                                                        (1+ string-begin)
                                                        'syntax-table 
                                                        '(15))
                                     (put-text-property s-end (1- s-end)
                                                        'syntax-table 
                                                        '(15))
                                     (put-text-property string-begin s-end
                                                        'fontified t)
                                     (setf code-begin string-end))))))
                         (php-skip-this-identifier string-end t))
                       string-end)))
              (setf text-begin code-end))))
        (when verbose
          (message "Setting text from %s -> %s" text-begin string-end))
        (put-text-property text-begin string-end 'face 
                           'font-lock-string-face)
        (put-text-property text-begin string-end 'fontified t))))
  font-lock-string-face)

(defun php-syntactic-face-function-process-bare-html ()
  (when verbose
    (message "Looking at bare html %s." (point)))
  (if (looking-at-p ">")
      (forward-char)
    (goto-char (point-min)))
  (save-match-data
    (when (looking-at "[^<]*>")
      (goto-char (match-end 0))))
  (let ((html-end 
         (or (save-excursion
               (save-match-data
                 (when (re-search-forward "<\\?\\(php\\|=\\)?" nil t)
                   (put-text-property (match-beginning 0) (match-end 0) 'face
                                      'font-lock-preprocessor-face)
                   (match-beginning 0))))
             (point-max)))
        tag end-tagp tag-begin tag-end)
    (when verbose 
      (message "Bare html ends at %s" html-end))
    (when (> html-end (point))
      (let* ((current-tag 
              (save-excursion
                (save-match-data
                  (catch 'done
                    (while (re-search-backward 
                            "<\\(/?\\)\\(\\(?:\\sw\\|-\\)+\\)" nil t)
                      (when (eq 'bare-html (php-get-text-type))
                        (throw 'done 
                               (when (zerop (length (match-string-no-properties 
                                                     1)))
                                 (downcase 
                                  (match-string-no-properties 2))))))))))
             (next-tag-pos 
              (save-excursion
                (save-match-data
                  (or (re-search-forward 
                       "<\\(/?\\)\\(\\(?:\\sw\\|-\\)+\\)" html-end t)
                      html-end))))
             (doc-begin (if (member current-tag '("style" "script"))
                            next-tag-pos
                          (point))))
        (setq doc-begin (save-excursion
                          (goto-char doc-begin)
                          (catch 'done
                            (while (< (point) html-end)
                              (if (get-text-property (point) 'fontified)
                                  (forward-char)
                                (throw 'done (point)))))))
        (when (wholenump doc-begin)
          (put-text-property doc-begin html-end 'face 'font-lock-doc-face)
          (put-text-property doc-begin html-end 'fontified t))
        (save-excursion
          (save-match-data
            (while (re-search-forward "#" html-end t)
              (when verbose
                (message "Found # at %s with syntax-table: %s"
                         (match-beginning 0)
                         (syntax-after (match-beginning 0))))
              (put-text-property (match-beginning 0) (match-end 0) 
                                 'syntax-table '(3))
              (put-text-property (line-end-position) (1+ (line-end-position))
                                 'syntax-table '(3)))))
        (save-excursion
          (save-match-data
            (when (re-search-forward "<!DOCTYPE[^>]*>" html-end t)
              (put-text-property (match-beginning 0) (match-end 0) 'face
                                 'font-lock-preprocessor-face)
              (put-text-property (match-beginning 0) (match-end 0) 'fontified 
                                 t))))
        (save-excursion
          (save-match-data
            (while (and (<= (point) html-end)
                        (re-search-forward "<\\(/?\\)\\(\\(?:\\sw\\|-\\)+\\)" 
                                           html-end t))
              ;; (when verbose
              ;;   (message "Found HTML tag %s at %s" 
              ;;            (match-string-no-properties 0) (match-beginning 0)))
              (setf tag (match-string-no-properties 2)
                    end-tagp 
                    (not (zerop (length (match-string-no-properties 1))))
                    tag-begin (match-beginning 0)
                    tag-begin-end (match-end 0)
                    tag-end
                    (or (save-excursion
                          (catch 'found
                            (while (re-search-forward "<\\?\\|[\"'>]" nil t)
                              (let ((match (match-string-no-properties 0)))
                                (cond ((member match '("\"" "'"))
                                       (backward-char)
                                       (unless (php-skip-this-text-struct nil t)
                                         (throw 'found (point-max))))
                                      ((string= match "<?")
                                       (unless (php-skip-this-script)
                                         (throw 'found (point-max))))
                                      (t (throw 'found (point))))))))
                        (point-max))
                    tag-end-begin 
                    (- tag-end 
                       (if (char-equal (char-before (1- tag-end)) ?/) 2 1)))
              ;; (when verbose 
              ;;   (message "Full tag: %s" 
              ;;            (buffer-substring-no-properties tag-begin 
              ;;                                            tag-end)))
              (put-text-property tag-begin tag-begin-end 'face 
                                 'font-lock-builtin-face)
              (put-text-property tag-begin tag-begin-end 'fontified t)
              (put-text-property tag-end-begin tag-end 'face 
                                 'font-lock-builtin-face)
              (put-text-property tag-end-begin tag-end 'fontified t)
              (while (and (<= (point) tag-end)
                          (re-search-forward ws-re tag-end t))
                (when (re-search-forward non-ws-re tag-end t)
                  (if (php-in-scriptp)
                      (php-skip-this-script)
                    (backward-char)
                    (save-match-data
                      (when (looking-at 
                             (concat "\\(\\(?:\\sw\\|-\\)+\\)\\(?:\\s-*=\\s-*"
                                     "\\(?:\\(?:\\(?8:[\"']\\)"
                                     "\\(?2:\\(?:\n\\|.\\)*?\\)"
                                     "\\(?:\\(?9:\\8\\)\\|<\\?\\)\\)"
                                     "\\|\\(?3:[^[:space:]>]*\\)\\)\\)?"))
                        ;; (when verbose 
                        ;;   (message "Found attribute: %s (%s, %s | %s (%s | %s)"
                        ;;            (match-string-no-properties 0)
                        ;;            (match-string-no-properties 1)
                        ;;            (match-string-no-properties 2)
                        ;;            (match-string-no-properties 3)
                        ;;            (match-string-no-properties 8)
                        ;;            (match-string-no-properties 9)))
                        (put-text-property (match-beginning 1) (match-end 1) 
                                           'face 
                                           'font-lock-variable-name-face)
                        (put-text-property (match-beginning 1) (match-end 1) 
                                           'fontified t)
                        (when (match-beginning 2)
                          (put-text-property (match-beginning 2) (match-end 2) 
                                             'face 'font-lock-string-face)
                          (put-text-property (match-beginning 2) (match-end 2) 
                                             'fontified t))
                        (let ((begin-quote (match-beginning 8))
                              (begin-quote-char (match-string-no-properties 8))
                              (end-quote (match-beginning 9)))
                          (when begin-quote
                            ;; (when verbose
                            ;;   (message "Stringifying opening quote at %s." 
                            ;;            begin-quote))
                            (put-text-property begin-quote (1+ begin-quote)
                                             'face 'font-lock-string-face)
                            (put-text-property begin-quote (1+ begin-quote)
                                               'fontified t))
                          (when (and begin-quote (not end-quote))
                            ;; (when verbose
                            ;;   (message "Searching for end quote.")
                              (let ((found 
                                     (catch 'found
                                       (save-excursion
                                         (goto-char (1+ begin-quote))
                                         (save-match-data
                                           (while (re-search-forward 
                                                   (concat "<\\?\\|" 
                                                           begin-quote-char) 
                                                   tag-end t)
                                             (if (string= (match-string 0) "<?")
                                                 (re-search-forward "\?>" 
                                                                    tag-end t)
                                               (throw 'found 
                                                      (match-beginning 
                                                       0)))))))))
                                ;; (when verbose
                                ;;   (if found 
                                ;;       (message "Found it: %s" found)
                                ;;     (message "Not found.")))
                                (setq end-quote found)))
                          (when end-quote
                            ;; (when verbose
                            ;;   (message "Stringifying closing quote at %s."
                            ;;            end-quote))
                            (put-text-property end-quote (1+ end-quote) 
                                               'face 'font-lock-string-face)
                            (put-text-property end-quote (1+ end-quote) 
                                               'fontified t)))
                        (when (match-beginning 3)
                          (put-text-property (match-beginning 3) (match-end 3) 
                                             'face 'font-lock-constant-face)
                          (put-text-property (match-beginning 3) (match-end 3) 
                                             'fontified t))
                        (let ((attribute-end (match-end 0)))
                          (if (and (match-beginning 8)
                                   (not (match-beginning 9)))
                              (let ((this-quote (match-string-no-properties 8)))
                                (save-match-data
                                  (catch 'found
                                    (while (re-search-forward "\\?>" nil t)
                                      (if (looking-at (concat "[^" this-quote 
                                                              "]*<\\?"))
                                          (goto-char (match-end 0))
                                        (when (looking-at 
                                               (concat ".*?" this-quote))
                                          (put-text-property 
                                           (match-beginning 0) 
                                           (match-end 0)
                                           'face 'font-lock-string-face)
                                          (put-text-property 
                                           (match-beginning 0) 
                                           (match-end 0)
                                           'fontified t)
                                          (goto-char (match-end 0))
                                          (throw 'found t))))))))
                          (goto-char attribute-end)))))))
              (when (and tag (not end-tagp))
                (cond ((string= (downcase tag) "style")
                       (php-syntactic-face-function-process-css))
                      ((string= (downcase tag) "script")
                       (php-syntactic-face-function-process-javascript)))))))
        (save-excursion
          (save-match-data
            (while (and (<= (point) html-end)
                        (re-search-forward "<!--" html-end t))
              (let* ((comment-begin (match-beginning 0))
                     (comment-end (or (re-search-forward "-->" nil t) 
                                      (point-max))))
                (put-text-property comment-begin comment-end 'face 
                                   'font-lock-comment-face)))))))
    nil))
 
(defun php-syntactic-face-function-process-css ()
  (when verbose
    (message "STYLE tag found, highlighting CSS."))
  (forward-char)
  (let ((css-start (point))
        (end-tag (save-excursion 
                   (if (re-search-forward "</style>" nil t)
                       (match-beginning 0)
                     (point-max)))))
    (put-text-property css-start end-tag 'face 'font-lock-doc-face)
    (put-text-property css-start end-tag 'fontified 't)
    (goto-char tag-end)
    (while (re-search-forward
            (concat "{\\|\\(\\(?:\\sw\\|-\\)+\\)"
                    "\\(?:\\([:!#]\\)\\(\\(?:\\sw\\|-\\)+\\)\\)?"
                    ws-re "*\\(,?\\)")
            end-tag t)
      (cond ((string= (match-string-no-properties 0) "{")
             (remove-text-properties (match-beginning 0) (match-end 0) 
                                     '(face nil))
             (let ((end-brace (or (save-excursion
                                    (re-search-forward "}" end-tag 
                                                       t))
                                  end-tag)))
               (when (= (char-before end-brace) ?})
                 (put-text-property (1- end-brace) end-brace 'face 
                                    nil))
               (while (re-search-forward
                       (concat "\\(\\(?:\\sw\\|-\\)+\\)" ws-re 
                               "*\\(:\\)" ws-re "*\\([^;}]*\\)"
                               "\\(;?\\)")
                       end-brace t)
                 (put-text-property (match-beginning 1) 
                                    (match-end 1) 'face 
                                    'font-lock-keyword-face)
                 (put-text-property (match-beginning 1) 
                                    (match-end 1) 'fontified t)
                 (when (match-beginning 2)
                   (remove-text-properties (match-beginning 2) (match-end 2) 
                                           '(face nil)))
                 (when (match-beginning 3)
                   (put-text-property (match-beginning 3)
                                      (match-end 3) 'face 
                                      'font-lock-constant-face)
                   (put-text-property (match-beginning 3)
                                      (match-end 3) 'fontified t))
                 (when (match-beginning 4)
                   (remove-text-properties (match-beginning 4) (match-end 4) 
                                           '(face nil))))
               (goto-char end-brace)))
            (t (put-text-property (match-beginning 1) (match-end 1) 
                                  'face 'font-lock-builtin-face)
               (put-text-property (match-beginning 1) (match-end 1) 
                                  'fontified 't)
               (when (match-beginning 2)
                 (remove-text-properties (match-beginning 2) (match-end 2) 
                                         '(face nil))
                 (put-text-property (match-beginning 3) 
                                    (match-end 3) 'face
                                    'font-lock-function-name-face)
                 (put-text-property (match-beginning 3) 
                                    (match-end 3) 'fontified 't))
               (when (match-beginning 4)
                 (put-text-property (match-beginning 4) 
                                    (match-end 4) 'face 
                                    nil)))))))

(defun php-syntactic-face-function-process-javascript ()
  (when verbose
    (message "SCRIPT tag found, highlighting Javascript."))
  (forward-char)
  (let ((end-tag (save-excursion 
                   (if (re-search-forward "<\\?\\|</script>" nil t)
                       (match-beginning 0)
                     (point-max)))))
    (remove-text-properties tag-end end-tag '(face nil))))

(provide 'php-font-lock)
