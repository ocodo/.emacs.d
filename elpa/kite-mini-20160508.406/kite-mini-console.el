(require 'cl)
(require 'comint)
(require 'kite-mini)

(require 'js) ; for syntax highlighting

(defface kite-mini-log-warning
  '((t :inherit warning))
  "Basic face used to highlight warnings."
  :version "24.1"
  :group 'kite-mini-faces)

(defface kite-mini-log-error
  '((t :inherit error))
  "Basic face used to highlight errors."
  :version "24.1"
  :group 'kite-mini-faces)

(defface kite-mini-log-debug
  '((t :inherit font-lock-comment))
  "Basic face used to highlight debug-level messages."
  :version "24.1"
  :group 'kite-mini-faces)

(defface kite-mini-log-log
  '((t :inherit default))
  "Basic face used to highlight regular messages."
  :version "24.1"
  :group 'kite-mini-faces)

(defcustom kite-mini-console-prompt "JS> "
  "Prompt used in kite-mini-console."
  :group 'kite-mini)

(defvar kite-mini-console-mode-map
  (let ((map (copy-keymap widget-keymap))
	(menu-map (make-sparse-keymap)))
    ;;(suppress-keymap map t)
    (define-key map "\t" 'kite-mini-async-completion-at-point)
    (define-key map "\C-cX" 'kite-clear-console)
    (define-key map "\C-cg" 'kite-console-visit-source)
    (define-key map "\C-ci" 'kite-show-log-entry)
    (define-key map "\C-j" 'kite-mini-console-send-input)
    (define-key map (kbd "RET") 'kite-mini-console-send-input)
    map)
  "Local keymap for `kite-console-mode' buffers.")

(defvar kite-mini-console-input)

(define-derived-mode kite-mini-console-mode comint-mode "kite-mini-console"
  "Provide a REPL into the visiting browser."
  :group 'kite-mini
  :syntax-table emacs-lisp-mode-syntax-table
  (setq comint-prompt-regexp (concat "^" (regexp-quote kite-mini-console-prompt))
        comint-get-old-input 'kite-mini-console-get-old-input ;; TODO: why?
        comint-input-sender 'kite-mini-console-input-sender
        comint-process-echoes nil)
  ;; (set (make-local-variable 'comint-prompt-read-only) t)
  (unless (comint-check-proc (current-buffer))
    (start-process "kite-mini-console" (current-buffer) nil)
    (set-process-query-on-exit-flag (kite-mini-console-process) nil)

    (set (make-local-variable 'font-lock-defaults)
         (list js--font-lock-keywords))

    (goto-char (point-max))
    (set (make-local-variable 'comint-inhibit-carriage-motion) t)
    (comint-output-filter (kite-mini-console-process) kite-mini-console-prompt)
    (set-process-filter (kite-mini-console-process) 'comint-output-filter)))

(defun kite-mini-console-append (data)
  (let ((buffer (get-buffer "*kite-mini-console*")))
    (when buffer
      (with-current-buffer buffer
        (comint-output-filter (kite-mini-console-process) (concat data "\n"))))))

(defun kite-mini-console-process ()
  ;; Return the current buffer's process.
  (get-buffer-process (current-buffer)))

(defun kite-mini-console-get-old-input nil
  ;; Return the previous input surrounding point
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

(defun kite-mini-console-input-sender (_proc input)
  ;; Just sets the variable kite-mini-console-input, which is in the scope
  ;; of `kite-mini-console-send-input's call.
  (setq kite-mini-console-input input))

(defun kite-mini-console-send-input ()
  "Evaluate the current console prompt input."
  (interactive)
  (let (kite-mini-console-input)	       	; set by
                                        ; kite-console-input-sender
    (comint-send-input)			; update history, markers etc.
    (kite-mini-console-eval-input kite-mini-console-input)))

(defun kite-mini-console-eval-input (input)
  (kite-mini-send-eval
   input
   (lambda (result)
     (if (eq :json-false (plist-get result :wasThrown))
         (comint-output-filter
          (kite-mini-console-process)
          (format "%s\n%s"
                  (plist-get (plist-get result :result) :value)
                  kite-mini-console-prompt))
       ;; TODO: fix and release object?
       (format "Error: %s\n%s"
               result
               kite-mini-console-prompt)))))
;; (let ((object-id
;;        (kite--get result :result :objectId)))
;;   (when object-id
;;     (kite--release-object object-id)))

;; (defun kite-mini--eval-in-current-context (input success-function)
;;   "Evaluate INPUT in the remote remote debugger in the current
;; execution context and asynchronously invoke SUCCESS-FUNCTION with
;; the results in case of success."
;;   (let ((eval-params (list :expression input))
;;         (context-id (plist-get (kite-session-current-context
;;                                 kite-session)
;;                                :id)))
;;     (when context-id
;;       (setq eval-params (plist-put eval-params :contextId context-id)))
;;     (kite-send
;;      "Runtime.evaluate"
;;      :params
;;      eval-params
;;      :success-function
;;      success-function)))


(defconst kite-mini--identifier-part-regexp
  (rx
   word-boundary
   (1+ (or alnum
           ?.
           (: "\\x" (repeat 2 xdigit))
           (: "\\u" (repeat 4 xdigit))))
   point)
  "Used by `kite-async-completion-at-point' to find a part of a
JavaScript identifier.")

(defun kite-mini-async-completion-at-point ()
  "Asynchronously fetch completions for the JavaScript expression
at point and, once results have arrived, perform completion using
`completion-in-region'.

Note: we can't use the usual mechanism of hooking into the
completions API (`completion-at-point-functions') because it
doesn't support asynchronicity."
  (interactive)
  (let (completion-begin)

    ;; Find the dotted JavaScript expression (consisting of
    ;; identifiers only) before point.  Note that we can't use just a
    ;; single regex because greedy regexes don't work when searching
    ;; backwards.
    (save-excursion
      (save-match-data
        (while (re-search-backward kite-mini--identifier-part-regexp nil t))
        (setq completion-begin (point))))

    ;; FIXME: the previous step is too broad, it will find identifiers
    ;; starting with a digit.  Could do a second pass here to make
    ;; sure that we're looking at a valid expression, or improve error
    ;; handling in `kite--get-properties-fast' to ensure that we do
    ;; the right thing when the JavaScript side gets back to us with a
    ;; complaint.

    (when (< completion-begin (point))
      (let* ((components (split-string (buffer-substring-no-properties
                                        completion-begin
                                        (point))
                                       "\\."))
             (last-component (car (last components))))

        (lexical-let ((lex-completion-begin (- (point)
                                               (length last-component)))
                      (lex-completion-end (point)))
          (kite-mini--get-properties-fast
           (if (> (length components) 1)
               (mapconcat 'identity
                          (subseq components
                                  0
                                  (- (length components) 1))
                          ".")
             "window")
           (concat "^" (regexp-quote last-component))
           (lambda (completions)
             (let* (completion-extra-properties
                    completion-in-region-mode-predicate)
               (completion-in-region
                lex-completion-begin
                lex-completion-end
                completions)))))))))

(defun kite-mini--get-properties-fast (object-expr js-regex callback)
  "Efficiently and asynchronously fetch matching property names
for the object resulting from evaluating OBJECT-EXPR, a
JavaScript expression.  Only properties matching JS-REGEX, a
regular expression using JavaScript syntax, are fetched.  The
resulting property names are passed as an unsorted list of
strings to CALLBACK, which should accept a single parameter.

FIXME: no error handling."
  (lexical-let ((lex-callback callback))
    (kite-mini-send-eval
     (format "(function(val) {
  var regex = new RegExp('%s')
  var test = regex.test.bind(regex)
  var keys = new Set
  for (var key in val) regex.test(key) && keys.add(key)
  Object.getOwnPropertyNames(val).forEach(key => regex.test(key) && keys.add(key))
  return Array.from(keys)
})(%s)"
             js-regex
             object-expr)
     (lambda (result)
       (funcall lex-callback (plist-get (plist-get result :result) :value))))))

(defun kite-mini--release-object (object-id)
  "Release the object with the given OBJECT-ID on the browser
side."
  (when (null object-id)
    (error "kite--release-object called with null OBJECT-ID"))
  (kite-mini-call-rpc "Runtime.releaseObject"
                      `((objectId . ,object-id))))

(defun kite-mini-console ()
  "Start a kite mini console."
  (interactive)
  (when (not (get-buffer "*kite-mini-console*"))
    (with-current-buffer (get-buffer-create "*kite-mini-console*")
      (kite-mini-console-mode)))
  (pop-to-buffer (get-buffer "*kite-mini-console*")))

(provide 'kite-mini-console)
