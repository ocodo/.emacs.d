;;; lsp-semantic-tokens.el --- Semantic tokens -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 emacs-lsp maintainers
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  Semantic tokens
;;  https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens
;;
;;; Code:

(require 'lsp-mode)

(defgroup lsp-semantic-tokens nil
  "LSP support for semantic-tokens."
  :prefix "lsp-semantic-tokens-"
  :group 'lsp-mode
  :tag "LSP Semantic tokens")

(define-obsolete-variable-alias 'lsp-semantic-highlighting-warn-on-missing-face 'lsp-semantic-tokens-warn-on-missing-face "lsp-mode 8.0.0")

(defcustom lsp-semantic-tokens-warn-on-missing-face nil
  "Warning on missing face for token type/modifier.
When non-nil, this option will emit a warning any time a token
or modifier type returned by a language server has no face associated with it."
  :group 'lsp-semantic-tokens
  :type 'boolean)

(defcustom lsp-semantic-tokens-apply-modifiers nil
  "Whether semantic tokens should take token modifiers into account."
  :group 'lsp-semantic-tokens
  :type 'boolean)

(defcustom lsp-semantic-tokens-allow-ranged-requests t
  "Whether to use ranged semantic token requests when available.

Note that even when this is set to t, delta requests will
be preferred whenever possible, unless
`lsp-semantic-tokens-allow-delta-requests' is false."
  :group 'lsp-semantic-tokens
  :type 'boolean)

(defcustom lsp-semantic-tokens-allow-delta-requests t
  "Whether to use semantic token delta requests when available.

When supported by the language server, delta requests are always
preferred over both full and ranged token requests."
  :group 'lsp-semantic-tokens
  :type 'boolean)

(defcustom lsp-semantic-tokens-honor-refresh-requests nil
  "Whether to honor semanticTokens/refresh requests.

When set to nil, refresh requests will be silently discarded.
When set to t, semantic tokens will be re-requested for all buffers
associated with the requesting language server."
  :group 'lsp-semantic-tokens
  :type 'boolean)

(defface lsp-face-semhl-constant
  '((t :inherit font-lock-constant-face))
  "Face used for semantic highlighting scopes matching constant scopes."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-variable
  '((t :inherit font-lock-variable-name-face))
  "Face used for semantic highlighting scopes matching variable.*.
Unless overridden by a more specific face association."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-function
  '((t :inherit font-lock-function-name-face))
  "Face used for semantic highlighting scopes matching entity.name.function.*.
Unless overridden by a more specific face association."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-method
  '((t :inherit lsp-face-semhl-function))
  "Face used for semantic highlighting scopes matching entity.name.method.*.
Unless overridden by a more specific face association."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-namespace
  '((t :inherit font-lock-type-face :weight bold))
  "Face used for semantic highlighting scopes matching entity.name.namespace.*.
Unless overridden by a more specific face association."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-comment
  '((t (:inherit font-lock-comment-face)))
  "Face used for comments."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-keyword
  '((t (:inherit font-lock-keyword-face)))
  "Face used for keywords."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-string
  '((t (:inherit font-lock-string-face)))
  "Face used for keywords."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-number
  '((t (:inherit font-lock-constant-face)))
  "Face used for numbers."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-regexp
  '((t (:inherit font-lock-string-face :slant italic)))
  "Face used for regexps."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-operator
  '((t (:inherit font-lock-function-name-face)))
  "Face used for operators."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-namespace
  '((t (:inherit font-lock-keyword-face)))
  "Face used for namespaces."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-type
  '((t (:inherit font-lock-type-face)))
  "Face used for types."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-struct
  '((t (:inherit font-lock-type-face)))
  "Face used for structs."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-class
  '((t (:inherit font-lock-type-face)))
  "Face used for classes."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-interface
  '((t (:inherit font-lock-type-face)))
  "Face used for interfaces."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-enum
  '((t (:inherit font-lock-type-face)))
  "Face used for enums."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-type-parameter
  '((t (:inherit font-lock-type-face)))
  "Face used for type parameters."
  :group 'lsp-semantic-tokens)

;; function face already defined, move here when support
;; for theia highlighting gets removed
(defface lsp-face-semhl-member
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for members."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-property
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for properties."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-event
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for event properties."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-macro
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used for macros."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-variable
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for variables."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-parameter
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for parameters."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-label
  '((t (:inherit font-lock-comment-face)))
  "Face used for labels."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-deprecated
  '((t :strike-through t))
  "Face used for semantic highlighting scopes matching constant scopes."
  :group 'lsp-semantic-tokens)

(defvar lsp-semantic-token-faces
  '(("comment" . lsp-face-semhl-comment)
    ("keyword" . lsp-face-semhl-keyword)
    ("string" . lsp-face-semhl-string)
    ("number" . lsp-face-semhl-number)
    ("regexp" . lsp-face-semhl-regexp)
    ("operator" . lsp-face-semhl-operator)
    ("namespace" . lsp-face-semhl-namespace)
    ("type" . lsp-face-semhl-type)
    ("struct" . lsp-face-semhl-struct)
    ("class" . lsp-face-semhl-class)
    ("interface" . lsp-face-semhl-interface)
    ("enum" . lsp-face-semhl-enum)
    ("typeParameter" . lsp-face-semhl-type-parameter)
    ("function" . lsp-face-semhl-function)
    ("method" . lsp-face-semhl-method)
    ("member" . lsp-face-semhl-member)
    ("property" . lsp-face-semhl-property)
    ("event" . lsp-face-semhl-event)
    ("macro" . lsp-face-semhl-macro)
    ("variable" . lsp-face-semhl-variable)
    ("parameter" . lsp-face-semhl-parameter)
    ("label" . lsp-face-semhl-label)
    ("enumConstant" . lsp-face-semhl-constant)
    ("enumMember" . lsp-face-semhl-constant)
    ("dependent" . lsp-face-semhl-type)
    ("concept" . lsp-face-semhl-interface))
  "Faces to use for semantic tokens.")

(defvar lsp-semantic-token-modifier-faces
  ;; TODO: add default definitions
  '(("declaration" . lsp-face-semhl-interface)
    ("deprecated" . lsp-face-semhl-deprecated)
    ("readonly" . lsp-face-semhl-constant))
  "Semantic tokens modifier faces.
Faces to use for semantic token modifiers if
`lsp-semantic-tokens-apply-modifiers' is non-nil.")

(defvar lsp-semantic-tokens-capabilities
  `((semanticTokens
     . ((dynamicRegistration . t)
        (requests . ((range . t) (full . t)))
        (tokenModifiers . ,(if lsp-semantic-tokens-apply-modifiers
                               (apply 'vector (mapcar #'car lsp-semantic-token-modifier-faces)) []))
        (tokenTypes . ,(apply 'vector (mapcar #'car lsp-semantic-token-faces)))
        (formats . ["relative"])))))


(defvar lsp--semantic-tokens-idle-timer nil)

(defvar-local lsp--semantic-tokens-cache nil
  "Previously returned token set.

When non-nil, `lsp--semantic-tokens-cache' should adhere to the
following lsp-interface:
`(_SemanticTokensCache
  (:_documentVersion :_ranged)
  (:response :_region))'.")

(defsubst lsp--semantic-tokens-putcache (k v)
  "Set key K of `lsp--semantic-tokens-cache' to V."
  (setq lsp--semantic-tokens-cache
        (plist-put lsp--semantic-tokens-cache k v)))

(defvar-local lsp--semantic-tokens-teardown nil)

(defun lsp--semantic-tokens-request-full-token-set-when-idle (fontify-immediately)
  "Request full token set after an idle timeout of `lsp-idle-delay'.

If FONTIFY-IMMEDIATELY is non-nil, fontification will be performed immediately
 once the corresponding response is received."
  (when lsp--semantic-tokens-idle-timer (cancel-timer lsp--semantic-tokens-idle-timer))
  (let ((buf (current-buffer)))
    (setq lsp--semantic-tokens-idle-timer
          (run-with-idle-timer
           lsp-idle-delay
           nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (lsp--semantic-tokens-request nil fontify-immediately))))))))

(defun lsp--semantic-tokens-ingest-range-response (response)
  "Handle RESPONSE to semanticTokens/range request."
  (lsp--semantic-tokens-putcache :response response)
  (lsp--semantic-tokens-putcache :_ranged t)
  (lsp--semantic-tokens-request-full-token-set-when-idle nil))

(defun lsp--semantic-tokens-ingest-full-response (response)
  "Handle RESPONSE to semanticTokens/full request."
  (lsp--semantic-tokens-putcache :response response)
  (lsp--semantic-tokens-putcache :_ranged nil))

(defsubst lsp--semantic-tokens-apply-delta-edits (old-data edits)
  "Apply EDITS obtained from full/delta request to OLD-DATA."
  (let* ((old-token-count (length old-data))
         (old-token-index 0)
         (substrings))
    (cl-loop
     for edit across edits
     do
     (when (< old-token-index (lsp-get edit :start))
       (push (substring old-data old-token-index (lsp-get edit :start)) substrings))
     (push (lsp-get edit :data) substrings)
     (setq old-token-index (+ (lsp-get edit :start) (lsp-get edit :deleteCount)))
     finally do (push (substring old-data old-token-index old-token-count) substrings))
    (apply #'vconcat (nreverse substrings))))

(defun lsp--semantic-tokens-ingest-full/delta-response (response)
  "Handle RESPONSE to semanticTokens/full/delta request."
  (if (lsp-get response :edits)
      (let ((old-data (--> lsp--semantic-tokens-cache (plist-get it :response) (lsp-get it :data))))
        (when old-data
          (lsp--semantic-tokens-putcache
           :response (lsp-put response
                              :data (lsp--semantic-tokens-apply-delta-edits
                                     old-data (lsp-get response :edits))))
          (lsp--semantic-tokens-putcache :_ranged nil)))
    ;; server decided to send full response instead
    (lsp--semantic-tokens-ingest-full-response response)))


(defun lsp--semantic-tokens-request (region fontify-immediately)
  "Send semantic tokens request to the language server.

A full/delta request will be sent if delta requests are supported by
the language server, allowed via `lsp-semantic-tokens-allow-delta-requests',
and if a full set of tokens had previously been received.
Otherwise, a ranged request will be dispatched if REGION is non-nil,
ranged requests are supported by the language server, and allowed via
`lsp-semantic-tokens-allow-delta-requests'. In all other cases, a full
tokens request will be dispatched.

If FONTIFY-IMMEDIATELY is non-nil, fontification will be performed immediately
 upon receiving the response."
  (let ((request-type "textDocument/semanticTokens/full")
        (request `(:textDocument ,(lsp--text-document-identifier)))
        (response-handler nil))
    (cond
     ((and lsp-semantic-tokens-allow-delta-requests
           (lsp-feature? "textDocument/semanticTokensFull/Delta")
           (--> lsp--semantic-tokens-cache
             (plist-get it :response)
             (and (lsp-get it :resultId) (lsp-get it :data) (not (lsp-get it :_ranged)))))
      (setq request-type "textDocument/semanticTokens/full/delta")
      (setq response-handler #'lsp--semantic-tokens-ingest-full/delta-response)
      (setq request
            (plist-put request :previousResultId
                       (lsp-get (plist-get lsp--semantic-tokens-cache :response) :resultId))))
     ((and lsp-semantic-tokens-allow-ranged-requests region
           (lsp-feature? "textDocument/semanticTokensRangeProvider"))
      (setq request-type "textDocument/semanticTokens/range")
      (setq request
            (plist-put request :range (lsp--region-to-range (car region) (cdr region))))
      (setq response-handler #'lsp--semantic-tokens-ingest-range-response))
     (t (setq response-handler #'lsp--semantic-tokens-ingest-full-response)))
    (when lsp--semantic-tokens-idle-timer (cancel-timer lsp--semantic-tokens-idle-timer))
    (lsp-request-async
     request-type request
     (lambda (response)
       (lsp--semantic-tokens-putcache :_documentVersion lsp--cur-version)
       (funcall response-handler response)
       (when fontify-immediately (font-lock-flush)))
     :error-handler ;; buffer is not captured in `error-handler', it is in `callback'
     (let ((buf (current-buffer)))
       (lambda (&rest _)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (lsp--semantic-tokens-request-full-token-set-when-idle t)))))
     :mode 'tick
     :cancel-token (format "semantic-tokens-%s" (lsp--buffer-uri)))))


(defun lsp-semantic-tokens--fontify (old-fontify-region beg-orig end-orig &optional loudly)
  "Apply fonts to retrieved semantic tokens.
OLD-FONTIFY-REGION is the underlying region fontification function,
e.g., `font-lock-fontify-region'.
BEG-ORIG and END-ORIG deliminate the requested fontification region and maybe
modified by OLD-FONTIFY-REGION.
LOUDLY will be forwarded to OLD-FONTIFY-REGION as-is."
  ;; TODO: support multiple language servers per buffer?
  (let ((faces (seq-some #'lsp--workspace-semantic-tokens-faces lsp--buffer-workspaces))
        (modifier-faces
         (when lsp-semantic-tokens-apply-modifiers
           (seq-some #'lsp--workspace-semantic-tokens-modifier-faces lsp--buffer-workspaces)))
        old-bounds
        beg end)
    (cond
     ((or (eq nil faces)
          (eq nil lsp--semantic-tokens-cache)
          (eq nil (plist-get lsp--semantic-tokens-cache :response)))
      ;; default to non-semantic highlighting until first response has arrived
      (funcall old-fontify-region beg-orig end-orig loudly))
     ((not (= lsp--cur-version (plist-get lsp--semantic-tokens-cache :_documentVersion)))
      ;; delay fontification until we have fresh tokens
      '(jit-lock-bounds 0 . 0))
     (t
      (setq old-bounds (funcall old-fontify-region beg-orig end-orig loudly))
      ;; this is to prevent flickering when semantic token highlighting
      ;; is layered on top of, e.g., tree-sitter-hl, or clojure-mode's syntax highlighting.
      (setq beg (min beg-orig (cadr old-bounds))
            end (max end-orig (cddr old-bounds)))
      (-let* ((inhibit-field-text-motion t)
              (data (lsp-get (plist-get lsp--semantic-tokens-cache :response) :data))
              (i0 0)
              (i-max (1- (length data)))
              (current-line 1)
              (line-delta)
              (column 0)
              (face)
              (line-start-pos)
              (line-min)
              (line-max-inclusive)
              (text-property-beg)
              (text-property-end))
        (save-mark-and-excursion
          (save-restriction
            (widen)
            (goto-char beg)
            (goto-char (line-beginning-position))
            (setq line-min (line-number-at-pos))
            (with-silent-modifications
              (goto-char end)
              (goto-char (line-end-position))
              (setq line-max-inclusive (line-number-at-pos))
              (forward-line (- line-min line-max-inclusive))
              (let ((skip-lines (- line-min current-line)))
                (while (and (<= i0 i-max) (< (aref data i0) skip-lines))
                  (setq skip-lines (- skip-lines (aref data i0)))
                  (setq i0 (+ i0 5)))
                (setq current-line (- line-min skip-lines)))
              (forward-line (- current-line line-min))
              (setq line-start-pos (point))
              (cl-loop
               for i from i0 to i-max by 5 do
               (setq line-delta (aref data i))
               (unless (= line-delta 0)
                 (forward-line line-delta)
                 (setq line-start-pos (point))
                 (setq column 0)
                 (setq current-line (+ current-line line-delta)))
               (setq column (+ column (aref data (1+ i))))
               (setq face (aref faces (aref data (+ i 3))))
               (setq text-property-beg (+ line-start-pos column))
               (setq text-property-end (+ text-property-beg (aref data (+ i 2))))
               (when face
                 (put-text-property text-property-beg text-property-end 'face face))
               (cl-loop for j from 0 to (1- (length modifier-faces)) do
                        (when (and (aref modifier-faces j)
                                   (> (logand (aref data (+ i 4)) (lsh 1 j)) 0))
                          (add-face-text-property text-property-beg text-property-end
                                                  (aref modifier-faces j))))
               when (> current-line line-max-inclusive) return nil)))))
      (let ((token-region (plist-get lsp--semantic-tokens-cache :_region)))
        (if token-region
            `(jit-lock-bounds ,(max beg (car token-region)) . ,(min end (cdr token-region)))
          `(jit-lock-bounds ,beg . ,end)))))))

(defun lsp-semantic-tokens--request-update ()
  "Request semantic-tokens update."
  (lsp--semantic-tokens-request (cons (window-start) (window-end)) t))

(defun lsp--semantic-tokens-as-defined-by-workspace (workspace)
  "Return plist of token-types and token-modifiers defined by WORKSPACE, or nil if none are defined."
  (when-let ((token-capabilities
              (or
               (-some->
                   (lsp--registered-capability "textDocument/semanticTokens")
                 (lsp--registered-capability-options))
               (lsp:server-capabilities-semantic-tokens-provider?
                (lsp--workspace-server-capabilities workspace)))))
    (-let* (((&SemanticTokensOptions :legend) token-capabilities))
      `(:token-types ,(lsp:semantic-tokens-legend-token-types legend)
        :token-modifiers ,(lsp:semantic-tokens-legend-token-modifiers legend)))))

(defun lsp-semantic-tokens-suggest-overrides ()
  "Suggest face overrides that best match the faces chosen by `font-lock-fontify-region'."
  (interactive)
  (-when-let* ((token-info (-some #'lsp--semantic-tokens-as-defined-by-workspace lsp--buffer-workspaces))
               ((&plist :token-types token-types :token-modifiers token-modifiers) token-info))
    (let* ((tokens (lsp-request
                    "textDocument/semanticTokens/full"
                    `(:textDocument, (lsp--text-document-identifier))))
           (inhibit-field-text-motion t)
           (data (lsp-get tokens :data))
           (associated-faces '())
           (line-delta)
           ;; KLUDGE: clear cache so our font-lock advice won't apply semantic-token faces
           (old-cache lsp--semantic-tokens-cache)
           (face-or-faces))
      (setq lsp--semantic-tokens-cache nil)
      (save-restriction
        (save-excursion
          (widen)
          (font-lock-fontify-region (point-min) (point-max) t)
          (save-mark-and-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (cl-loop
               for i from 0 to (1- (length data)) by 5 do
               (setq line-delta (aref data i))
               (unless (= line-delta 0) (forward-line line-delta))
               (forward-char (aref data (+ i 1)))
               (setq face-or-faces (get-text-property (point) 'face))
               ;; TODO: consider modifiers?
               (when face-or-faces
                 (--each (if (listp face-or-faces) face-or-faces (list face-or-faces))
                   (cl-pushnew `(,(aref data (+ i 3)) . ,it) associated-faces :test #'equal))))
              (setq lsp--semantic-tokens-cache old-cache)
              (font-lock-flush)))))
      (switch-to-buffer (get-buffer-create "*Suggested Overrides*"))
      (insert "(")
      ;; TODO: sort alternatives by frequency
      (--each-indexed (-group-by #'car associated-faces)
        (insert (if (= it-index 0) "(" "\n ("))
        (insert (format "%s . " (aref token-types (car it))))
        (--each-indexed (mapcar #'cdr (cdr it))
          (insert (if (= it-index 0) (format "%s)" (prin1-to-string it))
                    (format " ; Alternative: %s" (prin1-to-string it))))))
      (insert ")"))))


;;;###autoload
(defun lsp--semantic-tokens-initialize-buffer ()
  "Initialize the buffer for semantic tokens.
IS-RANGE-PROVIDER is non-nil when server supports range requests."
  (let* ((old-extend-region-functions font-lock-extend-region-functions)
         ;; make sure font-lock always fontifies entire lines (TODO: do we also have
         ;; to change some jit-lock-...-region functions/variables?)
         (new-extend-region-functions
          (if (memq 'font-lock-extend-region-wholelines old-extend-region-functions)
              old-extend-region-functions
            (cons 'font-lock-extend-region-wholelines old-extend-region-functions))))
    (setq lsp--semantic-tokens-cache nil)
    (setq font-lock-extend-region-functions new-extend-region-functions)
    (add-function :around (local 'font-lock-fontify-region-function) #'lsp-semantic-tokens--fontify)
    (add-hook 'lsp-on-change-hook #'lsp-semantic-tokens--request-update nil t)
    (lsp-semantic-tokens--request-update)
    (setq lsp--semantic-tokens-teardown
          (lambda ()
            (setq font-lock-extend-region-functions old-extend-region-functions)
            (when lsp--semantic-tokens-idle-timer
              (cancel-timer lsp--semantic-tokens-idle-timer)
              (setq lsp--semantic-tokens-idle-timer nil))
            (setq lsp--semantic-tokens-cache nil)
            (remove-function (local 'font-lock-fontify-region-function)
                             #'lsp-semantic-tokens--fontify)
            (remove-hook 'lsp-on-change-hook #'lsp-semantic-tokens--request-update t)))))

(defun lsp--semantic-tokens-build-face-map (identifiers faces category varname)
  "Build map of FACES for IDENTIFIERS using CATEGORY and VARNAME."
  (apply 'vector
         (mapcar (lambda (id)
                   (let ((maybe-face (cdr (assoc id faces))))
                     (when (and lsp-semantic-tokens-warn-on-missing-face (not maybe-face))
                       (lsp-warn "No face has been associated to the %s '%s': consider adding a corresponding definition to %s"
                                 category id varname)) maybe-face)) identifiers)))

(defun lsp-semantic-tokens--replace-alist-values (a b)
  "Replace alist A values with B ones where available."
  (-map
   (-lambda ((ak . av))
     (cons ak (alist-get ak b av nil #'string=)))
   a))

(defun lsp-semantic-tokens--type-faces-for (client)
  "Return the semantic token type faces for CLIENT."
  (lsp-semantic-tokens--replace-alist-values lsp-semantic-token-faces
                                             (plist-get (lsp--client-semantic-tokens-faces-overrides client) :types)))

(defun lsp-semantic-tokens--modifier-faces-for (client)
  "Return the semantic token type faces for CLIENT."
  (lsp-semantic-tokens--replace-alist-values lsp-semantic-token-modifier-faces
                                             (plist-get (lsp--client-semantic-tokens-faces-overrides client) :modifiers)))

(defun lsp--semantic-tokens-on-refresh (workspace)
  "Clear semantic tokens within all buffers of WORKSPACE, refresh in currently active buffer."
  (cl-assert (not (eq nil workspace)))
  (when lsp-semantic-tokens-honor-refresh-requests
    (cl-loop
     for ws-buffer in (lsp--workspace-buffers workspace) do
     (let ((fontify-immediately (equal (current-buffer) ws-buffer)))
       (with-current-buffer ws-buffer (lsp--semantic-tokens-request nil fontify-immediately))))))

;;;###autoload
(defun lsp--semantic-tokens-initialize-workspace (workspace)
  "Initialize semantic tokens for WORKSPACE."
  (cl-assert workspace)
  (-let (((&plist :token-types types :token-modifiers modifiers)
          (lsp--semantic-tokens-as-defined-by-workspace workspace))
         (client (lsp--workspace-client workspace)))
    (setf (lsp--workspace-semantic-tokens-faces workspace)
          (lsp--semantic-tokens-build-face-map
           types (lsp-semantic-tokens--type-faces-for client)
           "semantic token" "lsp-semantic-token-faces"))
    (setf (lsp--workspace-semantic-tokens-modifier-faces workspace)
          (lsp--semantic-tokens-build-face-map
           modifiers (lsp-semantic-tokens--modifier-faces-for client)
           "semantic token modifier" "lsp-semantic-token-modifier-faces"))))

;;;###autoload
(defun lsp-semantic-tokens--warn-about-deprecated-setting ()
  "Warn about deprecated semantic highlighting variable."
  (when (boundp 'lsp-semantic-highlighting)
    (pcase lsp-semantic-highlighting
      (:semantic-tokens
       (lsp-warn "It seems you wish to use semanticTokens-based
 highlighting. To do so, please remove any references to the
 deprecated variable `lsp-semantic-highlighting' from your
 configuration and set `lsp-semantic-tokens-enable' to `t'
 instead.")
       (setq lsp-semantic-tokens-enable t))
      ((or :immediate :deferred)
       (lsp-warn "It seems you wish to use Theia-based semantic
 highlighting. This protocol has been superseded by the
 semanticTokens protocol specified by LSP v3.16 and is no longer
 supported by lsp-mode. If your language server provides
 semanticToken support, please set
 `lsp-semantic-tokens-enable' to `t' to use it.")))))

;;;###autoload
(defun lsp-semantic-tokens--enable ()
  "Enable semantic tokens mode."
  (when (and lsp-semantic-tokens-enable
             (lsp-feature? "textDocument/semanticTokens"))
    (lsp-semantic-tokens--warn-about-deprecated-setting)
    (lsp-semantic-tokens-mode 1)))

(defun lsp-semantic-tokens--disable ()
  "Disable semantic tokens mode."
  (lsp-semantic-tokens-mode -1))

;;;###autoload
(define-minor-mode lsp-semantic-tokens-mode
  "Toggle semantic-tokens support."
  :group 'lsp-semantic-tokens
  :global nil
  (cond
   (lsp-semantic-tokens-mode
    (add-hook 'lsp-configure-hook #'lsp-semantic-tokens--enable nil t)
    (add-hook 'lsp-unconfigure-hook #'lsp-semantic-tokens--disable nil t)
    (mapc #'lsp--semantic-tokens-initialize-workspace
          (lsp--find-workspaces-for "textDocument/semanticTokens"))
    (lsp--semantic-tokens-initialize-buffer))
   (t
    (remove-hook 'lsp-configure-hook #'lsp-semantic-tokens--enable t)
    (remove-hook 'lsp-unconfigure-hook #'lsp-semantic-tokens--disable t)
    (when lsp--semantic-tokens-teardown
      (funcall lsp--semantic-tokens-teardown))
    (lsp-semantic-tokens--request-update)
    (setq lsp--semantic-tokens-idle-timer nil
          lsp--semantic-tokens-cache nil
          lsp--semantic-tokens-teardown nil))))

;; debugging helpers
(defun lsp--semantic-tokens-verify ()
  "Store current token set and compare with the response to a full token request."
  (interactive)
  (let ((old-tokens (--> lsp--semantic-tokens-cache (plist-get it :response) (lsp-get it :data)))
        (old-version (--> lsp--semantic-tokens-cache (plist-get it :_documentVersion))))
    (if (not (equal lsp--cur-version old-version))
        (message "Stored documentVersion %d differs from current version %d" old-version lsp--cur-version)
      (lsp-request-async
       "textDocument/semanticTokens/full" `(:textDocument ,(lsp--text-document-identifier))
       (lambda (response)
         (let ((new-tokens (lsp-get response :data)))
           (if (equal old-tokens new-tokens)
               (message "New tokens (total count %d) are identical to previously held token set"
                        (length new-tokens))
             (message "Newly returned tokens differ from old token set")
             (print old-tokens)
             (print new-tokens))))
       :mode 'tick
       :cancel-token (format "semantic-tokens-%s" (lsp--buffer-uri))))))

(defvar-local lsp-semantic-tokens--log '())

(defvar-local lsp-semantic-tokens--prev-response nil)

(defun lsp-semantic-tokens--log-buffer-contents (tag)
  "Log buffer contents for TAG."
  (save-restriction
    (save-excursion
      (widen) (push `(:tag ,tag
                      :buffer-contents ,(buffer-substring (point-min) (point-max))
                      :prev-response ,lsp-semantic-tokens--prev-response)
                    lsp-semantic-tokens--log))))

(defun lsp-semantic-tokens-enable-log ()
  "Enable logging of intermediate fontification states.

This is a debugging tool, and may incur significant performance penalties."
  (setq lsp-semantic-tokens--log '())
  (defadvice lsp-semantic-tokens--fontify (around advice-tokens-fontify activate)
    (lsp-semantic-tokens--log-buffer-contents 'before)
    (let ((result ad-do-it))
      (lsp-semantic-tokens--log-buffer-contents 'after)
      result))
  (defadvice lsp--semantic-tokens-ingest-full/delta-response
      (before log-delta-response (response) activate)
    (setq lsp-semantic-tokens--prev-response `(:request-type "delta"
                                               :response ,response
                                               :version ,lsp--cur-version)))
  (defadvice lsp--semantic-tokens-ingest-full-response
      (before log-full-response (response) activate)
    (setq lsp-semantic-tokens--prev-response `(:request-type "full"
                                               :response ,response
                                               :version ,lsp--cur-version)))
  (defadvice lsp--semantic-tokens-ingest-range-response
      (before log-range-response (response) activate)
    (setq lsp-semantic-tokens--prev-response `(:request-type "range"
                                               :response ,response
                                               :version ,lsp--cur-version))))

(defun lsp-semantic-tokens-disable-log ()
  "Disable logging of intermediate fontification states."
  (ad-unadvise 'lsp-semantic-tokens--fontify)
  (ad-unadvise 'lsp--semantic-tokens-ingest-full/delta-response)
  (ad-unadvise 'lsp--semantic-tokens-ingest-full-response)
  (ad-unadvise 'lsp--semantic-tokens-ingest-range-response))

(declare-function htmlize-buffer "ext:htmlize")

(defun lsp-semantic-tokens-export-log ()
  "Write HTML-formatted snapshots of previous fontification results to /tmp."
  (require 'htmlize)
  (let* ((outdir (f-join "/tmp" "semantic-token-snapshots"))
         (progress-reporter
          (make-progress-reporter
           (format "Writing buffer snapshots to %s..." outdir)
           0 (length lsp-semantic-tokens--log))))
    (f-mkdir outdir)
    (--each-indexed (reverse lsp-semantic-tokens--log)
      (-let* (((&plist :tag tag
                       :buffer-contents buffer-contents
                       :prev-response prev-response) it)
              (html-buffer))
        ;; FIXME: doesn't update properly; sit-for helps... somewhat,
        ;; but unreliably
        (when (= (% it-index 5) 0)
          (progress-reporter-update progress-reporter it-index)
          (sit-for 0.01))
        ;; we're emitting 2 snapshots (before & after) per update, so request
        ;; parameters should only change on every 2nd invocation
        (when (cl-evenp it-index)
          (with-temp-buffer
            (insert (prin1-to-string prev-response))
            (write-file (f-join outdir (format "parameters_%d.el" (/ it-index 2))))))
        (with-temp-buffer
          (insert buffer-contents)
          (setq html-buffer (htmlize-buffer))
          (with-current-buffer html-buffer
            ;; some configs such as emacs-doom may autoformat on save; switch to
            ;; fundamental-mode to avoid this
            (fundamental-mode)
            (write-file (f-join outdir (format "buffer_%d_%s.html" (/ it-index 2) tag)))))
        (kill-buffer html-buffer)))
    (progress-reporter-done progress-reporter)))

(lsp-consistency-check lsp-semantic-tokens)

(provide 'lsp-semantic-tokens)
;;; lsp-semantic-tokens.el ends here
