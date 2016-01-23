;;; highlight-leading-spaces.el --- Highlight leading spaces

;; Copyright (C) 2015 by Thomas Winant

;; Author: Thomas Winant <dewinant@gmail.com>
;; URL: https://github.com/mrBliss/highlight-leading-spaces
;; Package-Version: 20151216.422
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Created: Dec 14 2015

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This minor-mode highlights leading spaces with a face and replaces them
;; with a special character so they can easily be counted (this can be
;; disabled).

;; The whitespace.el package that is built-in to Emacs
;; (http://www.emacswiki.org/emacs/WhiteSpace) can be used to accomplish the
;; same effect, but for *all* spaces, including non-leading spaces between
;; words.  This minor-mode only highlights leading spaces that are part of the
;; indentation.

;; The face to highlight leading spaces with can be customised by changing the
;; highlight-leading-spaces face.  The character to replace leading spaces
;; with can be customised by changing `highlight-leading-spaces-char'.  If you
;; do not wish to them to be replaced with a special character, set it to a
;; space.

;; Suggested usage:
;;
;;     (add-hook 'prog-mode-hook 'highlight-leading-spaces-mode)

;; This minor-mode is quite efficient because it doesn't use overlays but text
;; properties for the leading spaces.  Furthermore, the highlights are
;; correctly and efficiently kept up-to-date by plugging in to font-lock, not
;; by adding various hooks.

;;; Code:


(defgroup highlight-leading-spaces nil
  "Highlight leading spaces."
  :group 'basic-faces)

(defcustom highlight-leading-spaces-char ?·
  "Character used to replace leading spaces.
Defaults to ?· (183).  To not replace leading spaces and only
highlight them with a face, set this to a space (32)."
  :type 'character
  :group 'highlight-leading-spaces)

(defface highlight-leading-spaces
  '((t (:inherit font-lock-comment-face)))
  "Face for leading spaces."
  :group 'highlight-leading-spaces)

(defvar-local highlight-leading-spaces--enabled nil
  "Buffer-local variable that indicates whether
  `highlight-leading-spaces-mode' is enabled in the buffer.")


(defun highlight-leading-spaces--highlight (beg end &optional loudly)
  "Highlight leading spaces.
When `highlight-leading-spaces--enabled' is non-nil, highlight
the leading spaces in the region from BEG to END.  The third
argument LOUDLY is ignored."
  (when highlight-leading-spaces--enabled
    (let ((inhibit-point-motion-hooks t)
          ;; When consecutive characters have the same string as the display
          ;; text property, they will be treated as one unit. The undesired
          ;; result of this is that all n leading spaces on one line are
          ;; replaced by just one `highlight-leading-spaces-char' instead of
          ;; n, consequently, the indentation disappears. To avoid this, we
          ;; use `make-string' to allocate two different one char strings. We
          ;; then alternate between the two strings when adding the text
          ;; properties so that no two consecutive highlighted spaces have the
          ;; same display string.
          (str1 (make-string 1 highlight-leading-spaces-char))
          (str2 (make-string 1 highlight-leading-spaces-char))
          ;; We toggle this bool to alternate between str1 and str2.
          (which-str t))
      (with-silent-modifications
        (goto-char beg)
        ;; Search for the first leading space.
        (while (re-search-forward "^[ ]" end t)
          ;; The search stopped after the first leading space, so look back.
          (while (eq 32 (char-before))
            (add-text-properties
             (1- (point)) (point)
             ;; Only when a different character is chosen should we change the
             ;; display text property of the leading space.
             (if (eq 32 highlight-leading-spaces-char)
                 '(face highlight-leading-spaces)
               `(face highlight-leading-spaces display
                      ,(if (setq which-str (not which-str))
                           str1 str2))))
            (forward-char 1)))))))


;;;###autoload
(define-minor-mode highlight-leading-spaces-mode
  "Highlight leading spaces."
  :init-value nil
  :lighter " ·"
  :global nil
  (if highlight-leading-spaces-mode

      ;; Enable
      (progn
        (setq-local highlight-leading-spaces--enabled t)
        ;; Indicate that font-lock should remove the display text property
        ;; when unfontifying because we add it when fontifying.
        (make-local-variable 'font-lock-extra-managed-props)
        (add-to-list 'font-lock-extra-managed-props 'display)
        ;; The advice only has to be added once globally, but this operation
        ;; is idempotent, so it does no harm here.
        (advice-add 'font-lock-fontify-region
                    :after #'highlight-leading-spaces--highlight)
        (font-lock-fontify-buffer))

    ;; Disable

    ;; Note that we leave the advice in place. If we were to remove it,
    ;; leading spaces would no longer be highlighted in other buffers that
    ;; have this mode still enabled. The advice will do nothing unless
    ;; `highlight-leading-spaces--enabled' is non-nil.
    (setq-local highlight-leading-spaces--enabled nil)

    ;; Unfontify to undo what we've done.
    (font-lock-unfontify-buffer)
    ;; The following line has to come after unfontifying, otherwise the
    ;; display text property is not removed.
    (kill-local-variable 'font-lock-extra-managed-props)
    ;; Refontify the buffer.
    (font-lock-fontify-buffer)))


(provide 'highlight-leading-spaces)
;;; highlight-leading-spaces.el ends here
