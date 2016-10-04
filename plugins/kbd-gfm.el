;;; kbd-gfm.el --- Reformat kbd strings for use with Github Formatted Markdown

;; Author: Jason Milkins <jasonm23@@gmail.com>
;; Version: 0.1.0

;; URL: https://github.com/ocodo/.emacs.d/tree/master/plugins/kbd-gfm.el

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
;;
;;  Reformat kbd strings for use with Github formatted
;;  Markdown.  Basically it wraps the parts of a kbd string in <kbd>
;;  tags.
;;

;;; Code:

(require 'dash)
(require 's)

;;;###autoload
(defvar kbd-gfm-modifier-list
  '(:M "Meta" :C "Ctrl" :S "Shift" :s "Super" :H "Hyper")
  "List of modifiers to their expanded names.")

;;;###autoload
(defun kbd-gfm-expanded-emacs-modifier (modifier)
  "Convert an Emacs MODIFIER to it's long form.
Expansion is defined in `kbd-gfm-modifier-list'."
  (plist-get kbd-gfm-modifier-list (intern (concat  ":" modifier))))

;;;###autoload
(defun kbd-gfm-key-to-gfm-kbd (key)
  "Take a KEY and wrap it in a <kbd></kbd> tag."
  (format "<kbd>%s</kbd>" key))

;;;###autoload
(defun kbd-gfm-key-chord-to-gfm-kbd (key-chord &optional long-modifiers)
  "Convert a KEY-CHORD to gfm kbd element style.

When LONG-MODIFIERS is non-nil convert modifiers to long form.

Uses `kbd-gfm-modifier-list' to define long form."
  (let ((modifiers (if long-modifiers
                       (--map (kbd-gfm-expanded-emacs-modifier it) (butlast key-chord))
                     (butlast key-chord)))
        (last-key (car (last key-chord))))
    (s-join "-" (-flatten (list (-map 'kbd-gfm-key-to-gfm-kbd modifiers) (kbd-gfm-key-to-gfm-kbd last-key))))))

;;;###autoload
(defun kbd-gfm-string-to-gfm (string &optional long-modifiers)
  "Convert a kbd binding STRING to github flavored markdown <kbd></kbd> style.
if LONG-MODIFIERS is non-nil expand modifier using `kbd-gfm-expanded-emacs-modifier'"
  (let* ((groups (s-split " " string))
         (keys (--map (s-split "-" it) groups)))
    (message "%S" keys)
    (s-join " " (--map (kbd-gfm-key-chord-to-gfm-kbd it long-modifiers) keys))))

(defun kbd-gfm-get-region-or-sexp-and-prefix (&optional override-prefix)
  "Get the region or current sexp and prefix for the `interactive' macro.

Note: `region-beginning' and `region-end' are the reason why an
`interactive' macro with \"r\" will blow up with the error:

\"The mark is not set now, so there is no region\"

Predicate `region-active-p' blocks calls to these functions when
there's no region.

When OVERRIDE-PREFIX is non-nil, return it instead of the true prefix."
  `(,@(if (region-active-p)
          (list (region-beginning) (region-end))
        (let ((bounds (bounds-of-thing-at-point 'sexp)))
                (if bounds
                    (list (car bounds) (cdr bounds))
                  '(nil nil))))
        ,(or override-prefix current-prefix-arg)))

(defun kbd-gfm-replace-with-gfm-kdb (begin end long)
  "Replace the region or sexp (BEGIN END) with the gfm kbd version.

if LONG is non-nil use extended modifier names."
  (interactive (kbd-gfm-get-region-or-sexp-and-prefix))
  (let* ((string (buffer-substring begin end))
         (replacement (kbd-gfm-string-to-gfm string long)))
    (kill-region begin end)
    (insert replacement)))

(defun kbd-gfm-replace-with-long-gfm-kbd ()
  "Replace the region or sexp with the long gfm kbd version."
  (interactive)
  (apply 'kbd-gfm-replace-with-gfm-kdb (kbd-gfm-get-region-or-sexp-and-prefix t)))

(provide 'kbd-gfm)

;;; kbd-gfm.el ends here
