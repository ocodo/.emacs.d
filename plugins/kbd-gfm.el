;;; kbd-gfm.el --- Reformat kbd strings for use with Github Formatted Markdown

;; Author: Jason Milkins <jasonm23@@gmail.com>

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
(defvar kbd-modifier-list
  '(:M "Meta" :C "Ctrl" :S "Shift" :s "Super" :H "Hyper")
  "List of modifiers to their expanded names.")

;;;###autoload
(defun kbd-expanded-emacs-modifier (modifier)
  "Convert an Emacs MODIFIER to it's long form.
Expansion is defined in `kbd-modifier-list'."
  (plist-get kbd-modifier-list (intern (concat  ":" modifier))))

;;;###autoload
(defun kbd-key-to-gfm-kbd (key)
  "Take a KEY and wrap it in a <kbd></kbd> tag."
  (format "<kbd>%s</kbd>" key))

;;;###autoload
(defun key-chord-to-gfm-kbd (key-chord &optional long-modifiers)
  "Convert a KEY-CHORD to gfm kbd element style.

When LONG-MODIFIERS is non-nil convert modifiers to long form.

Uses `kbd-modifier-list' to define long form."
  (let ((modifiers (if long-modifiers
                       (--map (kbd-expanded-emacs-modifier it) (butlast key-chord))
                     (butlast key-chord)))
        (last-key (car (last key-chord))))
    (s-join "-" (-flatten (list (-map 'kbd-key-to-gfm-kbd modifiers) (kbd-key-to-gfm-kbd last-key))))))

;;;###autoload
(defun kbd-string-to-gfm-kbd (string &optional long-modifiers)
  "Convert a kbd binding STRING to github flavored markdown <kbd></kbd> style.
if LONG-MODIFIERS is non-nil expand modifier using `kbd-expanded-emacs-modifier'"
  (let* ((groups (s-split " " string))
         (keys (--map (s-split "-" it) groups)))
    (message "%S" keys)
    (s-join " " (--map (key-chord-to-gfm-kbd it long-modifiers) keys))))

(provide 'kbd-gfm)

;;; kbd-gfm.el ends here
