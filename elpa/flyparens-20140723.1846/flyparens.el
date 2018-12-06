;;; flyparens.el --- Check for unbalanced parens on the fly

;; Author: Jisang Yoo
;; Created: 2014-06-28
;; Version: 0.5
;; EmacsWiki:
;; Github:
;; Keywords: faces, convenience, lisp, matching, parentheses, parens



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
;; flyparens provides a minor mode called flyparens-mode which checks
;; for unbalanced parens (or braces) on the fly (in Lisp code) and highlights the first
;; mismatched paren (at point or not). Also, you can customize to make it display
;; whether or not the parens in the buffer are balanced, via text cursor color or other means.
;;
;; See README for more detail (You might prefer reading it on the GitHub page).
;;




;;; Code:

;; Contributing guidelines:
;;
;; Make sure that any modification of this package stay within the following constraints:
;; + The package should not get confused when there are two Lisp buffers with this mode active.
;; + The package stays intended for absolute beginners. In particular:
;; + + The package should not rely on any external check tool that the user must manually install to his/her MS Windows machine.
;; + + The package should not conflict with rainbow-delimiters-mode, paredit, show-paren-mode (which are three modes that are recommended to beginners by many).
;; + + The package should be or can be customized to be usable on slow computers.

;; On feature contribution:
;;
;; If you wish to add a feature to this package, go through the following checklist first:
;; + Is the feature already covered by other existing packages?
;; + Would the inclusion of the new feature complicate beginners usage of this package?
;; + Can the feature be introduced by simply customizing the value of flyparens-function? (You can always share your customization with the community via EmacsWiki or blog or other means.)
;;


(defgroup flyparens nil
  "Check parens."
  :group 'paren-matching)

(defcustom flyparens-delay 0.3
  "Time (in seconds) after which to check parens."
  :group 'flyparens
  :type 'float)

(defface flyparens-mismatch-face
  '(((t (:inherit 'error))))
  "Face for first mismatched paren or brace."
  :group 'flyparens)

(defcustom flyparens-mismatch-overlay-priority 9999
  "Set this to a bigger number if mismatch is not shown.
Value of priority property for overlay for first mismatched paren or brace."
  :group 'flyparens)

(defcustom flyparens-function 'flyparens-default-function
  "This should be a function that takes position of first mismatch (if any) or nil (if none) as the argument and then displays that information to the user in some way.")

;; This global idle timer comes to life the first time flyparens-mode is turned on in any buffer and then stays until termination of Emacs.
(defvar flyparens--global-idle-timer nil)

;; Lifetime of this global overlay is like that of the global idle timer.
(defvar flyparens--global-overlay nil)


(defun flyparens-default-function (position)
  "Displays an overlay (using `flyparens-mismatch-face') over POSITION if any, but cleans up otherwise."
  (if position
      (move-overlay flyparens--global-overlay
                    position (1+ position) (current-buffer))
    (move-overlay flyparens--global-overlay
                  0 0 (current-buffer))))

(defun flyparens-highlight (position)
  "Highlight POSITION with `flyparens-function'."
  (funcall flyparens-function position))
(defun flyparens-unhighlight ()
  "Clean up with `flyparens-function'."
  (funcall flyparens-function nil))


;;;###autoload
(define-minor-mode flyparens-mode
  " FlyParens"
  :group 'flyparens
  (progn
    (when (and flyparens-mode
               (null flyparens--global-idle-timer))
      ;; one time initialization of the global timer and the global overlay.
      (setq flyparens--global-idle-timer
            (run-with-idle-timer flyparens-delay
                                 :repeat
                                 'flyparens--internal))
      (setq flyparens--global-overlay
            (make-overlay 0 0))
      (overlay-put flyparens--global-overlay
                   'face 'flyparens-mismatch-face)
      (overlay-put flyparens--global-overlay
                   'priority flyparens-mismatch-overlay-priority))
    (unless flyparens-mode
      ;; cleans things up.
      (flyparens-unhighlight))))

(defun flyparens--internal ()
  "Update display."
  (when flyparens-mode
    ;; partly from check-parens
    (when (eq
           (condition-case data
               (progn
                 ;; Buffer can't have more than (point-max) sexps.
                 (scan-sexps (point-min) (point-max))
                 :no-error-rus5oyso)
             (scan-error
              (flyparens-highlight (nth 2 data)))
             ;; (error
             ;;  (warn "(ejv5cxmy) Strange."))
             )
           :no-error-rus5oyso)
      (flyparens-unhighlight))))

(provide 'flyparens)
;;; flyparens.el ends here
