;;; evil-space.el --- Repeat motion in Evil. Correct the behaviour of what SPC should do.

;; Copyright (C) 2014 Quang Linh LE

;; Author: Quang Linh LE <linktohack@gmail.com>
;; URL: http://github.com/linktohack/evil-space
;; Version: 20140626.32
;; X-Original-Version: 0.0.2
;; Keywords: space repeat motion
;; Package-Requires: ((evil "1.0.0"))

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of evil-space
;;
;; evil-space is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; evil-space is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This program emulates vim-space initially developed by Henrik Öhman
;; (spiiph) It help you using <SPC> key to repeat the last motion like
;; what the dot <.> key does to repeat the last command. The motion
;; are normally setup in pair, that means the <S-SPC> (or customized
;; to what one needs) to reverse that motion.

;;; Example:
;;
;; After hits <}> to go to next paragraph, hits <SPC> again will move
;; to the next one, while <S-SPC> will move to the previous one.


;;; Code:

(require 'evil)

(defcustom evil-space-next-key (kbd "SPC")
  "Key that triggers the repeat motion."
  :group 'evil-space)
(defcustom evil-space-prev-key (kbd "S-SPC")
  "Key that triggers the repeat motion in reverse direction."
  :group 'evil-space)

;;;###autoload
(defmacro evil-space-setup (key next prev)
  "Setup `evil-space` for motion `key`

`SPC` and `S-SPC` are map to next and prev"
  `(progn
     (when (not (fboundp ',(intern (concat "evil-space-" next))))
       (fset ',(intern (concat "evil-space-" next))
         (symbol-function ',(lookup-key evil-motion-state-map next))))
     (when (not (fboundp ',(intern (concat "evil-space-" next))))
       (fset ',(intern (concat "evil-space-" prev))
         (symbol-function ',(lookup-key evil-motion-state-map prev))))
     (defadvice ,(lookup-key evil-motion-state-map key)
       (before ,(intern (concat (symbol-name (lookup-key evil-motion-state-map key)) "-space")) activate)
       ,(concat "Setup evil-space for motion " key)
       (define-key evil-motion-state-map ,evil-space-next-key ',(intern (concat "evil-space-" next)))
       (define-key evil-motion-state-map ,evil-space-prev-key ',(intern (concat "evil-space-" prev))))))

;;;###autoload
(defun evil-space-default-setup ()
  (evil-space-setup "n" "n" "N")
  (evil-space-setup "N" "N" "n")
  (evil-space-setup "t" ";" ",")
  (evil-space-setup "f" ";" ",")
  (evil-space-setup "T" "," ";")
  (evil-space-setup "F" "," ";")
  (evil-space-setup "{" "{" "}")
  (evil-space-setup "}" "}" "{")
  (evil-space-setup "]]" "]]" "[[")
  (evil-space-setup "[[" "[[" "]]"))

(provide 'evil-space)

;;; evil-space.el ends here
