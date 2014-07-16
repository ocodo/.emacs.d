;;; achievements.el --- achievements for emacs usage

;; Author: Ivan Andrus <darthandrus@gmail.com>
;; Maintainer: Ivan Andrus <darthandrus@gmail.com>
;; Created: 2012-10-07
;; Keywords: games

;;; Install:

;; Install from MELPA, or add to `load-path' and (require
;; 'achievements).  It is also highly recommended to install the
;; keyfreq package in order to get all the functionality.

;;; Commentary:

;; Running `achievements-list-achievements' will show a list of all
;; unlocked achievements.  To earn some achievements, and the check
;; automatically for earned achievements, you must enable
;; `achievements-mode`.

;;; Code:

(require 'achievements-functions)

;; Set things up before we load any achievements files, otherwise the
;; definitions will populate achievements-list instead of the saved
;; values.
(achievements-init)

(provide 'achievements)

;;; achievements.el ends here
