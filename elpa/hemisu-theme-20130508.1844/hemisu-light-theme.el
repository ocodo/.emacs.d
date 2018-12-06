;;; hemisu-theme.el --- Hemisu for Emacs.

;; based on Hemisu vim theme
;; of Noah Frederick

;; Copyright (C) 2013 Andrzej Sliwa

;; Author: Andrzej Sliwa
;; URL: http://github/anrzejsliwa/django-theme
;; Version: 1.0.0
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A port of Hemisu theme to Emacs. (https://github.com/noahfrederick/Hemisu)
;;
;;; Installation:
;;
;;   M-x package-install -> hemisu-theme
;;
;;
;;   (load-theme 'hemisu-dark t)
;;
;;     or
;;
;;   (load-theme 'hemisu-light t)
;;
;; Don't forget that the theme requires Emacs 24.
;;
;;; Bugs
;;
;; None that I'm aware of.
;;
;;; Credits
;;
;; Noah Frederick created the original theme for vim on such this port
;; is based.
;;
;;; Code:

(require 'hemisu-theme)

(deftheme hemisu-light "The light variant of the Hemisu colour theme")
(create-hemisu-theme 'light 'hemisu-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide-theme 'hemisu-light)
;;; hemisu-dark-light.el ends here
