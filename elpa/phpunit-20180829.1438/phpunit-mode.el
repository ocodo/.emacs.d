;;; phpunit-mode.el --- Minor mode for PHPUnit

;; Copyright (C) 2014-2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'phpunit)

(defvar phpunit-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-t t") 'phpunit-current-test)
    (define-key map (kbd "C-t c") 'phpunit-current-class)
    (define-key map (kbd "C-t p") 'phpunit-current-project)
    map)
  "Keymap for PHPUnit minor mode.")

(define-minor-mode phpunit-mode
  "PHPUnit minor mode"
  :lighter " phpunit"
  :keymap phpunit-mode-map)

(add-to-list 'auto-mode-alist '("\\.php$'" . phpunit-mode))

;;; phpunit-mode.el ends here
