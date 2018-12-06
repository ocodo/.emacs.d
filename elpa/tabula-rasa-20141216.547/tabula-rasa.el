;;; tabula-rasa.el --- Distraction free writing mode

;; Copyrigth (C) 2011-15  Ido Magal

;; Author: Ido Magal <misc@satans.church>
;; Version: 20141215.2119
;; Package-Version: 20141216.547
;; version func: (insert (format-time-string "%Y%m%d.%H%M" (current-time)))
;; Keywords: distraction free, writing
;; URL: https://github.com/idomagal/Tabula-Rasa/blob/master/tabula-rasa.el
;; Package-Requires: ((emacs "24.4"))

;; This file is *NOT* part of GNU Emacs.

;;; Commentary:
;; Tabula Rasa was inspired by darkroom-mode.el, WriteRoom, and all of the other  
;; distraction free tools. It was developed out of the need for a more customizable  
;; distraction free mode for Emacs.  

;;; Installation  

;; Install through Melpa
;; or
;; put this file in your common lisp files dir (e.g. .emacs.d/site-lisp)  
;;
;; Regardless, put the following in your Emacs configuration file (e.g. .emacs) file:
;;
;; (require 'tabula-rasa)  
;;
;; Type M-x tabula-rasa-mode to toggle the mode.  

;; For customization of colors, etc, type M-x customize-group RET tabula-rasa RET
;; Customization options include:  
;; - Text font and colors.  
;; - Cursor and region colors.  
;; - Column width  
;; - Line spacing  
;; - Minor modes to enable or disable for Tabula Rasa  
;; - Whether or not to antialias text (experimental)  

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


(defgroup tabula-rasa nil
  "The latest in high-tech distraction free writing."
  :version 0.1
  :group 'text)

(defcustom tabula-rasa-width 80
  "Width of writing space."
  :type 'integer
  :group 'tabula-rasa
  :set (lambda (symbol value)
         (setq tabula-rasa-width value)
         (tabula-rasa-update-window))
  :initialize 'custom-initialize-default)

(defcustom tabula-rasa-line-spacing 5
  "Vertical line spacing."
  :type 'integer
  :group 'tabula-rasa
  :initialize 'custom-initialize-default)

(defcustom tabula-rasa-toggle-antialiasing nil
  "Whether or not to toggle anti-aliasing on text.
Recommended for non-monotype, 'pretty' fonts."
  :type 'boolean
  :group 'tabula-rasa)

(defcustom tabula-rasa-minor-mode-states
  '(("global-highline-mode" . nil) ("global-highlight-parentheses-mode" . nil))
  "This allows you to temporarily disable or enable minor modes for Tabula Rasa.
Add the minor mode and the desired state while in Tabula Rasa mode."
  :type '(repeat (cons :format "%v"
                       (string :tag "Minor Mode")
                       (boolean :tag "State"))))  

(defface tabula-rasa-default
  '(
    (((class grayscale)
      (background light)) (:background "LightGray"))
    (((class grayscale)
      (background dark))  (:background "DimGray"))
    (((class color)
      (background light)) (:background "White" :foreground "Black"))
    (((class color)
      (background dark))  (:background "Black" :foreground "White"))
    )
  "Face for tabula-rasa mode."
  :group 'tabula-rasa)

(defface tabula-rasa-cursor
  '((t (
        :inherit tabula-rasa-default
        :inverse-video t
    )))
  "Face for the Tabula Rasa cursor. Only background and foreground colors matter."
  :group 'tabula-rasa)

(defface tabula-rasa-region
  '(
    (((class grayscale)
      (background light)) (:background "White" ))
    (((class grayscale)
      (background dark))  (:background "DimGray"))
    (((class color)
      (background light)) (:background "White" :foreground "Black"))
    (((class color)
      (background dark))  (:inherit tabula-rasa-default :background "gray" ))
    )
  "Face for the Tabula Rasa region select. Only background and foreground colors matter."
  :group 'tabula-rasa)

(defvar tabula-rasa-frame nil)

(define-minor-mode tabula-rasa-mode
  "Distraction Free Writing" 
  :lighter " TR"
  :init-value nil
  :group 'tabula-rasa
  :global t
  (if tabula-rasa-mode
      (tabula-rasa-mode-enable)
    (tabula-rasa-mode-disable t)))

(defun tabula-rasa-update-window()
  (cond 
   ((not (frame-live-p tabula-rasa-frame))
    (tabula-rasa-mode-disable nil))
   ((or (one-window-p t tabula-rasa-frame) (window-full-width-p tabula-rasa-window))
    (set-window-margins tabula-rasa-window
                        (/ (- (frame-width tabula-rasa-frame) tabula-rasa-width) 2)              
                        (/ (- (frame-width tabula-rasa-frame) tabula-rasa-width) 2)))
   (t
    (set-window-margins tabula-rasa-window 0 0)
    (set-window-margins (next-window) 0 0))))

;width test: 80 chars
;12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789

;; Emacs bug? It takes 2 calls to set bg color to set frame margin colors.
(defun tabula-rasa-set-frame-parms ()
  (progn
    (modify-frame-parameters tabula-rasa-frame 
			     `(
			       (fullscreen . fullboth)
			       (line-spacing . ,tabula-rasa-line-spacing)
			       (foreground-color . ,(face-attribute 'tabula-rasa-default :foreground))
			       (background-color . ,(face-attribute 'tabula-rasa-default :background))
			       (font . ,(face-font "tabula-rasa-default"))
			       (unsplittable . t)
			       (left-fringe . 0)
			       (right-fringe . 0)
			       (tool-bar-lines . 0)
			       (menu-bar-lines . 0)
			       (vertical-scroll-bars . nil)
			       ))
    
    (set-face-foreground 'region (face-attribute 'tabula-rasa-region :foreground) tabula-rasa-frame)
    (set-face-background 'region (face-attribute 'tabula-rasa-region :background) tabula-rasa-frame)
    (tabula-rasa-update-window)
    ))

(defun tabula-rasa-save-minor-modes ()
  (defvar tabula-rasa-saved-minor-modes '())
  (mapc (lambda (mode)
          (if (boundp (read (car mode)))
              (setq tabula-rasa-saved-minor-modes (cons (cons (car mode) (symbol-value (intern (car mode)))) tabula-rasa-saved-minor-modes))))
        tabula-rasa-minor-mode-states))

(defun tabula-rasa-set-minor-modes (minor-modes-alist)
  (mapc (lambda (mode)
          (if (boundp (read (car mode)))
              (cond
               ((cdr mode) (eval (read (concat "(" (car mode) " 1)"))))
               (t          (eval (read (concat "(" (car mode) " 0)")))))))
        minor-modes-alist))


(defun tabula-rasa-mode-enable()
  (if (eq tabula-rasa-mode 1)
      (progn
        (message "Tabula Rasa mode is already running")
        (exit)))
  ;; minor modes adjustment
  (tabula-rasa-save-minor-modes)
  (tabula-rasa-set-minor-modes tabula-rasa-minor-mode-states)
  ;; antialiasing
  (if tabula-rasa-toggle-antialiasing
      (setq ns-antialias-text (not ns-antialias-text)))
  
  (setq tabula-rasa-frame (make-frame '((fullscreen . fullboth))))
  
  (add-hook 'window-configuration-change-hook 'tabula-rasa-update-window t nil)
  (add-hook 'delete-frame-functions (
                                     lambda (frame)
                                            (if (eq frame tabula-rasa-frame)
                                                (tabula-rasa-mode-disable nil))))
  (select-frame tabula-rasa-frame)
  (setq tabula-rasa-window (frame-selected-window tabula-rasa-frame))
  (tabula-rasa-set-frame-parms)

  )



(defun tabula-rasa-mode-disable(del-frame)
  (if (frame-live-p tabula-rasa-frame)
      (progn
        (setq tabula-rasa-mode nil)
        (select-frame tabula-rasa-frame)
        
        (remove-hook 'window-configuration-change-hook 'tabula-rasa-update-window)
        (remove-hook 'delete-frame-functions 'tabula-rasa-mode-disable)
	
        (if del-frame (delete-frame tabula-rasa-frame))

        (tabula-rasa-set-minor-modes tabula-rasa-saved-minor-modes)
        
        (if tabula-rasa-toggle-antialiasing
            (progn
              (setq ns-antialias-text (not ns-antialias-text))
              ;; On OSX, toggling antialiasing doesn't refresh the entire display so
              ;; we force it.
              (redraw-display)))

        )))

(provide 'tabula-rasa)

;;; tabula-rasa.el ends here
