;;; lorem-ipsum.el --- Insert dummy pseudo Latin text.
;; Author & Maintainer: Jean-Philippe Theberge (jphil21@sourceforge.net)
;; Special Thanks: The emacswiki users, the #emacs@freenode.net citizens 
;;                 and Marcus Tullius Cicero
;;
;; version :
(defconst lorem-ipsum-version "0.1")
;; last update: 16/09/2003

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 2003 Jean-Philippe Theberge
;;
;; This file is not (yet?) part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst lorem-ipsum-text
  '(("Lorem ipsum dolor sit amet, consectetuer adipiscing elit."
     "Donec hendrerit tempor tellus."
     "Donec pretium posuere tellus."
     "Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus."
     "Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus."
     "Nulla posuere."
     "Donec vitae dolor."
     "Nullam tristique diam non turpis."
     "Cras placerat accumsan nulla."
     "Nullam rutrum."
     "Nam vestibulum accumsan nisl.")

    ("Pellentesque dapibus suscipit ligula."
     "Donec posuere augue in quam."
     "Etiam vel tortor sodales tellus ultricies commodo."
     "Suspendisse potenti."
     "Aenean in sem ac leo mollis blandit."
     "Donec neque quam, dignissim in, mollis nec, sagittis eu, wisi."
     "Phasellus lacus."
     "Etiam laoreet quam sed arcu."
     "Phasellus at dui in ligula mollis ultricies."
     "Integer placerat tristique nisl."
     "Praesent augue."
     "Fusce commodo."
     "Vestibulum convallis, lorem a tempus semper, dui dui euismod elit, vitae placerat urna tortor vitae lacus."
     "Nullam libero mauris, consequat quis, varius et, dictum id, arcu."
     "Mauris mollis tincidunt felis."
     "Aliquam feugiat tellus ut neque."
     "Nulla facilisis, risus a rhoncus fermentum, tellus tellus lacinia purus, et dictum nunc justo sit amet elit.")

    ("Aliquam erat volutpat."
     "Nunc eleifend leo vitae magna."
     "In id erat non orci commodo lobortis."
     "Proin neque massa, cursus ut, gravida ut, lobortis eget, lacus."
     "Sed diam."
     "Praesent fermentum tempor tellus."
     "Nullam tempus."
     "Mauris ac felis vel velit tristique imperdiet."
     "Donec at pede."
     "Etiam vel neque nec dui dignissim bibendum."
     "Vivamus id enim."
     "Phasellus neque orci, porta a, aliquet quis, semper a, massa."
     "Phasellus purus."
     "Pellentesque tristique imperdiet tortor."
     "Nam euismod tellus id erat.")

    ("Nullam eu ante vel est convallis dignissim."
     "Fusce suscipit, wisi nec facilisis facilisis, est dui fermentum leo, quis tempor ligula erat quis odio."
     "Nunc porta vulputate tellus."
     "Nunc rutrum turpis sed pede."
     "Sed bibendum."
     "Aliquam posuere."
     "Nunc aliquet, augue nec adipiscing interdum, lacus tellus malesuada massa, quis varius mi purus non odio."
     "Pellentesque condimentum, magna ut suscipit hendrerit, ipsum augue ornare nulla, non luctus diam neque sit amet urna."
     "Curabitur vulputate vestibulum lorem."
     "Fusce sagittis, libero non molestie mollis, magna orci ultrices dolor, at vulputate neque nulla lacinia eros."
     "Sed id ligula quis est convallis tempor."
     "Curabitur lacinia pulvinar nibh."
     "Nam a sapien.")))

(defvar lorem-ipsum-paragraph-separator "\n\n")
(defvar lorem-ipsum-sentence-separator "  ")
(defvar lorem-ipsum-list-beginning "")
(defvar lorem-ipsum-list-bullet "* ")
(defvar lorem-ipsum-list-item-end "\n")
(defvar lorem-ipsum-list-end "")

(make-variable-buffer-local 'lorem-ipsum-paragraph-separator)
(make-variable-buffer-local 'lorem-ipsum-sentence-separator)
(make-variable-buffer-local 'lorem-ipsum-list-beginning)
(make-variable-buffer-local 'lorem-ipsum-list-bullet)
(make-variable-buffer-local 'lorem-ipsum-list-item-end)
(make-variable-buffer-local 'lorem-ipsum-list-end)

(add-hook 'sgml-mode-hook (lambda ()
			    (setq lorem-ipsum-paragraph-separator "<br><br>\n"
				  lorem-ipsum-sentence-separator "&nbsp;&nbsp;"
				  lorem-ipsum-list-beginning "<ul>\n"
				  lorem-ipsum-list-bullet "<li>"
				  lorem-ipsum-list-item-end "</li>\n"
				  lorem-ipsum-list-end "</ul>\n")))

(defun lorem-ipsum-insert-paragraphs (&optional num)
  (interactive "p")
  (if (not num)(setq num 1))
  (if (> num 0)
      (progn
	(insert (concat 
		 (mapconcat 'identity 
			    (nth (if (interactive-p) 0 (random (length lorem-ipsum-text)))
				 lorem-ipsum-text) " ")
		 lorem-ipsum-paragraph-separator))
	(lorem-ipsum-insert-paragraphs (- num 1)))))

(defun lorem-ipsum-insert-sentences (&optional num)
  (interactive "p")
  (if (not num)(setq num 1))
  (if (> num 0)
      (progn
	(let ((para 
	       (nth (if (interactive-p) 0 (random (length lorem-ipsum-text))) lorem-ipsum-text)))
	  (insert (concat (nth (if (interactive-p) 0 (random (length para))) para) lorem-ipsum-sentence-separator)))
	(lorem-ipsum-insert-sentences (- num 1)))))
	  
(defun lorem-ipsum-insert-list (&optional num)
  (interactive "p")
  (if (not num)(setq num 1))
  (if (> num 0)
      (progn
	(if (interactive-p) (insert lorem-ipsum-list-beginning))
	(let ((para (nth (if (interactive-p) 0 (random (length lorem-ipsum-text))) lorem-ipsum-text)))
	  (insert (concat lorem-ipsum-list-bullet 
			  (nth (if (interactive-p) 0 (random (length para))) para) 
			  lorem-ipsum-list-item-end)))
	(lorem-ipsum-insert-list (- num 1)))
    (insert lorem-ipsum-list-end)))


(provide 'lorem-ipsum)

;;; lorem-ipsum.el ends here
