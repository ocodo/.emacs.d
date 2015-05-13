;;; 4clojure.el --- Open and evaluate 4clojure.com questions

;; Copyright (C) 2013 Joshua Hoff

;; Author: Joshua Hoff
;; Keywords: languages, data
;; Package-Version: 20131014.1507
;; Version: 0.1.1
;; Package-Requires: ((json "1.2") (request "0.2.0"))

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

;; To open a specific problem, use `4clojure-open-question':
;; e.g. "M-x 4clojure-open-question RET 2" opens question 2.

;; To check your answers, use `4clojure-check-answers':
;; e.g. `M-x 4clojure-check-answers`

;; To open the next question (or the first if you’re not in a 4clojure
;; buffer), use `4clojure-next-question'. Similarly,
;; `4clojure-previous-question' opens the previous question.

;;; Code:

(require 'json)
(require 'request)

(defvar 4clojure-cached-question nil
  "The current question, in the format: (number question-data).")

(defun 4clojure/get-question-cached (problem-number)
  "Gets a 4clojure problem, saves it, returns that if asked again"
  (if (string= (car 4clojure-cached-question) problem-number)
      (cadr 4clojure-cached-question)
    (progn
      (request
       (format "http://www.4clojure.com/api/problem/%s" problem-number)
       :parser 'json-read
       :sync t
       :success (function*
                 (lambda (&key data &allow-other-keys)
                   (setq 4clojure-cached-question
                         `(,problem-number ,data)))))
      (cadr 4clojure-cached-question))))

(defun 4clojure/questions-for-problem (problem-number)
  "Gets a list of questions/tests corresponding to a 4clojure problem"
  (mapconcat 'identity
             (assoc-default 'tests
                            (4clojure/get-question-cached problem-number))
             "\n\n"))

(defun 4clojure/first-question-for-problem (problem-number)
  "Gets the first question of a 4clojure problem (sometimes there only is one),
these are called 'tests' on the site"
  (replace-regexp-in-string
   "" ""
   (elt (assoc-default 'tests
                       (4clojure/get-question-cached problem-number))
        0)))

(defun 4clojure/description-of-problem (problem-number)
  "Gets the description of a 4clojure problem"
  (assoc-default 'description
                 (4clojure/get-question-cached problem-number)))

(defun 4clojure/restrictions-for-problem (problem-number)
  "Gets any restrictions for a problem (a list of functions you're not allowed
to use); or nil if there are no restrictions"
  (let ((restrictions (assoc-default 'restricted
                        (4clojure/get-question-cached problem-number))))
    (if (= 0 (length restrictions))
        nil
      restrictions)))

(defun 4clojure/start-new-problem (problem-number)
  "Opens a new buffer with a 4clojure problem and description in it. Doesn't
clobber existing text in the buffer (if the problem was already opened)."
  (let ((buffer (get-buffer-create (format "*4clojure-problem-%s*" problem-number)))
        (questions (4clojure/questions-for-problem problem-number))
        (description (4clojure/description-of-problem problem-number))
        (restrictions (4clojure/restrictions-for-problem problem-number)))
    (switch-to-buffer buffer)
    ; only add to empty buffers, thanks: http://stackoverflow.com/q/18312897
    (when (= 0 (buffer-size buffer))
      (insert (4clojure/format-problem-for-buffer problem-number description questions restrictions))
      (beginning-of-buffer)
      (search-forward "__")
      (backward-char 2)
      (when (functionp 'clojure-mode)
        (clojure-mode)))))

(defun 4clojure/format-problem-for-buffer (problem-number description questions &optional restrictions)
  "Formats a 4clojure question and description for an emacs buffer (adds a
header, a tip about how to check your answers, etc)"
  (concat
   ";; 4Clojure Question " problem-number "\n"
   ";;\n"
   ";; " (replace-regexp-in-string "\s*\n+\s*" "\n;;\n;; " description) "\n"
   (when restrictions
     (concat ";;\n;; Restrictions (please don't use these function(s)): "
             (mapconcat 'identity restrictions ", ")
             "\n"))
   ";;\n;; Use M-x 4clojure-check-answers when you're done!\n\n"
   (replace-regexp-in-string "" "" questions)))

(defun 4clojure/get-answer-from-current-buffer (problem-number)
  "Gets the user's answer to the first question by getting the original question
 (with a blank in it) from 4clojure and matching that against the current
 buffer"
  (string-match
   (replace-regexp-in-string
    "__"
    "\\(\\(\n\\|.\\)\+\\)"
    (replace-regexp-in-string
     "[\s\n]\+"
     "[\s\n]\+"
     (regexp-quote (4clojure/first-question-for-problem problem-number))
     nil t)
    nil t)
   (buffer-string))
  (match-string 1 (buffer-string)))

(defun 4clojure/problem-number-of-current-buffer ()
  "Gets the problem number of the current buffer or 0 if current buffer isn't
named something like *blah-blah-123*"
  (let* ((bufname (buffer-name (current-buffer)))
         (number-with-star (first (last (split-string bufname "-"))))
         (problem-number (substring number-with-star
                                    0
                                    (1- (string-width number-with-star)))))
    (if (string-match "[^0-9]" problem-number)
        0
      (string-to-int problem-number))))

(defun 4clojure/check-answer (problem-number answer)
  "Sends an answer to 4clojure and returns the result"
  (request
   (format "http://www.4clojure.com/rest/problem/%s" problem-number)
   :type "POST"
   :parser 'json-read
   :sync t
   :data `(("id" . ,problem-number) ("code" . ,answer))
   :success (function*
             (lambda (&key data &allow-other-keys)
               (let ((error (assoc-default 'error data))
                     (message (assoc-default 'message data))
                     (indexOfFailing (assoc-default 'failingTest data)))
                 (setq result
                       (if (> (string-width error) 0)
                           `(,indexOfFailing ,error)
                         `(,nil ,message)))))))
  result)

;;;###autoload
(defun 4clojure-open-question (problem-number)
  "Opens a 4clojure problem in an aptly named buffer"
  (interactive "sWhich 4clojure question? ")
  (4clojure/start-new-problem problem-number))


;;;###autoload
(defun 4clojure-next-question ()
  "Gets the next 4clojure question or 1st question based on the current buffer
name"
  (interactive)
  (let ((problem-number (4clojure/problem-number-of-current-buffer)))
    (4clojure/start-new-problem (int-to-string (1+ problem-number)))))


;;;###autoload
(defun 4clojure-previous-question ()
  "Opens the previous 4clojure question or 1st question based on the current
buffer name"
  (interactive)
  (let ((problem-number (4clojure/problem-number-of-current-buffer)))
    (4clojure/start-new-problem (int-to-string (if (< problem-number 3)
                                                   1
                                                 (1- problem-number))))))

;;;###autoload
(defun 4clojure-check-answers ()
  "Sends the first answer to 4clojure and gets a message back"
  (interactive)
  (let* ((problem-number-as-int (4clojure/problem-number-of-current-buffer))
         (problem-number (int-to-string problem-number-as-int))
         (result (4clojure/check-answer
                  problem-number
                  (4clojure/get-answer-from-current-buffer problem-number))))
    (if (car result)
        (message "Test %d failed.\n%s"
                 (car result)
                 (cadr result))
      (message "%s" (cadr result)))))

(provide '4clojure)
;;; 4clojure.el ends here
