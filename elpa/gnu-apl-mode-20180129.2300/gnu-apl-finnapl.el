;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'tabulated-list)

(eval-when-compile (require 'cl-lib))
;; optional helm dependency
(require 'helm nil t)

(defvar *gnu-apl--finapl-url*
  "https://aplwiki.com/FinnAplIdiomLibrary?action=raw"
  "Url of the page containing FinnAPL idioms list")

(defvar *gnu-apl-finapl-use-helm-choice* t
  "Determines how to present the list of idioms.
If t, use helm if available.
If nil, always use internal tabular mode.")


(defvar *gnu-apl--finnapl-idioms*
  nil
  "The list of parsed idioms from FinnAPL wiki:
 https://aplwiki.com/FinnAplIdiomLibrary
Each list entry is a list:
- idiom id (not necessary unique if idiom repeats with different
variations)
- idiom name
- parameters
- idiom itself - APL expression")

(defvar *gnu-apl--finnapl-prev-buffer*
  nil
  "The buffer to insert the idiom. It is set to any buffer
which was active then `gnu-apl-finnapl-list' was called")

(defvar *gnu-apl--finnapl-title* "*APL Idioms*")

(defun gnu-apl-finnapl-download-list ()
  "Download the FinnAPL idioms list and parse them
The variable `*gnu-apl--finnapl-idioms*' will be constructed
containing parsed values from this list"
  (url-retrieve *gnu-apl--finapl-url*
                #'gnu-apl--parse-finnapl-text-webpage))


(defun gnu-apl--parse-finnapl-text-webpage (&optional status)
  (unless (plist-member status :error)
    ;; clear the old idioms list
    (setq *gnu-apl--finnapl-idioms* nil)
    ;; start with the beginning of the buffer and
    ;; find the beginnin of the list
    (goto-char (point-min))
    (let* ((start-text "== Idiom Library Listing ==[ \n\r]*")
           (start-point (search-forward-regexp start-text (point-max) t)))
      ;; collect all sections
      (when start-point
        (let* ((section-regexp "^=== \\(.*\\) ===[ \n\r]*")
               (sections
                (loop for index-start = 0 then (match-end 0)
                      for last-section-name = nil then (decode-coding-string (match-string 1) 'utf-8)
                      while (search-forward-regexp section-regexp
                                                   (point-max)
                                                   t)
                      when last-section-name
                      collect (list last-section-name
                                    index-start
                                    (match-beginning 0))
                      into result
                      finally
                      (return (nconc result (list (list last-section-name index-start (point-max))))))))
          ;; parse sections updating the global list of idioms
          (dolist (x sections)
            (apply #'gnu-apl--parse-finnapl-section x))
          ;; kill the buffer created by url-retrieve. 
          (kill-buffer (current-buffer))
          (setq *gnu-apl--finnapl-idioms* (nreverse *gnu-apl--finnapl-idioms*))
          (message "List of APL idioms successfully downloaded")
          (if (and *gnu-apl-finapl-use-helm-choice* (fboundp 'helm))
              (gnu-apl-finnapl-choice-helm)
            (gnu-apl-finnapl-choice-tabular)))))))




(defun gnu-apl--parse-finnapl-section (name start end)
  "Parse a section with the name NAME and boundaries in
the buffer created by url-retrieve START END."
  (save-excursion
    (goto-char start)
    (let* ((idiom-title-regexp "rowspan=\\([0-9]+\\).*> +\\([0-9]+\\). || \\(.*\\) ||.*{{{\\(.*\\)}}}")
           (idiom-list
            (loop for index-start = 0 then (match-end 0)
                  for last-idiom-title = nil then (decode-coding-string (match-string 3) 'utf-8)
                  for last-title-rows = 0 then (string-to-number (match-string 1))
                  for last-title-number = 0 then (match-string 2)
                  for last-title-params = nil then (decode-coding-string (match-string 4) 'utf-8)
                  while (search-forward-regexp idiom-title-regexp
                                               end
                                               t)
                  when last-idiom-title
                  collect (list last-title-number
                                last-idiom-title
                                last-title-params
                                last-title-rows
                                index-start
                                (match-beginning 0))
                  into result
                  finally
                  (return (nconc result (list (list (match-string 2)
                                                    last-idiom-title
                                                    (decode-coding-string (match-string 4) 'utf-8)
                                                    (string-to-number (match-string 1))
                                                    (match-end 0)
                                                    end)))))))
      (mapc #'gnu-apl--parse-finnapl-idiom idiom-list))))


(defun gnu-apl--parse-finnapl-idiom (idiom)
  "Parse particular IDIOM part of the buffer.
The IDIOM is a list of: 
  - Idiom number (string)
  - Idiom name
  - Idiom arguments
  - Number of rows to parse
  - start position of idiom contents
  - end position of idiom contents"
  (destructuring-bind (id name args numrows start end)
      idiom
    (let* ((idiom-row-regexp "colspan=[0-9].*>\\(.*\\)[ \t]*||")
           (rows
            (save-excursion
              (goto-char start)
              ;; collect contents of all rows starting with "colspan"
              (loop while (search-forward-regexp idiom-row-regexp
                                                 end
                                                 t)
                    collect (decode-coding-string (match-string 1) 'utf-8)))))
      ;; sanity check, numrows in table is header line + list of idioms
      (assert (= (length rows) (1- numrows)))
      (cond ((and (= (length rows) 1)
                  (string-match ".*{{{\\(.*\\).*}}}" (car rows)))
             (push (list id name args (match-string 1 (pop rows)))
                   *gnu-apl--finnapl-idioms*))
            ;; special case then more than 1 implementation of the
            ;; idiom provided
            ((> (length rows) 1)
             (assert (string-match ".*{{{\\(.*\\).*}}}" (car rows)))
             (push (list id name args (match-string 1 (pop rows)))
                   *gnu-apl--finnapl-idioms*)
             (loop for i from 0 below (/ (length rows) 2)
                   for header-line = (elt rows (* 2 i))
                   for code-line = (elt rows (1+ (* 2 i)))
                   do
                   (assert (string-match "{{{\\(.*\\)}}}"
                                         code-line))
                   (setf code-line (match-string 1 code-line))
                   (push (list (concat id  "." (int-to-string (1+ i)))
                               (concat name " : "
                                       (string-trim header-line))
                               args code-line)
                         *gnu-apl--finnapl-idioms*)))))))


(define-derived-mode gnu-apl-idioms-choice-mode tabulated-list-mode "GNU APL Idioms list"
  "Major mode for selecting idioms in GNU APL"
  (progn
    (setq tabulated-list-format `[("Id" 5 nil)
                                  ("Idiom" 0 nil)])
    (tabulated-list-init-header)))

(defun gnu-apl-finnapl--insert-idiom (idiom)
  (with-current-buffer *gnu-apl--finnapl-prev-buffer*
    (insert idiom)
    (goto-char (+ (point) (length idiom)))))



(defun gnu-apl-finnapl-choice-tabular-action (button)
  "Action on either Enter key or mouse1.
This action inserts the selected idiom into the buffer
and closes the idioms window."
  (ignore button)
  (let* ((id (tabulated-list-get-id))
         (idiom (fourth (cl-find id *gnu-apl--finnapl-idioms*
                                 :test #'string= :key #'car))))
    (quit-window t)  
    (gnu-apl-finnapl--insert-idiom idiom)))

(defun gnu-apl-finnapl-choice-tabular ()
  "Create a buffer with a list of alternatives to choose from."
  (let ((buf (get-buffer-create *gnu-apl--finnapl-title*)))
    ;; first create and switch to the new empty buffer
    (switch-to-buffer buf)
    ;; turn on the choices mode (based on tabulated-list)
    (gnu-apl-idioms-choice-mode)
    ;; convert list of string idioms to something
    ;; accepted by the tabulated list
    ;; we create 3 lines per entry:
    ;; with the name, with the arguments and with the idiom itself
    ;; The action on any of these lines is the same - to insert
    ;; an idiom into the GNU APL interpreter buffer
    (cl-flet ((create-line (id col1 col2 &optional (face 'default))
                           (list id
                                 (vector
                                  (cons col1
                                        (list 'action
                                              #'gnu-apl-finnapl-choice-tabular-action
                                              'face face))
                                  (cons col2
                                        (list 'action
                                              #'gnu-apl-finnapl-choice-tabular-action
                                              'face face))))))
      (setq tabulated-list-entries
            (loop for (id name args idiom) in *gnu-apl--finnapl-idioms*
                  ;; first line is the idiom id and a name
                  collect (create-line id id name)
                  ;; second line is just arguments
                  collect (create-line id "" args)
                  ;; third line is the idiom itself
                  collect (create-line id "" idiom 'comint-highlight-input))))
    (tabulated-list-print)
    nil))


(defun gnu-apl-finnapl-list ()
  "Download and present the list of FinnAPL idioms.
User can click or press Enter on any row to insert idiom into
the GNU APL.

The idioms list is downloaded from the APL Wiki:
https://aplwiki.com/FinnAplIdiomLibrary"
  (interactive)
  (setq *gnu-apl--finnapl-prev-buffer* (current-buffer))
  (save-selected-window
    (cond ((not *gnu-apl--finnapl-idioms*)
           (gnu-apl-finnapl-download-list))
          ((and *gnu-apl-finapl-use-helm-choice* (fboundp 'helm))
           (gnu-apl-finnapl-choice-helm))
          (t (gnu-apl-finnapl-choice-tabular)))))



(defun gnu-apl-finnapl-choice-helm ()
  "Present helm narrowing search buffer for FinnAPL idioms"
  (let* ((candidates
          (mapcar (lambda(x)
                    (cons (concat (car x) ". " (second x) "\n"
                                  (third x) "\n" (fourth x))
                          (fourth x)))
                  *gnu-apl--finnapl-idioms*))
         (sources
          `((name . "FinnAPL Idioms")
            (multiline . t)
            (candidates . ,candidates)
            (action . (lambda (x) (gnu-apl-finnapl--insert-idiom x))))))
    (let ((helm-candidate-number-limit nil))
      (helm :sources sources
            :prompt "Search for: "
            :buffer *gnu-apl--finnapl-title*))))


(provide 'gnu-apl-finnapl)
