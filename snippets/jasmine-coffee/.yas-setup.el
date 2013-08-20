(defun search-backward-wrapped-string (wrap_start wrap_end)
  "Search for a string behind point which is wrapped in two
strings, wrap_start and wrap_end.

if wrap_end and wrap_start are equal, we first position the point
at the beginning of the first wrap_end match, before the initial
point.

The string found between the two wrappers is returned.

This is useful for naive finding of symbols previously defined in the
buffer.

Revisit this function to figure out a regexp version of "
  (save-excursion
    (when (equal wrap_start wrap_end)
      (search-backward wrap_end nil t))
    (let* ((start_beginning nil)
           (start_match nil)
           (end_match nil))
      (setq start_beginning (search-backward wrap_start nil t))
      (when start_beginning
        (setq start_match (+ start_beginning (length wrap_start)))
        (when start_match
          (goto-char start_match)
          (setq end_match (- (search-forward wrap_end nil t) 1))
          (when  end_match
            (buffer-substring-no-properties start_match end_match)))))))

(defun jasmine-coffee-ng/before-each-module-name ()
  "Find the name of a module included by a previous beforeEach"
  (or
   (search-backward-wrapped-string "beforeEach module \"" "\"")
   (search-backward-wrapped-string "beforeEach(module(\"" "\"")
   (search-backward-wrapped-string "beforeEach module '" "'")
   (search-backward-wrapped-string "beforeEach(module('" "'")))

(defvar jasmine-ng/dependency-types
  '("Controllers" "Directives" "Services" "Routes" "Filters"))
