(require 'ac-html-core)

(defun ac-html-testing-tags ()
  '("tag1" "tag2" "tag3"))

(defun ac-html-testing-classes ()
  '("class1" "class2"))

(defun ac-html-testing-ids ()
  '("id1" "id2" "id3"))

(ac-html-define-data-provider 'ac-html-testing-data-provider
  :tag-func 'ac-html-testing-tags
  :class-func 'ac-html-testing-classes
  :id-func 'ac-html-testing-ids)

(provide 'ac-html-testing-data-provider)
