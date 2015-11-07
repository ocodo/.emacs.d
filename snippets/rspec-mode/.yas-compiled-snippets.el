;;; Compiled snippets and support files for `rspec-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'rspec-mode
                     '(("scn" "scenario \"${1:does something}\" do\n  $0\nend\n" "scenario \"does something\" do ... end" nil nil nil "/Users/jason/.emacs.d/snippets/rspec-mode/scn" nil nil)
                       ("let" "let(:${1:var}) { $0 }" "let(:var) { ... }" nil nil nil "/Users/jason/.emacs.d/snippets/rspec-mode/let" nil nil)
                       ("it" "it \"${1:does something}\" do\n  $0\nend" "it \"does something\" do ... end" nil nil nil "/Users/jason/.emacs.d/snippets/rspec-mode/it" nil nil)
                       ("helper" "require File.dirname(__FILE__) + '../spec_helper'\n\n$0" "require File.dirname(__FILE__) + '../spec_helper'" nil nil nil "/Users/jason/.emacs.d/snippets/rspec-mode/helper" nil nil)
                       ("featm" "feature \"${1:description}\", \"${2:modifier}\" do\n  $0\nend\n" "feature \"description\", \"modifier\" do ... end" nil nil nil "/Users/jason/.emacs.d/snippets/rspec-mode/featm" nil nil)
                       ("feat" "feature \"${1:description}\" do\n  $0\nend\n" "feature \"description\" do ... end" nil nil nil "/Users/jason/.emacs.d/snippets/rspec-mode/feat" nil nil)
                       ("expb" "expect { $1 }.to $0" "expect { ... }.to ..." nil nil nil "/Users/jason/.emacs.d/snippets/rspec-mode/expb" nil nil)
                       ("exp" "expect($1).to $0" "expect(...).to ..." nil nil nil "/Users/jason/.emacs.d/snippets/rspec-mode/exp" nil nil)
                       ("descm" "describe ${1:`(rspec-class-from-file-name)`}, \"${2:modifier}\" do\n  $0\nend" "describe Class, \"modifier\" do ... end" nil nil nil "/Users/jason/.emacs.d/snippets/rspec-mode/descm" nil nil)
                       ("desc" "describe `maybe-quote`${1:`(and top-level (rspec-class-from-file-name))`}`maybe-quote` do\n  $0\nend" "describe Class do ... end" nil nil
                        ((top-level
                          (rspec-top-level-desc-p))
                         (maybe-quote
                          (unless top-level "\"")))
                        "/Users/jason/.emacs.d/snippets/rspec-mode/desc" nil nil)
                       ("cont" "context \"${1:modifier}\" do\n  $0\nend" "context \"modifier\" do ... end" nil nil nil "/Users/jason/.emacs.d/snippets/rspec-mode/cont" nil nil)
                       ("bef" "before$1 do\n  $0\nend\n" "before do ... end" nil nil nil "/Users/jason/.emacs.d/snippets/rspec-mode/bef" nil nil)
                       ("aft" "after$1 do\n  $0\nend" "after do ... end" nil nil nil "/Users/jason/.emacs.d/snippets/rspec-mode/aft" nil nil)))


;;; Do not edit! File generated at Sat Nov  7 12:16:41 2015
