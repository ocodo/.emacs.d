;;; Compiled snippets and support files for `rspec-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'rspec-mode
                     '(("sp" "specify { $0 }\n" "specify { ... }" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/sp" nil nil)
                       ("sfgs" "subject(:${1:name}) { `(rspec-snippets-fg-method-call \"build_stubbed\")`(:$1) }\n" "subject(:${1:name} { build_stubbed(:$1) })" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/sfgs" nil nil)
                       ("sfgc" "subject(:${1:name}) { `(rspec-snippets-fg-method-call \"create\")`(:$1) }\n" "subject(:${1:name} { create(:$1) })" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/sfgc" nil nil)
                       ("sfgb" "subject(:${1:name}) { `(rspec-snippets-fg-method-call \"build\")`(:$1) }\n" "subject(:${1:name} { build(:$1) })" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/sfgb" nil nil)
                       ("scn" "scenario \"${1:does something}\" do\n  $0\nend\n" "scenario \"does something\" do ... end" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/scn" nil nil)
                       ("sbj" "subject(:${1:name}) { $0 }\n" "subject(:${1:name} { ... })" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/sbj" nil nil)
                       ("lfgs" "let(:${1:name}) { `(rspec-snippets-fg-method-call \"build_stubbed\")`(:$1) }\n" "let(:${1:name} { build_stubbed(:$1) })" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/lfgs" nil nil)
                       ("lfgc" "let(:${1:name}) { `(rspec-snippets-fg-method-call \"create\")`(:$1) }\n" "let(:${1:name} { create(:$1) })" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/lfgc" nil nil)
                       ("lfgb" "let(:${1:name}) { `(rspec-snippets-fg-method-call \"build\")`(:$1) }\n" "let(:${1:name} { build(:$1) })" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/lfgb" nil nil)
                       ("let" "let(:${1:var}) { $0 }" "let(:var) { ... }" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/let" nil nil)
                       ("itiexp" "it { is_expected.to $0 }" "it { is_expected.to ... }" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/itiexp" nil nil)
                       ("it" "it \"${1:does something}\" do\n  $0\nend" "it \"does something\" do ... end" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/it" nil nil)
                       ("iexp" "is_expected.to $0" "is_expected.to ..." nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/iexp" nil nil)
                       ("helper" "require File.dirname(__FILE__) + '../spec_helper'\n\n$0" "require File.dirname(__FILE__) + '../spec_helper'" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/helper" nil nil)
                       ("featm" "feature \"${1:description}\", \"${2:modifier}\" do\n  $0\nend\n" "feature \"description\", \"modifier\" do ... end" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/featm" nil nil)
                       ("feat" "feature \"${1:description}\" do\n  $0\nend\n" "feature \"description\" do ... end" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/feat" nil nil)
                       ("expb" "expect { $1 }.to $0" "expect { ... }.to ..." nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/expb" nil nil)
                       ("exp" "expect($1).to $0" "expect(...).to ..." nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/exp" nil nil)
                       ("descm" "`(and top-level (not global-dsl) \"RSpec.\")`describe ${1:`(rspec-class-from-file-name)`}, \"${2:modifier}\" do\n  $0\nend" "describe Class, \"modifier\" do ... end" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/descm" nil nil)
                       ("desc" "`(and top-level (not global-dsl) \"RSpec.\")`describe `maybe-quote`${1:`(and top-level (rspec-class-from-file-name))`}`maybe-quote` do\n  $0\nend" "describe Class do ... end" nil nil
                        ((top-level
                          (rspec-top-level-desc-p))
                         (global-dsl rspec-expose-dsl-globally)
                         (maybe-quote
                          (unless top-level "\"")))
                        "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/desc" nil nil)
                       ("cont" "context \"${1:modifier}\" do\n  $0\nend" "context \"modifier\" do ... end" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/cont" nil nil)
                       ("bef" "before$1 do\n  $0\nend\n" "before do ... end" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/bef" nil nil)
                       ("back" "background do\n  $0\nend" "background do ... end" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/back" nil nil)
                       ("aft" "after$1 do\n  $0\nend" "after do ... end" nil nil nil "/Users/jason/.emacs.d/elpa/rspec-mode-20151003.315/snippets/rspec-mode/aft" nil nil)))


;;; Do not edit! File generated at Sat Nov  7 12:16:42 2015
