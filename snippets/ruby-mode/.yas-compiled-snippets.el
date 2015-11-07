;;; Compiled snippets and support files for `ruby-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'ruby-mode
                     '(("zip" "zip(${enums}) { |${row}| $0 }" "zip(...) { |...| ... }" nil
                        ("collections")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/zip" nil nil)
                       ("y" ":yields: $0" ":yields: arguments (rdoc)" nil
                        ("general")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/y" nil nil)
                       ("while" "while ${condition}\n  $0\nend" "while ... end" nil
                        ("control structure")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/while" nil nil)
                       ("when" "when ${condition}\n  $0\nend" "when ... end" nil
                        ("control structure")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/when" nil nil)
                       ("w" "attr_writer :" "attr_writer ..." nil
                        ("definitions")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/w" nil nil)
                       ("upt" "upto(${n}) { |${i}|\n  $0\n}" "upto(...) { |n| ... }" nil
                        ("control structure")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/upt" nil nil)
                       ("until" "until ${condition}\n  $0\nend" "until ... end" nil
                        ("control structure")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/until" nil nil)
                       ("tu" "require 'test/unit'" "tu" nil nil nil "/Users/jason/.emacs.d/snippets/ruby-mode/tu" nil nil)
                       ("to_" "def to_s\n    \"${1:string}\"\nend\n$0" "to_" nil nil nil "/Users/jason/.emacs.d/snippets/ruby-mode/to_" nil nil)
                       ("tim" "times { |${n}| $0 }" "times { |n| ... }" nil
                        ("control structure")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/tim" nil nil)
                       ("tc" "class TC_${1:Class} < Test::Unit::TestCase\n      $0\nend" "test class" nil nil nil "/Users/jason/.emacs.d/snippets/ruby-mode/test class" nil nil)
                       ("s" "#{$0}" "str" nil nil nil "/Users/jason/.emacs.d/snippets/ruby-mode/str" nil nil)
                       ("select" "select { |${1:element}| $0 }" "select { |...| ... }" nil
                        ("collections")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/select" nil nil)
                       ("rw" "attr_accessor :" "attr_accessor ..." nil
                        ("definitions")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rw" nil nil)
                       ("rreq" "require File.join(File.dirname(__FILE__), $0)" "require File.join(File.dirname(__FILE__), ...)" nil
                        ("general")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rreq" nil nil)
                       ("req" "require \"$0\"" "require \"...\"" nil
                        ("general")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/req" nil nil)
                       ("reject" "reject { |${1:element}| $0 }" "reject { |...| ... }" nil
                        ("collections")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/reject" nil nil)
                       ("rb" "#!/usr/bin/ruby -wKU\n" "/usr/bin/ruby -wKU" nil
                        ("general")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rb" nil nil)
                       ("r" "attr_reader :" "attr_reader ..." nil
                        ("definitions")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/r" nil nil)
                       ("mm" "def method_missing(method, *args)\n  $0\nend" "def method_missing ... end" nil
                        ("definitions")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/mm" nil nil)
                       ("inject" "inject(${1:0}) { |${2:injection}, ${3:element}| $0 }" "inject(...) { |...| ... }" nil
                        ("collections")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/inject" nil nil)
                       ("init" "def initialize(${1:args})\n    $0\nend" "init" nil nil nil "/Users/jason/.emacs.d/snippets/ruby-mode/init" nil nil)
                       ("ife" "if ${1:condition}\n  $2\nelse\n  $3\nend" "if ... else ... end" nil
                        ("control structure")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/ife" nil nil)
                       ("if" "if ${1:condition}\n  $0\nend" "if ... end" nil
                        ("control structure")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/if" nil nil)
                       ("form" "require 'formula'\n\nclass ${1:Name} <Formula\n  url '${2:url}'\n  homepage '${3:home}'\n  md5 '${4:md5}'\n\n  def install\n    ${5:system \"./configure\"}\n    $0\n  end\nend\n" "formula" nil nil nil "/Users/jason/.emacs.d/snippets/ruby-mode/formula" nil nil)
                       ("forin" "for ${1:element} in ${2:collection}\n  $0\nend" "for ... in ...; ... end" nil
                        ("control structure")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/forin" nil nil)
                       ("for" "for ${1:el} in ${2:collection}\n    $0\nend" "for" nil nil nil "/Users/jason/.emacs.d/snippets/ruby-mode/for" nil nil)
                       ("eawi" "each_with_index { |${e}, ${i}| $0 }" "each_with_index { |e, i| ... }" nil
                        ("collections")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/eawi" nil nil)
                       ("eav" "each_value { |${val}| $0 }" "each_value { |val| ... }" nil
                        ("collections")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/eav" nil nil)
                       ("eai" "each_index { |${i}| $0 }" "each_index { |i| ... }" nil
                        ("collections")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/eai" nil nil)
                       ("eac" "each_cons(${1:2}) { |${group}| $0 }" "each_cons(...) { |...| ... }" nil
                        ("collections")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/eac" nil nil)
                       ("ea" "each { |${e}| $0 }" "each { |...| ... }" nil
                        ("collections")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/ea" nil nil)
                       ("dow" "downto(${0}) { |${n}|\n  $0\n}" "downto(...) { |n| ... }" nil
                        ("control structure")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/dow" nil nil)
                       ("det" "detect { |${e}| $0 }" "detect { |...| ... }" nil
                        ("collections")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/det" nil nil)
                       ("deli" "delete_if { |${e} $0 }" "delete_if { |...| ... }" nil
                        ("collections")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/deli" nil nil)
                       ("def" "def ${1:method}${2:(${3:args})}\n    $0\nend" "def" nil nil nil "/Users/jason/.emacs.d/snippets/ruby-mode/def" nil nil)
                       ("dee" "Marshal.load(Marshal.dump($0))" "deep_copy(...)" nil
                        ("general")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/dee" nil nil)
                       ("deb" "debugger" "debugger" nil
                        ("debugging")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/debugger" nil nil)
                       ("cstub" "\nclass ${1:Resource}sController < ApplicationController\n\n  respond_to :html\n\n  def index\n    @${2:resource}s = $1.all\n    respond_with @$2s\n  end\n\n  def show\n    @$2 = $1.find params[:id]\n    respond_with @$2\n  end\n\n  def new\n    @$2 = $1.new\n    respond_with @$2\n  end\n\n  def create\n    @$2 = $1.new(params[:$2])\n    if @$2.save\n      cookies[:last_$2_id] = @$2.id\n      flash[:notice] = \"Successfully created $2.\"\n    end\n    respond_with @$2\n  end\n\n  def edit\n    @$2 = $1.find(params[:id])\n    respond_with @$2\n  end\n\n  def update\n    @$2 = $1.find params[:id]\n    if @$2.update_attributes params[:$2]\n      flash[:notice] = \"Successfully updated $2.\"\n    end\n    respond_with @$2\n  end\n\n  def destroy\n    @$2 = $1.find params[:id]\n    @$2.destroy\n    flash[:notice] = \"Successfully destroyed $2.\"\n    respond_with @$2 \n  end\n  \nend" "Controller Stub" nil
                        ("Rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/cstub" nil nil)
                       ("collect" "collect { |${e}| $0 }" "collect { |...| ... }" nil
                        ("collections")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/collect" nil nil)
                       ("cls" "class ${1:`(let ((fn (capitalize (file-name-nondirectory\n                                 (file-name-sans-extension\n				 (or (buffer-file-name)\n				     (buffer-name (current-buffer))))))))\n           (cond\n             ((string-match \"_\" fn) (replace-match \"\" nil nil fn))\n              (t fn)))`}\n  $0\nend\n" "class ... end" nil
                        ("definitions")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/cls" nil nil)
                       ("classify" "classify { |${e}| $0 }" "classify { |...| ... }" nil
                        ("collections")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/classify" nil nil)
                       ("cla" "class << ${self}\n  $0\nend" "class << self ... end" nil
                        ("definitions")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/cla" nil nil)
                       ("case" "case ${1:object}\nwhen ${2:condition}\n  $0\nend" "case ... end" nil
                        ("general")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/case" nil nil)
                       ("bp" "binding.pry" "binding.pry" nil
                        ("debugging")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/bp" nil nil)
                       ("bm" "Benchmark.bmbm(${1:10}) do |x|\n  $0\nend" "Benchmark.bmbm(...) do ... end" nil
                        ("general")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/bm" nil nil)
                       ("bench" "require \"benchmark\"\n\nTESTS = ${1:1_000}\nBenchmark.bmbm do |x|\n  x.report(\"${2:var}\") {}\nend\n" "bench" nil nil nil "/Users/jason/.emacs.d/snippets/ruby-mode/bench" nil nil)
                       ("@" "@${1:attr} = $0" "attribute" nil nil nil "/Users/jason/.emacs.d/snippets/ruby-mode/attribute" nil nil)
                       ("app" "if __FILE__ == $PROGRAM_NAME\n  $0\nend" "if __FILE__ == $PROGRAM_NAME ... end" nil
                        ("general")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/app" nil nil)
                       ("any" "any? { |${e}| $0 }" "any? { |...| ... }" nil
                        ("collections")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/any" nil nil)
                       ("am" "alias_method :${new_name}, :${old_name}" "alias_method new, old" nil
                        ("definitions")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/am" nil nil)
                       ("all" "all? { |${e}| $0 }" "all? { |...| ... }" nil
                        ("collections")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/all" nil nil)
                       ("$" "$${1:GLOBAL} = $0" "GLOB" nil nil nil "/Users/jason/.emacs.d/snippets/ruby-mode/GLOB" nil nil)
                       ("Comp" "include Comparable\n\ndef <=> other\n  $0\nend" "include Comparable; def <=> ... end" nil
                        ("definitions")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/Comp" nil nil)
                       ("=b" "=begin rdoc\n  $0\n=end" "=begin rdoc ... =end" nil
                        ("general")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/=b" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'ruby-mode
                     '(("xput" "xhr :put, :${1:update}, id: ${2:1}, ${3:object}: { $4 }$0" "xhr put" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/xhr-put.yasnippet" nil nil)
                       ("xpost" "xhr :post, :${1:create}, ${2:object}: { $3 }" "xhr post" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/xhr-post.yasnippet" nil nil)
                       ("xget" "xhr :get, :${1:show}${2:, id: ${3:1}}$0" "xhr get" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/xhr-get.yasnippet" nil nil)
                       ("xdelete" "xhr :delete, :${1:destroy}, id: ${2:1}$0" "xhr delete" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/xhr-delete.yasnippet" nil nil)
                       ("format" "format.${1:js|xml|html}${2: { $0 \\}}" "format.format" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/wants_format.yasnippet" nil nil)
                       ("verify" "verify only: [:$1], session: :user, params: :id, redirect_to: { action: '${2:index}' }\n" "verify — redirect" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/verify-redirect-(verify).yasnippet" nil nil)
                       ("verify" "verify only: [:$1], method: :post, render: { status: 500, text: \"use HTTP-POST\" }\n" "verify — render" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/verify-(verify).yasnippet" nil nil)
                       ("vuif" "validates_uniqueness_of :${1:attribute}${2:, on: :${3:create}, message: \"${4:must be unique}\", if: proc { |obj| ${6:obj.condition?} }}" "validates_uniqueness_of if" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_uniqueness_of-if-(vuif).yasnippet" nil nil)
                       ("vu" "validates_uniqueness_of :${1:attribute}${2:, on: :${3:create}, message: \"${4:must be unique}\"}" "validates_uniqueness_of" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_uniqueness_of-(vu).yasnippet" nil nil)
                       ("vpif" "validates_presence_of :${1:attribute}${2:, on: :${3:create}, message: \"${4:can't be blank}\"}, if: proc { |obj| ${5:obj.condition?} }}" "validates_presence_of if" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_presence_of-if-(vpif)-2.yasnippet" nil nil)
                       ("vp" "validates_presence_of :${1:attribute}${2:, on: :${3:create}, message: \"${4:can't be blank}\"}" "validates_presence_of" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_presence_of-(vp).yasnippet" nil nil)
                       ("vn" "validates_numericality_of :${1:attribute}${2:, on: :${3:create}, message: \"${4:is not a number}\"}" "validates_numericality_of" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_numericality_of.yasnippet" nil nil)
                       ("vnif" "validates_numericality_of :${1:attribute}${2:, on: :${3:create}, message: \"${4:is not a number}\"}, if: proc { |obj| ${5:obj.condition?} }}" "validates_numericality_of if" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_numericality_of-if.yasnippet" nil nil)
                       ("vlif" "validates_length_of :${1:attribute}, within: ${2:3..20}${3:, on: :${4:create}, message: \"${5:must be present}\"}, if: proc { |obj| ${6:obj.condition?} }}" "validates_length_of if" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_length_of-if.yasnippet" nil nil)
                       ("vl" "validates_length_of :${1:attribute}, within: ${2:3..20}${3:, on: :${4:create}, message: \"${5:must be present}\"}" "validates_length_of" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_length_of-(vl).yasnippet" nil nil)
                       ("vi" "validates_inclusion_of :${1:attribute}${2:, in: ${3:%w( ${4:mov avi} )}, on: :${5:create}, message: \"${6:extension %s is not included in the list}\"}" "validates_inclusion_of" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_inclusion_of.yasnippet" nil nil)
                       ("viif" "validates_inclusion_of :${1:attribute}${2:, in: ${3:%w( ${4:mov avi} )}, on: :${5:create}, message: \"${6:extension %s is not included in the list}\"}, if: proc { |obj| ${7:obj.condition?} }}" "validates_inclusion_of if" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_inclusion_of-if.yasnippet" nil nil)
                       ("vf" "validates_format_of :${1:attribute}, with: /${2:^[${3:\\w\\d}]+\\$}/${4:, on: :${5:create}, message: \"${6:is invalid}\"}" "validates_format_of" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_format_of.yasnippet" nil nil)
                       ("vfif" "validates_format_of :${1:attribute}, with: /${2:^[${3:\\w\\d}]+\\$}/${4:, on: :${5:create}, message: \"${6:is invalid}\"}, if: proc { |obj| ${7:obj.condition?} }}" "validates_format_of if" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_format_of-if.yasnippet" nil nil)
                       ("veif" "validates_exclusion_of :${1:attribute}${2:, in: ${3:%w( ${4:mov avi} )}, on: :${5:create}, message: \"${6:extension %s is not allowed}\"}, if: proc { |obj| ${7:obj.condition?} }}" "validates_exclusion_of if" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_exclusion_of-if-(veif).yasnippet" nil nil)
                       ("ve" "validates_exclusion_of :${1:attribute}${2:, in: ${3:%w( ${4:mov avi} )}, on: :${5:create}, message: \"${6:extension %s is not allowed}\"}" "validates_exclusion_of" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_exclusion_of-(ve).yasnippet" nil nil)
                       ("vcif" "validates_confirmation_of :${1:attribute}${2:, on: :${3:create}, message: \"${4:should match confirmation}\", if: proc { |obj| ${5:obj.condition?} }}" "validates_confirmation_of if" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_confirmation_of-if-(vcif).yasnippet" nil nil)
                       ("vc" "validates_confirmation_of :${1:attribute}${2:, on: :${3:create}, message: \"${4:should match confirmation}\"}" "validates_confirmation_of" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_confirmation_of-(vc).yasnippet" nil nil)
                       ("vaif" "validates_associated :${1:attribute}${2:, on: :${3:create}, if: proc { |obj| ${5:obj.condition?} }}" "validates_associated if" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_associated-if-(vaif).yasnippet" nil nil)
                       ("va" "validates_associated :${1:attribute}${2:, on: :${3:create}}" "validates_associated" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_associated-(va).yasnippet" nil nil)
                       ("vao" "validates_acceptance_of :${1:terms}${2:${3:, accept: \"${4:1}\"}${5:, message: \"${6:You must accept the terms of service}\"}}" "validates_acceptance_of" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_acceptance_of.yasnippet" nil nil)
                       ("vaoif" "validates_acceptance_of :${1:terms}${2:${3:, accept: \"${4:1}\"}${5:, message: \"${6:You must accept the terms of service}\"}}, if: proc { |obj| ${7:obj.condition?} }}" "validates_acceptance_of if" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/validates_acceptance_of-if.yasnippet" nil nil)
                       ("t." "t.timestamps\nt.$0" "t.timestamps (tctss)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/t_timestamps-(tctss).yasnippet" nil nil)
                       ("t." "t.timestamp :${1:title}\nt.$0" "t.timestamp (tcts)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/t_timestamp-(tcts).yasnippet" nil nil)
                       ("t." "t.time :${1:title}\nt.$0" "t.time (tcti)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/t_time-(tcti).yasnippet" nil nil)
                       ("t." "t.text :${1:title}\nt.$0" "t.text (tct)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/t_text-(tct).yasnippet" nil nil)
                       ("t." "t.string :${1:title}\nt.$0" "t.string (tcs)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/t_string-(tcs).yasnippet" nil nil)
                       ("t." "t.rename(:${1:old_column_name}, :${2:new_column_name})\nt.$0" "t.rename (tre)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/t_rename-(tre).yasnippet" nil nil)
                       ("t." "t.references :${1:taggable}${2:, polymorphic: ${3:{ default: '${4:Photo}' \\}}}\nt.$0" "t.references (tcr)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/t_references-(tcr).yasnippet" nil nil)
                       ("t." "t.integer :lock_version, null: false, default: 0\nt.$0" "t.lock_version (tcl)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/t_lock_version-(tcl).yasnippet" nil nil)
                       ("t." "t.integer :${1:title}\nt.$0" "t.integer (tci)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/t_integer-(tci).yasnippet" nil nil)
                       ("t." "t.float :${1:title}\nt.$0" "t.float (tcf)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/t_float-(tcf).yasnippet" nil nil)
                       ("t." "t.decimal :${1:title}${2:${3:, precision: ${4:10}}${5:, scale: ${6:2}}}\nt.$0" "t.decimal (tcd)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/t_decimal-(tcd).yasnippet" nil nil)
                       ("t." "t.datetime :${1:title}\nt.$0" "t.datetime (tcdt)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/t_datetime-(tcdt).yasnippet" nil nil)
                       ("t." "t.date :${1:title}\nt.$0" "t.date (tcda)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/t_date-(tcda).yasnippet" nil nil)
                       ("t." "t.boolean :${1:title}\nt.$0" "t.boolean (tcb)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/t_boolean-(tcb).yasnippet" nil nil)
                       ("t." "t.binary :${1:title}${2:, limit: ${3:2}.megabytes}\nt.$0" "t.binary (tcbi)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/t_binary-(tcbi).yasnippet" nil nil)
                       ("st" "= submit_tag \"${1:Save changes}\"${2:, id: \"${3:submit}\"}${4:, name: \"${5:$3}\"}${6:, class: \"${7:form_$3}\"}${8:, disabled: ${9:false}}${10:, disable_with: \"${11:Please wait...}\"}" "submit_tag" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/submit_tag.yasnippet" nil nil)
                       ("slt" "= stylesheet_link_tag ${1::application}${2:, media: \"${3:all}\"}${4:, cache: ${5:true}}" "stylesheet_link_tag" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/stylesheet_link_tag-(slt).yasnippet" nil nil)
                       ("scar" "scope :${1:name}, ->(${2:arg}){ where(${3:attribute:} $2) }\n" "scope with arguments" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/scope_with_args.yasnippet" nil nil)
                       ("sc" "scope :${1:name}, ->{ where(${2:attr:} ${3:value}) }\n" "scope" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/scope.yasnippet" nil nil)
                       ("resdm" "resources :${1:res_name} do\n  member do\n    ${2:get} :${3:action}\n  end\nend" "resources with a custom member block" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/routes_resources_member_block.yasnippet" nil nil)
                       ("resdc" "resources :${1:res_name} do\n  collection do\n    ${2:get} :${3:action}\n  end\nend" "resources with a custom collection block" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/routes_resources_collection_block.yasnippet" nil nil)
                       ("resd" "resources :${1:res_name} do\n  $2\nend" "resources with an empty params block" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/routes_resources_block.yasnippet" nil nil)
                       ("res" "resources :${1:res_name}" "resources :resource_name" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/routes_resources.yasnippet" nil nil)
                       ("reso" "resources :${1:res_name}, only: [:${2:index}]" "resources :name, only: [:actions]" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/routes_resource_only.yasnippet" nil nil)
                       ("returning" "returning ${1:variable} do${2/(^(?<var>\\s*[a-z_][a-zA-Z0-9_]*\\s*)(,\\g<var>)*,?\\s*$)|.*/(?1: |)/}${2:v}${2/(^(?<var>\\s*[a-z_][a-zA-Z0-9_]*\\s*)(,\\g<var>)*,?\\s*$)|.*/(?1:|)/}\n	$0\nend" "returning do |variable| … end" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/returning-do-7Cvariable7C-E280A6-end.yasnippet" nil nil)
                       ("rest" "respond_to do |format|\n	format.${1:html}${2: { $0 \\}}\nend" "respond_to" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/respond_to.yasnippet" nil nil)
                       ("ru" "render :update do |${2:page}|\n	$2.$0\nend" "render (update)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(update).yasnippet" nil nil)
                       ("rts" "render text: \"${1:text to render...}\", status: ${2:401}" "render (text, status)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(text-status)-(rts).yasnippet" nil nil)
                       ("rtlt" "render text: \"${1:text to render...}\", layout: ${2:true}" "render (text, layout: true)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(text-layout=3Etrue)-(rtlt).yasnippet" nil nil)
                       ("rtl" "render text: \"${1:text to render...}\", layout: \"${2:layoutname}\"" "render (text, layout)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(text-layout)-(rtl).yasnippet" nil nil)
                       ("rt" "render text: \"${1:text to render...}\"" "render (text)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(text)-(rt).yasnippet" nil nil)
                       ("rps" "render partial: \"${1:item}\", status: ${2:500}" "render (partial, status)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(partial-status)-(rps).yasnippet" nil nil)
                       ("rpo" "= render partial: \"${1:item}\", object: ${2:@$1}" "render (partial, object)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(partial-object)-(rpo).yasnippet" nil nil)
                       ("rpc" "= render partial: \"${1:item}\", collection: ${2:@$1s}" "render (partial, collection)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(partial-collection)-(rpc).yasnippet" nil nil)
                       ("rp" "= render \"${1:partial/path}\"${2:, ${3:var}: @${3:var}}" "render partial" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(partial)-(rp).yasnippet" nil nil)
                       ("rns" "render nothing: ${1:true}, status: ${2:401}" "render (nothing, status)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(nothing-status)-(rns).yasnippet" nil nil)
                       ("rn" "render nothing: ${1:true}" "render (nothing)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(nothing)-(rn).yasnippet" nil nil)
                       ("rl" "render layout: \"${1:layoutname}\"" "render (layout)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(layout)-(rl).yasnippet" nil nil)
                       ("rit" "render inline: \"${1:= 'hello' }\", type: ${2::rxml}" "render (inline, type)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(inline-type)-(rit).yasnippet" nil nil)
                       ("ril" "render inline: \"${1:= 'hello' }\", locals: { ${2:name}: \"${3:value}\"$4 }" "render (inline, locals)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(inline-locals)-(ril).yasnippet" nil nil)
                       ("ri" "= render inline: \"${1:= 'hello' }\"" "render (inline)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(inline)-(ri).yasnippet" nil nil)
                       ("rfu" "render file: \"${1:filepath}\", use_full_path: ${2:false}" "render (file, use_full_path)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(file-use_full_path)-(rfu).yasnippet" nil nil)
                       ("rf" "render file: \"${1:filepath}\"" "render (file)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(file)-(rf).yasnippet" nil nil)
                       ("ral" "render action: \"${1:action}\", layout: \"${2:layoutname}\"" "render (action, layout)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(action-layout)-(ral).yasnippet" nil nil)
                       ("ra" "render action: \"${1:action}\"" "render (action)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/render-(action)...-(ra).yasnippet" nil nil)
                       ("repp" "redirect_to(${2:${10:model}s_path})" "redirect_to (path plural)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/redirect_to-(path-plural).yasnippet" nil nil)
                       ("rep" "redirect_to(${2:${12:model}_path(${13:@}${14:$12})})" "redirect_to (path)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/redirect_to-(path).yasnippet" nil nil)
                       ("renpp" "redirect_to(${2:${10:parent}_${11:child}_path(${12:@}${13:$10})})" "redirect_to (nested path plural)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/redirect_to-(nested-path-plural).yasnippet" nil nil)
                       ("renp" "redirect_to(${2:${12:parent}_${13:child}_path(${14:@}${15:$12}, ${16:@}${17:$13})})" "redirect_to (nested path)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/redirect_to-(nested-path).yasnippet" nil nil)
                       ("recai" "redirect_to controller: \"${1:items}\", action: \"${2:show}\", id: ${0:@item}" "redirect_to (controller, action, id)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/recai.yasnippet" nil nil)
                       ("reca" "redirect_to controller: \"${1:items}\", action: \"${2:list}\"" "redirect_to (controller, action)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/reca.yasnippet" nil nil)
                       ("rec" "redirect_to controller: \"${1:items}\"" "redirect_to (controller)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/rec.yasnippet" nil nil)
                       ("reai" "redirect_to action: \"${1:show}\", id: ${0:@item}" "redirect_to (action, id)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/reai.yasnippet" nil nil)
                       ("rea" "redirect_to action: \"${1:index}\"" "redirect_to (action)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/rea.yasnippet" nil nil)
                       ("flash" "flash[:${1:notice}] = \"${2:Successfully created...}\"$0" "flash[…]" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/rails-flash.yasnippet" nil nil)
                       ("lsc" "scope :name, ->(${1:param}){ ${3:['${4:${5:field} = ?}', ${6:$1}]} }\n" "lambda" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/named_scope-lambda.yasnippet" nil nil)
                       ("mrw" "mattr_accessor :${0:attr_names}" "mattr_accessor" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/mattr_accessor.yasnippet" nil nil)
                       ("mp" "map(&:${1:id})" "map(&:sym_proc)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/map(-3Asym_proc).yasnippet" nil nil)
                       ("logw" "logger.warn { \"${1:message}\" }$0" "logger.warn" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/logger_warn.yasnippet" nil nil)
                       ("logi" "logger.info { \"${1:message}\" }$0" "logger.info" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/logger_info.yasnippet" nil nil)
                       ("logf" "logger.fatal { \"${1:message}\" }$0" "logger.fatal" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/logger_fatal.yasnippet" nil nil)
                       ("loge" "logger.error { \"${1:message}\" }$0" "logger.error" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/logger_error.yasnippet" nil nil)
                       ("logd" "logger.debug { \"${1:message}\" }$0" "logger.debug" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/logger_debug.yasnippet" nil nil)
                       ("lim" "= link_to ${1:model}.${2:name}, ${3:${4:$1}_path(${14:$1})}" "link_to model" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/link_to-model.yasnippet" nil nil)
                       ("lipp" "= link_to ${1:\"${2:link text...}\"}, ${3:${4:model}s_path}" "link_to (path plural)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/link_to-(path-plural).yasnippet" nil nil)
                       ("lip" "= link_to ${1:\"${2:link text...}\"}, ${3:${12:model}_path(${13:@}${14:$12})}" "link_to (path)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/link_to-(path).yasnippet" nil nil)
                       ("linpp" "= link_to ${1:\"${2:link text...}\"}, ${3:${10:parent}_${11:child}_path(${12:@}${13:$10})}" "link_to (nested path plural)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/link_to-(nested-path-plural).yasnippet" nil nil)
                       ("linp" "= link_to ${1:\"${2:link text...}\"}, ${3:${12:parent}_${13:child}_path(${14:@}${15:$12}, ${16:@}${17:$13})}" "link_to (nested path)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/link_to-(nested-path).yasnippet" nil nil)
                       ("licai" "= link_to \"${1:link text...}\", controller: \"${2:items}\", action: \"${3:edit}\", id: ${4:@item}" "link_to (controller, action, id)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/licai.yasnippet" nil nil)
                       ("lica" "= link_to \"${1:link text...}\", controller: \"${2:items}\", action: \"${3:index}\"" "link_to (controller, action)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/lica.yasnippet" nil nil)
                       ("lic" "= link_to \"${1:link text...}\", controller: \"${2:items}\"" "link_to (controller)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/lic.yasnippet" nil nil)
                       ("liai" "= link_to \"${1:link text...}\", action: \"${2:edit}\", id: ${3:@item}" "link_to (action, id)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/liai.yasnippet" nil nil)
                       ("lia" "= link_to \"${1:link text...}\", action: \"${2:index}\"" "link_to (action)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/lia.yasnippet" nil nil)
                       ("jit" "= javascript_include_tag ${1::application}${2:, cache: ${3:true}}" "javascript_include_tag" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/javascript_include_tag-(jit).yasnippet" nil nil)
                       ("defi" "def initialize(${1:attribute})\n  @$1 = $1\nend" "Define initializer method" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/initializer-method.yasnippet" nil nil)
                       ("ist" "= image_submit_tag(\"${1:agree.png}\"${2:${3:, id: \"${4:${1/^(\\w+)(\\.\\w*)?$/$1/}}\"}${5:, name: \"${6:${1/^(\\w+)(\\.\\w*)?$/$1/}}\"}${7:, class: \"${8:${1/^(\\w+)(\\.\\w*)?$/$1/}-button}\"}${9:, disabled: ${10:false}}})" "image_submit_tag" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/image_submit_tag.yasnippet" nil nil)
                       ("hot" "has_one :${1:object}, through: :${2:join_association}${3:, source: :${4:$2_table_foreign_key_to_$1_table}}" "has_one through" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/has_one_through-(hot).yasnippet" nil nil)
                       ("ho" "has_one :${1:object}${2:, class_name: \"${3:${1/[[:alpha:]]+|(_)/(?1::\\u$0)/g}}\", foreign_key: \"${4:$1_id}\"}" "has_one" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/has_one-(ho).yasnippet" nil nil)
                       ("hmd" "has_many :${1:object}s${2:, class_name: \"$1\", foreign_key: \"${4:reference}_id\"}, dependent: :destroy$0" "has_many dependent: :destroy" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/has_many-dependent-=-destroy.yasnippet" nil nil)
                       ("hmt" "has_many :${1:objects}, through: :${2:join_association}${3:, source: :${4:$2_table_foreign_key_to_$1_table}}" "has_many (through)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/has_many-(through).yasnippet" nil nil)
                       ("hm" "has_many :${1:object}s${2:, class_name: \"$1\", foreign_key: \"${4:reference}_id\"}" "has_many" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/has_many-(hm).yasnippet" nil nil)
                       ("habtm" "has_and_belongs_to_many :${1:object}${2:, join_table: \"${3:table_name}\", foreign_key: \"${4:$1_id}\"}" "has_and_belongs_to_many" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/has_and_belongs_to_many-(habtm).yasnippet" nil nil)
                       ("ff" "= form_for @${1:model} do |f|\n  $0\nend" "form_for" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for.yasnippet" nil nil)
                       ("ffe" "= error_messages_for :${1:model}\n\n= form_for @${2:$1} do |f| -\n  $0\n" "form_for with errors" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-with-errors.yasnippet" nil nil)
                       ("f." "= f.text_field :${1:attribute}" "f.text_field (fftf)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-text_field.yasnippet" nil nil)
                       ("fftf" "= f.text_field :${1:attribute}" "form_for text_field" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-text_field-2.yasnippet" nil nil)
                       ("f." "= f.text_area :${1:attribute}" "f.text_area (ffta)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-text_area.yasnippet" nil nil)
                       ("ffta" "= f.text_area :${1:attribute}" "form_for text_area" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-text_area-2.yasnippet" nil nil)
                       ("f." "= f.submit \"${1:Submit}\"${2:, disable_with: '${3:$1ing...}'}" "f.submit (ffs)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-submit.yasnippet" nil nil)
                       ("ffs" "= f.submit \"${1:Submit}\"${2:, disable_with: '${3:$1ing...}'}" "form_for submit" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-submit-2.yasnippet" nil nil)
                       ("f." "= f.radio_box :${1:attribute}, :${2:tag_value}" "f.radio_box (ffrb)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-radio_box.yasnippet" nil nil)
                       ("ffrb" "= f.radio_box :${1:attribute}, :${2:tag_value}" "form_for radio_box" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-radio_box-2.yasnippet" nil nil)
                       ("f." "= f.password_field :${1:attribute}" "f.password_field (ffpf)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-password_field.yasnippet" nil nil)
                       ("ffpf" "= f.password_field :${1:attribute}" "form_for password_field" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-password_field-2.yasnippet" nil nil)
                       ("f." "= f.label :${1:attribute}${2:, \"${3:${1/[[:alpha:]]+|(_)/(?1: :\\u$0)/g}}\"}" "f.label (ffl)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-label.yasnippet" nil nil)
                       ("ffl" "= f.label :${1:attribute}${2:, \"${3:${1/[[:alpha:]]+|(_)/(?1: :\\u$0)/g}}\"}" "form_for label" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-label-2.yasnippet" nil nil)
                       ("f." "= f.hidden_field :${1:attribute}" "f.hidden_field (ffhf)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-hidden_field.yasnippet" nil nil)
                       ("ffhf" "= f.hidden_field :${1:attribute}" "form_for hidden_field" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-hidden_field-2.yasnippet" nil nil)
                       ("f." "= f.file_field :${1:attribute}" "f.file_field (ffff)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-file_field.yasnippet" nil nil)
                       ("ffff" "= f.file_field :${1:attribute}" "form_for file_field" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-file_field-2.yasnippet" nil nil)
                       ("f." "= f.check_box :${1:attribute}" "f.check_box (ffcb)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-checkbox.yasnippet" nil nil)
                       ("ffcb" "= f.check_box :${1:attribute}" "form_for check_box" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/form_for-check_box.yasnippet" nil nil)
                       ("for" "- if !${1:list}.blank?\n  - for ${2:item} in $1\n    $3\n\n- else\n  $4\n" "for loop in rhtml" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/for-loop-erb.yasnippet" nil nil)
                       ("fp" "find(params[:${1:id}])" "find(params[:id])" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/find_params_id_(fp).yasnippet" nil nil)
                       ("fini" "find(${1:id})" "find(id)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/find(id).yasnippet" nil nil)
                       ("finf" "find(:first${1:, conditions: ['${2:${3:field} = ?}', ${5:true}]})" "find(:first)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/find(3Afirst).yasnippet" nil nil)
                       ("fina" "find(:all${1:, conditions: ['${2:${3:field} = ?}', ${5:true}]})" "find(:all)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/find(3Aall).yasnippet" nil nil)
                       ("deftp" "def test_should_post_${1:action}\n	${3:@$2 = ${4:$2s}(:${5:fixture_name})\n	}post :$1${6:, id: @$2.to_param}, ${2:model}: { $0 }\n	assert_response :redirect\n\nend" "def test_should_post_action" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/def-post-request.yasnippet" nil nil)
                       ("deftg" "def test_should_get_${1:action}\n	${2:@${3:model} = ${4:$3s}(:${5:fixture_name})\n	}get :$1${6:, id: @$3.to_param}\n	assert_response :success\n	$0\nend" "def test_should_get_action" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/def-get-request.yasnippet" nil nil)
                       ("defcreate" "def create\n  @${1:model} = ${2:${1/[[:alpha:]]+|(_)/(?1::\\u$0)/g}}.new(params[:$1])\n  $0\n  respond_to do |format|\n    if @$1.save\n      flash[:notice] = '$2 was successfully created.'\n      format.html { redirect_to(@$1) }\n      format.xml { render xml: @$1, status: :created, location: @$1 }\n    else\n      format.html { render action: \"new\" }\n      format.xml { render xml: @$1.errors, status: :unprocessable_entity }\n    end\n  end\nend\n" "def create - resource" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/def-create-resource.yasnippet" nil nil)
                       ("crud" "\n  def index\n    @$1 = ${1/(.+)/\\u$1/g}.scoped\n  end\n\n  def show\n    @$1 = ${1/(.+)/\\u$1/g}.find(params[:id])\n  end\n\n  def create\n    @$1 = ${1/(.+)/\\u$1/g}.new(params[:$1])\n  end\n\n  def edit\n    @$1 = ${1/(.+)/\\u$1/g}.find(params[:id])\n  end\n\n  def destroy\n    $1 = ${1/(.+)/\\u$1/g}.find(params[:id])\n  end" "crud actions" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/crud-actions.yasnippet" nil nil)
                       ("class" "class ${1:${TM_FILENAME/(?:\\A|_)([A-Za-z0-9]+)(?:\\.rb)?/(?2::\\u$1)/g}}\n$0\nend" "class NAME end" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/class-end.yasnippet" nil nil)
                       ("crw" "cattr_accessor :${0:attr_names}" "cattr_accessor" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/cattr_accessor.yasnippet" nil nil)
                       ("bt" "belongs_to :${1:object}${2:, class_name: \"${3:${1/[[:alpha:]]+|(_)/(?1::\\u$0)/g}}\", foreign_key: \"${4:$1_id}\"}" "belongs_to" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/belongs_to-(bt).yasnippet" nil nil)
                       ("befvou" "before_validation ${1:validation_method}, on: :update" "before_validation on update" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/before_validation_on_update.yasnippet" nil nil)
                       ("befvoc" "before_validation ${1:validation_method}, on: :create" "before_validation on create" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/before_validation_on_create.yasnippet" nil nil)
                       ("befv" "before_validation " "before_validation" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/before_validation.yasnippet" nil nil)
                       ("befu" "before_update " "before_update" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/before_update.yasnippet" nil nil)
                       ("befs" "before_save " "before_save" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/before_save.yasnippet" nil nil)
                       ("befd" "before_destroy " "before_destroy" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/before_destroy.yasnippet" nil nil)
                       ("befc" "before_create " "before_create" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/before_create.yasnippet" nil nil)
                       ("ass" "assert_select '${1:path}'${2:, ${3:text}: ${4:'${5:inner_html}'}}${6: do\n	$0\nend}" "assert_select" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/assert_select.yasnippet" nil nil)
                       ("artpp" "assert_redirected_to ${10:${2:model}s_path}" "assert_redirected_to (path plural)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/assert_redirected_to-(path-plural).yasnippet" nil nil)
                       ("artp" "assert_redirected_to ${2:${12:model}_path(${13:@}${14:$12})}" "assert_redirected_to (path)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/assert_redirected_to-(path).yasnippet" nil nil)
                       ("artnpp" "assert_redirected_to ${10:${2:parent}_${3:child}_path(${4:@}${5:$2})}" "assert_redirected_to (nested path plural)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/assert_redirected_to-(nested-path-plural).yasnippet" nil nil)
                       ("artnp" "assert_redirected_to ${2:${12:parent}_${13:child}_path(${14:@}${15:$12}, ${16:@}${17:$13})}" "assert_redirected_to (nested path)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/assert_redirected_to-(nested-path).yasnippet" nil nil)
                       ("asnd" "assert_no_difference \"${1:Model}.${2:count}\" do\n  $0\nend" "assert_no_difference" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/assert_no_difference.yasnippet" nil nil)
                       ("asd" "assert_difference \"${1:Model}.${2:count}\", ${3:1} do\n  $0\nend" "assert_difference" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/assert_difference.yasnippet" nil nil)
                       ("asg" "assert(${1:var} = assigns(:$1), \"Cannot find @$1\")\n$0" "assert(var = assigns(:var))" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/assert(var-=-assigns(3Avar)).yasnippet" nil nil)
                       ("aftvou" "after_validation ${1:validation_method}, on: :update" "after_validation on update" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/after_validation_on_update.yasnippet" nil nil)
                       ("aftvoc" "after_validation ${1:validation_method}, on: :create" "after_validation on create" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/after_validation_on_create.yasnippet" nil nil)
                       ("aftv" "after_validation " "after_validation" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/after_validation.yasnippet" nil nil)
                       ("aftu" "after_update " "after_update" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/after_update.yasnippet" nil nil)
                       ("afts" "after_save " "after_save" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/after_save.yasnippet" nil nil)
                       ("aftd" "after_destroy " "after_destroy" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/after_destroy.yasnippet" nil nil)
                       ("aftc" "after_create " "after_create" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/after_create.yasnippet" nil nil)
                       ("arc" "class ${1:${TM_FILENAME/(?:\\A|_)([A-Za-z0-9]+)(?:\\.rb)?/(?2::\\u$1)/g}} < ActiveRecord::Base\n$0\nend" "Active Record model" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/active-record-class-end.yasnippet" nil nil)
                       ("anaf" "accepts_nested_attributes_for :${1:relationship}, ${2:reject_if: ->(attributes){ attributes['name'].blank? \\}}${3, :allow_destroy: true}" "accepts_nested_attributes_for" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/accepts_nested_attributes_for.yasnippet" nil nil)
                       ("asre" "assert_response :${1:success}, @response.body$0" "assert_response" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Test-Assert-Response-(are).yasnippet" nil nil)
                       ("art" "assert_redirected_to ${2:action: \"${1:index}\"}" "assert_redirected_to" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Test-Assert-Redirected-To-(art).yasnippet" nil nil)
                       ("tre" "t.rename(:${1:old_column_name}, :${2:new_column_name})\n$0" "Table column(s) rename" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Table-column(s)-rename.yasnippet" nil nil)
                       ("rdb" "RAILS_DEFAULT_LOGGER.debug \"${1:message}\"$0" "RAILS_DEFAULT_LOGGER.debug" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/RAILS_DEFAULT_LOGGER.debug-(rdb).yasnippet" nil nil)
                       ("mmvc" "rename_column :${1:table}, :${2:old_column_name}, :${3:new_column_name}" "Rename Column" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Migration-Rename-Column.yasnippet" nil nil)
                       ("mrmi" "remove_index :${1:table}, name: :${2:index_name}" "Remove Index" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Migration-Remove-Index.yasnippet" nil nil)
                       ("mrmc" "remove_column :${1:table}, :${2:column}" "Remove Column" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Migration-Remove-Column.yasnippet" nil nil)
                       ("mdtab" "drop_table :${1:table}" "Drop Table" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Migration-Drop-Table-(mdct).yasnippet" nil nil)
                       ("mtab" "create_table :${1:table} do |t|\n  t.${2:string} :${3:name}\n\n  t.timestamps\nend\n" "Create Table" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Migration-Create-Table-(mdct).yasnippet" nil nil)
                       ("mcct" "t.column ${1:title}, :${2:string}\nmccc$0" "Create Several Columns in Table" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Migration-Create-Column-Continue-(mccc).yasnippet" nil nil)
                       ("mcolt" "t.column ${1:title}, :${2:string}\n$0" "Create Column in Table" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Migration-Create-Column-(mcc).yasnippet" nil nil)
                       ("mccol" "change_column :${1:table}, :${2:column}, :${3:string}${4:, ${5:limit}: ${6:255}}" "Change Column" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Migration-Change-Column.yasnippet" nil nil)
                       ("mind" "add_index :${1:table}, :${2:columns}${4:, ${5:name}: \"${6:custom_index_name}\"}" "Add Index" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Migration-Add-Index.yasnippet" nil nil)
                       ("mcol" "add_column :${1:table}, :${2:column}, :${3:string}${4:, ${5:limit}: ${6:255}}" "Add Column" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Migration-Add-Column.yasnippet" nil nil)
                       ("$L" "\\$LABEL" "$LABEL" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/LABEL.yasnippet" nil nil)
                       ("tctss" "t.timestamps\n$0" "Table column timestamps" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Create-timestamps-columns.yasnippet" nil nil)
                       ("tcts" "t.timestamp :${1:title}\n$0" "Table column timestamp" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Create-timestamp-column.yasnippet" nil nil)
                       ("tcti" "t.time :${1:title}\n$0" "Table column time" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Create-time-column.yasnippet" nil nil)
                       ("tct" "t.text :${1:title}\n$0" "Table column text" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Create-text-column.yasnippet" nil nil)
                       ("tcs" "t.string :${1:title}\n$0" "Table column string" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Create-string-column.yasnippet" nil nil)
                       ("tcr" "t.references :${1:taggable}${2:, polymorphic: ${3:{ default: '${4:Photo}' \\}}}\n$0" "Table column(s) references" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Create-references-column.yasnippet" nil nil)
                       ("tcl" "t.integer :lock_version, null: false, default: 0\n$0" "Table column lock_version" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Create-lock_version-column.yasnippet" nil nil)
                       ("tci" "t.integer :${1:title}\n$0" "Table column integer" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Create-integer-column.yasnippet" nil nil)
                       ("cla" "require File.dirname(__FILE__) + '/../test_helper'\n\nclass ${1:Model}ControllerTest < ActionController::TestCase\n	deft$0\nend\n" "Create functional test class" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Create-functional-test-class.yasnippet" nil nil)
                       ("tcf" "t.float :${1:title}\n$0" "Table column float" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Create-float-column.yasnippet" nil nil)
                       ("tcd" "t.decimal :${1:title}${2:${3:, precision: ${4:10}}${5:, scale: ${6:2}}}\n$0" "Table column decimal" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Create-decimal-column.yasnippet" nil nil)
                       ("tcdt" "t.datetime :${1:title}\n$0" "Table column datetime" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Create-datetime-column.yasnippet" nil nil)
                       ("tcda" "t.date :${1:title}\n$0" "Table column date" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Create-date-column.yasnippet" nil nil)
                       ("con" "class ${1:${TM_FILENAME/(?:\\A|_)([A-Za-z0-9]+)(?:\\.rb)?/(?2::\\u$1)/g}} < ${2:Application}Controller\n  $3\nend" "Create controller class" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Create-controller-class.yasnippet" nil nil)
                       ("tcb" "t.boolean :${1:title}\n$0" "Table column boolean" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Create-boolean-column.yasnippet" nil nil)
                       ("tcbi" "t.binary :${1:title}${2:, limit: ${3:2}.megabytes}\n$0" "Table column binary" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/Create-binary-column.yasnippet" nil nil)
                       ("fi" "= Fixtures.identify(:${1:name}) $0" "Fixtures.identify(:symbol)" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/3C=-Fixtures_identify(3Asymbol)-3E.yasnippet" nil nil)
                       ("ft" "= form_tag(${1:action: \"${5:update}\"}${6:, {:${8:class}: \"${9:form}\"\\}}) do\n  $0\nend" "form_tag" nil
                        ("rails")
                        nil "/Users/jason/.emacs.d/snippets/ruby-mode/rails/180-rails-form_tag.yasnippet" nil nil)))


;;; Do not edit! File generated at Sat Nov  7 12:16:41 2015
