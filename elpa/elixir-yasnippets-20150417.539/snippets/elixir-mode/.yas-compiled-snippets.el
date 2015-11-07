;;; Compiled snippets and support files for `elixir-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'elixir-mode
                     '(("unless" "unless ${1:false} do\n$0\nend\n" "unless" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/unless" nil nil)
                       ("test" "test \"$1\" do\n$0\nend\n" "test" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/test" nil nil)
                       ("Supervisor" "defmodule ${1:Module}.Supervisor do\nuse Supervisor\n\ndef start_link do\nSupervisor.start_link(__MODULE__, :ok)\nend\n\ndef init(:ok) do\nchildren = [\n$0\n]\nsupervise(children, strategy: :one_for_one)\nend\n\nend" "Supervisor" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/supervisor" nil nil)
                       ("receive" "receive do\n$0\nend\n" "receive" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/receive" nil nil)
                       ("pry" "require IEx; IEx.pry" "pry" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/pry" nil nil)
                       ("mdoc" "@moduledoc \"\"\"\n$0\n\"\"\"\n" "moduledoc" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/mdoc" nil nil)
                       ("if" "if ${1:true} do\n$0\nend\n" "if" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/if" nil nil)
                       ("GenServer" "defmodule ${1:Module} do\nuse GenServer\n\n# Client API\ndef start_link(default) do\nGenServer.start_link(__MODULE__, default)\nend\n\n# Server callbacks\ndef init(state) do\n{:ok, state}\nend\n\n$0\nend" "GenServer" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/genserver" nil nil)
                       ("GenEvent" "defmodule ${1:EventHandler} do\nuse GenEvent\n\ndef handle_event(event, parent) do\n${2:send parent, event}$0\n{:ok, parent}\nend\nend" "GenEvent" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/genevent" nil nil)
                       ("for" "for ${2:x} <- ${1:Enumeration}, do: $2$0\n" "for" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/for" nil nil)
                       ("fn" "fn ${1:x} -> $1$0 end\n" "fn" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/fn" nil nil)
                       ("doc" "@doc \"\"\"\n$0\n\"\"\"\n" "doc" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/doc" nil nil)
                       ("do" "do\n$0\nend" "do" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/do" nil nil)
                       ("defprotocol" "defprotocol ${1:Protocol} do\ndef ${0:callback(args)}\nend\n" "defprotocol" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/defprotocol" nil nil)
                       ("defp" "defp ${1:function}${2:(${3:args})} do\n$0\nend" "defp" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/defp" nil nil)
                       ("defmodule" "defmodule ${1:Module} do\n$0\nend\n" "defmodule" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/defmodule" nil nil)
                       ("defmacrop" "defmacrop ${1:Macro()} do\n$0\nend\n" "defmacrop" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/defmacrop" nil nil)
                       ("defmacro" "defmacro ${1:Macro()} do\n$0\nend\n" "defmacro" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/defmacro" nil nil)
                       ("defimpl" "defimpl ${1:Type}, for: ${2:Protocol} do\ndef ${0:callback(args)}\nend\n" "defimpl" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/defimpl" nil nil)
                       ("def" "def ${1:function}${2:(${3:args})} do\n$0\nend" "def" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/def" nil nil)
                       ("cond" "cond do\n$0\nend\n" "cond" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/cond" nil nil)
                       ("case" "case ${1:true} do\n$0\nend\n" "case" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/case" nil nil)
                       ("Behaviour" "defmodule ${1:MyBehaviour} do\nuse Behaviour\n\ndefcallback ${0:${2:function} :: ${3:any}}\nend" "Behaviour" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/behaviour" nil nil)
                       ("Application" "defmodule ${1:MyApp} do\nuse Application\n\ndef start(_type, _args) do\n$1.Supervisor.start_link()$0\nend\nend" "Application" nil nil nil "/Users/jason/.emacs.d/elpa/elixir-yasnippets-20150417.539/snippets/elixir-mode/application" nil nil)))


;;; Do not edit! File generated at Sat Nov  7 12:16:42 2015
