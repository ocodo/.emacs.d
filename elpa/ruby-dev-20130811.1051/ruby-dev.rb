#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require 'rubygems'
gem 'pry',  '~> 0.9'
gem 'yard', '~> 0.8'

require 'set'
require 'json'
require 'fiber'
require 'socket'

require 'pry'
begin
  require 'pry-doc'
rescue LoadError
end

require 'yard'

# Module to make a process able to communicate with Emacs (or another potential
# client that understands this JSON-based protocol).
#
# You can use {RubyDev.run} or {RubyDev.run_server} to actually start the
# communication (through standard input streams and a TCP server, respectively).
class RubyDev
  DefaultHost = "127.0.0.1"
  DefaultPort = 6475

  # @return [Integer] Size of chunks to read
  BlockSize = 1024

  # An input object for Pry, that actually just feeds data to the user of a
  # Fiber.
  #
  # @see RubyDev::Output
  class Input
    # @param [String] prompt
    # @return [String] Line read from the client
    def readline(prompt)
      Fiber.yield :read, prompt
    end

    # @return [Proc]
    attr_accessor :completion_proc
  end

  # An output object for Pry, works like the Input class.
  #
  # @see RubyDev::Input
  class Output
    def write(data)
      Fiber.yield :write, data.to_s
      data.size
    end

    def <<(data)
      Fiber.yield :write, data.to_s
      self
    end

    def print(*strings)
      strings.each do |str|
        Fiber.yield :write, str.to_s
      end

      nil
    end

    # Notice this needs to special case Array, since IO#puts does that
    # too. Instead of printing the result of Array#to_s, we need to print
    # every element on a new line.
    def puts(*lines)
      Fiber.yield :write, "\n" if lines.empty?

      lines.each do |line|
        if line.is_a? Array
          line.each { |sub_line| puts(sub_line) }
        else
          Fiber.yield :write, line.to_s.chomp + "\n"
        end
      end

      nil
    end
  end

  # A wrapper to communicate with the actual {Pry} session.
  #
  # @attr [Fiber] fiber
  # @attr [RubyDev::Input]  input
  # @attr [RubyDev::Output] output
  REPL = Struct.new(:fiber, :input, :output)

  # Starts RubyDev, using standard input streams.
  def self.run
    o = new
    o.run
  ensure
    o.clean_up
  end

  # Starts a RubyDev server.
  #
  # @param [String, nil] host If nil, it defaults to the RUBY_DEV_HOST env
  #   variable or {RubyDev::DefaultHost}
  # @param [Integer, nil] port If nil, it defaults to the RUBY_DEV_PORT env
  #   variable or {RubyDev::DefaultPort}
  def self.run_server(host = nil, port = nil)
    host ||= ENV["RUBY_DEV_HOST"] || RubyDev::DefaultHost
    port ||= (p = ENV["RUBY_DEV_PORT"]) ? p.to_i : RubyDev::DefaultPort

    TCPServer.open(host, port) do |server|
      clients = {}

      while out = IO.select([server] + clients.keys) and out[0]
        out[0].each do |io|
          if io == server
            if socket = server.accept_nonblock
              client = RubyDev.new(socket, socket)
              clients[socket] = client
            end
          else
            clients.delete io unless clients[io].process_io
          end
        end
      end
    end
  end

  @commands = {}

  def self.commands
    @commands
  end

  # Defines a handler for certain commands.
  #
  # @param [String] name Name fo the query to process.
  # @yieldparam [Hash] query JSON object received from the client
  # @yieldreturn [Hash] The object to send back to the client
  def self.command(name, &block)
    commands[name] = block
  end

  def initialize(input = $stdin, output = $stdout, error = $stderr)
    @input  = input
    @output = output
    @error  = error

    @repls = {}

    # for asynchronous processing.
    @buffer = ""
  end

  def clean_up
    # Nothing, for now
  end

  def commands
    self.class.commands
  end

  # Main loop for synchronous processing.
  #
  # Each line received from input is parsed as a JSON object, and the
  # corresponding handler is then run.
  def run
    @input.each_line do |line|
      process_line(line)
    end
  end

  # Function to call to further process the input, meant to be called only
  # when there is such input (e.g. when IO.select notified us of it).
  #
  # @return [Boolean] True if there's still input to process, false otherwise.
  def process_io
    if @input.eof?
      clean_up
      @input.close
      false
    else
      receive @input.read_nonblock(BlockSize)
      true
    end
  end

  # Adds a string to the buffer, and processes complete lines that have been
  # found in it.
  #
  # @param [String] text
  def receive(text)
    @buffer << text

    while pos = @buffer.index("\n")
      process_line @buffer.slice!(0..pos)
    end
  end

  # Processes a single line.
  def process_line(line)
    begin
      query = JSON.parse(line)
      write_result dispatch(query)
    rescue JSON::JSONError => e
      @error.puts "#{e.class}: #{e.message}"
    end
  end

  # Attempts to run the correct handler for a certain type of query.
  def dispatch(query)
    if c = commands[query["type"]]
      instance_exec(query, &c)
    else
      {:success   => false,
       :error     => "Unknown query type: #{query["type"]}",
       :backtrace => []}
    end
  rescue Exception => e
    begin
      {:success => false, :error => "#{e.class}: #{e.message}",
       :backtrace => e.backtrace}
    rescue Exception
      # if the user is trying to break stuff, this can happen
      {:success => false, :error => "(unknown error)", :backtrace => []}
    end
  end

  # Writes an object to the output.
  #
  # @param [Hash] object Object to write to the output
  def write_result(object)
    @output.puts object.to_json
  end

  # Evalutes some arbitrary expression.
  command "eval" do |query|
    object = TOPLEVEL_BINDING.eval(query["code"], query["filename"],
                                   query["line"])
    {:success => true, :result => object.inspect}
  end

  # Searches for symbols that start with a given input.
  command "search-doc" do |query|
    search = query["input"]
    {
      :success => true,
      :completions => if limit = query["limit"]
                        search_symbol(search).take(limit)
                      else
                        search_symbol(search).to_a
                      end
    }
  end

  # Recursively searches for a symbol.
  #
  # @param [String] search Prefix required for the symbol
  # @param [Module] mod Module to search for symbols in
  # @param [Set] seen Modules that have already been traversed
  #
  # @yieldparam [String] symbol Symbol that matches the research
  def search_symbol(search, mod = Object, seen = Set.new, &block)
    # Warnings are silenced because this code may trigger irrelevant deprecation
    # warnings (e.g. we're accessing ::Config).
    old_warn, $WARN = $WARN, false

    return to_enum(__method__, search, mod, seen) unless block
    return if seen.include? mod

    seen << mod

    possible_source = mod.name &&
      (mod.name.start_with?(search) ||
       search.start_with?(mod.name))

    if possible_source
      yield mod.name if mod.name.start_with? search

      [[mod.methods, "."], [mod.instance_methods, "#"]].each do |(mlist, sep)|
        mlist.each do |m|
          name = "#{mod.name}#{sep}#{m}"
          yield name if name.start_with? search
        end
      end
    end

    mod.constants(false).each do |const|
      begin
        val = mod.const_get(const)
      rescue NameError, LoadError
        next
      end

      if Module === val
        search_symbol(search, val, seen, &block)
      else
        begin
          const_name = "#{mod.name}::#{const}"
          yield const_name if const_name.start_with? search
        rescue Exception
          # just assume this is a weird (e.g. BasicObject) constant.
        end
      end
    end if possible_source || mod == Object
  ensure
    $WARN = old_warn
  end

  # Retrieves documentation-related informations about a specific symbol.
  command "object-info" do |query|
    symbol = query["symbol"]

    if doc = Pry::WrappedModule.from_str(symbol)
      # HACK: Pry::WrappedModule doesn't let us retrieve the wrapped object,
      # which we need if want to be able to use methods.

      wrapped = binding.eval(symbol)

      is_class = wrapped.is_a? Class

      superclass = wrapped.superclass if is_class

      {
        :success            => true,
        :symbol             => symbol,
        :type               => is_class ? :class : :module,
        :'source-location'  => doc.source_location,
        :superclass         => (superclass.name if superclass),
        :'included-modules' => doc.included_modules,

        :methods => {
          :new => wrapped.methods(false),
          :old => wrapped.methods - Class.instance_methods -
                  wrapped.methods(false),
        },
        :'instance-methods' => {
          :new => doc.instance_methods(false),
          :old => doc.instance_methods - doc.instance_methods(false),
        },

        :source => begin
                     doc.source
                   rescue Pry::CommandError
                   end,
        :doc    => begin
                     parse_doc(doc.doc)
                   rescue Pry::CommandError
                   end
      }
    elsif doc = Pry::Method.from_str(symbol)
      {
        :success           => true,
        :symbol            => symbol,
        :type              => :method,
        :'source-location' => doc.source_location,
        :language          => doc.source_type,
        :visibility        => doc.visibility,
        :signature         => (s = doc.signature) && s[s.index('(')..-1],
        :source            => begin
                                doc.source
                              rescue Pry::CommandError,
                                MethodSource::SourceNotFoundError
                              end,
        :doc               => begin
                                parse_doc(doc.doc)
                              rescue Pry::CommandError
                              end
      }
    else
      {:success   => false, :error => "Can't find object: #{symbol}",
       :backtrace => []}
    end
  end

  def parse_doc(doc)
    if doc
      docstring = YARD::DocstringParser.new.parse(doc).to_docstring
      doc_tag_to_hash(docstring)
    end
  end

  def doc_tag_to_hash(tag)
    case tag
    when YARD::Tags::Tag
      default       =  {
        :'tag-name' => tag.tag_name,
        :name       => tag.name,
        :types      => tag.types,
        :text       => tag.text
      }

      default.merge case tag
                    when YARD::Tags::OverloadTag
                      {
                       :parameters => tag.parameters,
                       :signature  => tag.signature,
                       :docstring  => doc_tag_to_hash(tag.docstring)
                      }
                    when YARD::Tags::OptionTag
                      {
                       :pair => doc_tag_to_hash(tag.pair)
                      }
                    when YARD::Tags::DefaultTag
                      {
                        :defaults => tag.defaults
                      }
                    else {}
                    end
    when YARD::Docstring
      {
        :text => tag,
        :tags => tag.tags.map { |t| doc_tag_to_hash(t) }
      }
    end
  end

  # Starts a REPL.
  command "repl-start" do |query|
    id = query["id"]

    @repls[id] = repl = REPL.new

    repl.input  = Input.new
    repl.output = Output.new

    repl.fiber = Fiber.new {
      Pry.start(TOPLEVEL_BINDING.eval(query["object"]),
                :input  => repl.input,
                :output => repl.output)
      repl.fiber = nil
    }

    {:succes => true}
  end

  # Processes a line of input.
  command "repl-handle" do |query|
    if repl = @repls[query["id"]]
      begin
        while request = repl.fiber.resume(query["argument"]) and
            request[0] != :read
          case request[0]
          when :write then
            write_result(:"repl-id" => query["id"],
                         :type      => "write",
                         :string    => request[1])
          end
        end


        write_result(:"repl-id" => query["id"],
                     :type      => "read",
                     :prompt    => request[1])
      rescue FiberError
      end

      {:success => true, :"repl-id" => query["id"]}
    else
      {:success => false, :error => "No such REPL: #{query["id"]}",
       :backtrace => [], :"repl-id" => query["id"]}
    end
  end

  # Kills a REPL.
  command "repl-stop" do |query|
    @repls.delete query["id"]
    {:success => true}
  end

  # Tries to autocomplete input in a REPL.
  command "repl-complete" do |query|
    if repl = @repls[query["id"]] and repl.fiber.alive?
      if proc = repl.input.proc
        {:success => true, :completions => proc.call(query["word"])}
      else
        {:success => true, :completions => []}
      end
    else
      {:success => false, :error => "No such REPL: #{query["id"]}",
       :backtrace => []}
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  if ARGV[0] == "--help" || ARGV[0] == "-h"
    puts "Usage: [RUBY_DEV_PORT=PORT RUBY_DEV_HOST=HOST] #$PROGRAM_NAME"\
         " [--server]"
  elsif ARGV[0] == "--server"
    RubyDev.run_server
  else
    RubyDev.run
  end
else
  Thread.new { RubyDev.run_server }
end
