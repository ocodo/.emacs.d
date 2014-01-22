#!/usr/bin/env ruby
# -*- coding: utf-8 :mode ruby-mode -*-

print_methods = lambda do
  def method_missing(name)
    # do nothing if instance_methods method wasn't exist
    nil
  end

  begin
    load_modules = lambda do
      paths = []
      $LOAD_PATH.each do |path|
        paths << Dir.entries(path) if Dir.exist? path
      end
      paths.flatten.uniq.each do |path|
        # load modules without below regexp
        require path if /\.rb$/ =~ path && /debug|benchmark|profile/ !~ path
      end
      return true
    end

    pretty_print = lambda do |klass|
      print = lambda {|method| puts klass.to_s + "#" + method.to_s}
      klass.instance_methods(false).each {|method| print.call(method)}
      klass.singleton_methods(false).each {|method| print.call(method)}
      klass.public_instance_methods(false).each {|method| print.call(method)}
      klass.private_instance_methods(false).each {|method| print.call(method)}
    end

    print_modules = lambda do
      Module.constants.select do |c|
        klass = RbConfig.const_get(c)
        if klass.class == Class || klass.class == Module
          pretty_print.call(klass)
        end
      end
    end

    print_other_modules = lambda do
      `ri -l --no-pager`.split.each do |klass|
        pretty_print.call(klass)
      end
    end

    print_modules.call
    # comment out, below code was just slowed to display
    # TODO: save somewhere
    # if load_modules.call
    #   print_modules.call
    #   print_other_modules.call
    # end
  rescue Exception => error
    puts error
  end
end

print_methods.call
