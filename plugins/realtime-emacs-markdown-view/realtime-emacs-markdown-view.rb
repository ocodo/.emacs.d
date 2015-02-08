#!/usr/bin/env ruby
# -*- coding:utf-8 -*-

require 'sinatra'
require 'sinatra-websocket'
require 'redcarpet'

set :server, 'thin'
set :sockets, []
set :theme, "default"
set :bootswatch, {
      "default"   => "http://bootswatch.com/bower_components/bootstrap/dist/css/bootstrap.min.css",
      "cerulean"  => "http://bootswatch.com/cerulean/bootstrap.min.css",
      "cosmo"     => "http://bootswatch.com/cosmo/bootstrap.min.css",
      "cyborg"    => "http://bootswatch.com/cyborg/bootstrap.min.css",
      "darkly"    => "http://bootswatch.com/darkly/bootstrap.min.css",
      "flatly"    => "http://bootswatch.com/flatly/bootstrap.min.css",
      "journal"   => "http://bootswatch.com/journal/bootstrap.min.css",
      "lumen"     => "http://bootswatch.com/lumen/bootstrap.min.css",
      "paper"     => "http://bootswatch.com/paper/bootstrap.min.css",
      "readable"  => "http://bootswatch.com/readable/bootstrap.min.css",
      "sandstone" => "http://bootswatch.com/sandstone/bootstrap.min.css",
      "simplex"   => "http://bootswatch.com/simplex/bootstrap.min.css",
      "slate"     => "http://bootswatch.com/slate/bootstrap.min.css",
      "spacelab"  => "http://bootswatch.com/spacelab/bootstrap.min.css",
      "superhero" => "http://bootswatch.com/superhero/bootstrap.min.css",
      "united"    => "http://bootswatch.com/united/bootstrap.min.css",
      "yeti"      => "http://bootswatch.com/yeti/bootstrap.min.css"
    }

get '/' do
  erb :index
end

get '/settings' do
  settings.theme = params[:theme]
  puts settings.theme
end

get '/emacs' do
  request.websocket {|ws|
    ws.onopen { puts "@@ emacs connected" }
    ws.onmessage {|msg|
      renderer = Redcarpet::Render::HTML.new()
      extensions = {
        autolink: true,
        tables: true,
        superscript: true,
        strikethrough: true,
        no_intra_emphasis: true
      }
      puts settings.theme
      markdown = Redcarpet::Markdown.new(renderer, extensions)
      html = markdown.render msg
      EM.next_tick do
        settings.sockets.each{|s| s.send(html)}
      end
    }
    ws.onclose { settings.sockets.delete(ws) }
  }
end

get '/markdown' do
  request.websocket do |ws|
    ws.onopen do
      settings.sockets << ws
    end
    ws.onclose do
      warn("wetbsocket closed")
      settings.sockets.delete(ws)
    end
  end
end
