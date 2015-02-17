#!/usr/bin/env ruby
# -*- coding:utf-8 -*-

require 'sinatra'
require 'sinatra-websocket'
require 'redcarpet'

set :server, 'thin'
set :sockets, []

get '/' do
  erb :index
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
        footnotes: true,
        strikethrough: true,
        fenced_code_blocks: true,
        no_intra_emphasis: true
      }
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
      warn "wetbsocket closed"
      settings.sockets.delete(ws)
    end
  end
end
