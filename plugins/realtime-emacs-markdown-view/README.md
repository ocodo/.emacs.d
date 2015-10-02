# Realtime Emacs Markdown View

This is an Emacs extension for realtime editing and previewing of
Markdown as live HTML.

This is began as a fork of Syohex's Realtime Markdown Viewer.

### Major changes

- Only uses Ruby [SinatraRB](http://www.sinatrarb.com) for the websocket/http server component
- Markdown parsing Github flavoured markdown
- CSS is based on Github markdown

#### Dependencies

* Emacs 24
* Ruby - http://ruby-lang.org/
* Bundler - http://bundler.io/

#### Installation

Copy these files into a folder within your `.emacs.d` and add it to the `load-path`

Then add it to the load-path, ie. add the following line to `~/.emacs.d/init.el` (or `~/.emacs`):

    (add-path (concat user-emacs-directory "/plugins/realtime-emacs-markdown-view/" ))

In a shell, run the following line:

    bundle install

From the `~/.emacs.d/plugins/realtime-emacs-markdown-view` folder

### Usage

When you want to turn previewing on in a `markdown-mode` buffer do the
following:

From emacs:

    M-x realtime-emacs-markdown-view-mode

Then open in a web browser (with Javascript enabled):

    http://localhost:5021/

Any activity in the Emacs markdown buffer will trigger an update in
the browser page.

### RVM

When using RVM, you'll need to install the `rvm.el` package (find it on MELPA)

Once Emacs starts, and `rvm.el` is installed, do:

    M-x rvm-use-default

Realtime emacs markdown view mode will find the correct `ruby` to
use. (assuming that you did `bundle install` with the default `rvm`
ruby, if not, use the `M-x rvm-use` command to select the correct
ruby/gemset combination first.)

For more on RVM visit https://rvm.io
