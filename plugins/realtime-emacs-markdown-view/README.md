# Realtime Emacs Markdown View

Emacs module for realtime editing/previewing of Markdown as HTML (also
using bootstrap css.)

Note: This is a fork of [Syohei Yoshida's](https://github.com/syohex)
Realtime Markdown Viewer.

Syohex's version allows the user to pick between a Perl or Ruby
based http/websocket server. In an effort to simplify, this fork is
using only Ruby/Sinatra.

There are also no static web files (bootstrap and jquery.) They are
served by CDN.

## Requirements

* Emacs 24.
* Latest [websocket.el](https://github.com/ahyatt/emacs-websocket)
    - websocket.el older than 2012/SEP/01 does not support multibyte characters
* [SinatraRB](http://www.sinatrarb.com/)

#### Installation

Currently this module is only available from this repo on Github, I
don't intend to release via MELPA.

Clone the repo into a folder within your `.emacs.d` and add it to the `load-path`

For example:

    cd ~/.emacs.d/plugins/
    git clone https://github.com/jasonm23/realtime-emacs-markdown-view

Then add it to the load-path, ie. add the following line to `~/.emacs.d/init.el` (or `~/.emacs`):

    (add-path (concat user-emacs-directory "/plugins/realtime-emacs-markdown-view/" ))

In a shell, run the following line:

    bundle install

(from the `~/.emacs.d/plugins/realtime-emacs-markdown-view` folder)

### Usage

When you want to turn previewing on in an `markdown-mode` buffer do
the following:

From emacs:

    M-x realtime-emacs-markdown-view-mode

Which should also open in the default browser:

    http://localhost:5021/

Any activity in the Emacs markdown buffer will trigger an update in
the browser page.

### Troubleshooting

Sinatra uses a gem called `eventmachine` which uses native extensions,
this can prove a bit troublesome when run from Emacs, (note: only if
you have multiple versions of ruby installed.)

Open an issue in this repo if you have problems.
