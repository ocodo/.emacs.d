# Realtime Markdown Viewer

Emacs module for realtime editing/browser previewing of Markdown.

This is a simplified fork of
[Syohei Yoshida's](https://github.com/syohex) Realtime Markdown Viewer.

NOTE: Syohex's version allows the user to pick between a Perl or Ruby
based http/websocket server. In an effort to simplify this fork is
using only Ruby/Sinatra.

## Requirements

* Emacs 24.
* Latest [websocket.el](https://github.com/ahyatt/emacs-websocket)
    - websocket.el older than 2012/SEP/01 does not support multibyte characters
* [SinatraRB](http://www.sinatrarb.com/)

## Demonstration

Basically this is the same as Syohei's code (I have just stripped out the Perl option).

Here's the existing [YouTube demo](http://www.youtube.com/watch?feature=player_embedded&v=qnoMo0ynyZo)


#### Installation

Currently this module is only available from this repo on Github, I
don't intend to release via MELPA.

Clone the repo into a folder within your `.emacs.d` and add it to the `load-path`

For example:

    cd ~/.emacs.d/plugins/
    git clone https://github.com/jasonm23/emacs-realtime-markdown-viewer

Then add it to the load-path, ie. add the following line to `~/.emacs.d/init.el` (or `~/.emacs`):

    (add-path (concat user-emacs-directory "/plugins/emacs-realtime-markdown-viewer/" ))

In a shell, run the following line:

    bundle install

(from the `~/.emacs.d/plugins/emacs-realtime-markdown-viewer` folder)

### Usage

When you want to turn previewing on in an `markdown-mode` buffer do the following:

From emacs:

    M-x realtime-markdown-viewer-mode

From the browser:

    open http://0.0.0.0:5021/

The next time you make a change to the Emacs buffer, the browser will
render it as HTML.

### Troubleshooting

Sinatra uses a gem called `eventmachine` which uses native extensions,
this can prove a bit troublesome when run from Emacs, (note: only if you
have multiple versions of ruby installed.)

Open an issue in this repo if you have problems.
