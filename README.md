# Quick start guide...

Fork this repo, and then add it as upstream.

    git clone [your-fork]

    git remote add upstream https://github.com/ocodo/emacs.d

You can add packages from Elpa, and customisations to `local/init.el`,
these will run after `init.el` has finished.

Note: extended downstream customisation options are being added soon.

Another Note: your local color theme choice will not be overwritten by
updates from `upstream` - pick one with `M-x helm-themes`

### Updating from upstream

    git pull --all

Make sure we're in our own master branch

    git checkout master

Grab all the upstream changes

    git rebase upstream/master

If you've modified shared resources, you'll have conflicts to deal
with.

If you wish to maintain upstream complete update compatibility, only
update your `./local/..` (which is ignored by git, so you can make it
a separate git repository if you want to.)

I suggest you break away from upstream as soon as you are comfortable
installing your own packages.

### Local Config

This config is intended to be used to make Emacs a highly useful
editor, and give it some minimalist UI touches. (no Menu, no Toolbar,
etc.) It also includes a huge library of pre-installed extensions.

To add localised config, create and use the folder `./local/`

Add general config to `./local/init.el`

Customize will write to (and load from) `./local/custom.el` if you add
it before startup.

Copy `./custom/custom.el` to `./local/custom.el` if you want to start
from the same state as the general config.

# Package/Feature configuration

The biggest feature of this config is the use of Nginx / Apache Server
style split configuration.

Feature/Package configuration is set in `/_configs` and enabled by
symlinking to `_activated`.

Currently this is only supported for the centralized repo, so changes
are propagated unless you fork.

Using a git rebase / upstream style of updating would be ok, but you
will risk clobbering your own changes.

I am planning to add the ability to apply similar configuration via a local/init

# Local Color Theme Persistence

You color-theme selection is always stored locally, in
~/.emacs-theme - initially this will be non-existent or empty.

After first running Emacs with this config, simply run:

    M-x helm-themes

Now select a theme from the list.

Your selection will be saved, and untouched by upstream.

### Handy resources for getting more out of Emacs

* [Tuhdo's Emacs Mini Manual](http://tuhdo.github.io/emacs-tutor.html)

This deserves special mention. Tuhdo has done great work here, and
managed to cover many many modern extensions to Emacs.  Reading the
entire set of articles will give you a lot of valuable info.
Especially when it comes to making Emacs feel like a modern editor.

This config, has most of the extensions mentiond installed already,
and every week (or even more frequently) they're updated in this
`upstream`.

* [Mastering Emacs](http://www.masteringemacs.org/)
* [Emacs Rocks](http://emacsrocks.com/)
* [Emacs Wiki](http://www.emacswiki.org/)

And of course, the official emacs tour and manual...

* [Emacs Quick Ref](http://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf)
* [Emacs Tour](http://www.gnu.org/software/emacs/tour/)
* [Emacs Manual](http://www.gnu.org/software/emacs/manual/html_mono/emacs.html)
* [The Emacs manual as a PDF](http://www.gnu.org/software/emacs/manual/emacs.pdf)

### Invaluable reading

As you may know, Lisp is the backbone of Emacs, and although
emacs-lisp isn't quite up there with Common Lisp or Clojure, it's
still extremely powerful as a programming tool.

If you have a chance, read Paul Graham's OnLisp, it is the most
extraordinary book, when it comes to revealing the true power of Lisp
it's in a class of it's own.

It's available as a free PDF download from
http://www.paulgraham.com/onlisp.html

### Still don't know what Emacs actually is?

Emacs is often jokingly called an OS, really though, it's a Lisp
programming scratchpad, quite similar to the old Symbolics Lisp
Machines, Smalltalk, IPython, IJulia etc. albeit without much
attention paid to GUI features.

Just 30+ years focussed on editing features... thousands of them, just
take a look at `M-x package-list-packages`.

I've added scores of editing features tailored to my own tastes, I'm
sure you'll find it relatively easy to add your own once you get over
parenthesiophobia (the fear of Lisp.)

### iTerm2 OSX Semantic History

If you use Iterm2, you may be a little sad that there's no Emacs entry
in the `Open With Editor..` "Semantic History" option. (If you don't
know of it, you'll find it in Advanced preferences)

There's a few solutions dotted around in different gists and other
places around the web. Personally I didn't have too much luck with the
scripts I found, so I've added one to my config which works better for
me.

Set **Semantic History** to **Run coprocess...** and
paste in the following:

    ~/.emacs.d/bin/iterm-emacsclient +\2 \1

Then you're able to <kbd>Cmd</kbd> click on filenames in **iTerm2**
and it will open up an Emacs buffer (or start Emacs from
`/Applications`)

To open the file at line number.  <kbd>Cmd</kbd> clicking on a
filename printed in the terminal with a number suffix,
e.g. `README.md:130` would open **README.md** at line 130.

### git elpa

I've included a utility to assist with keeping my elpa packages committed individually when they're updated.

I keep versioned copies of elpa packages for several reasons.

- If a package suddenly disappears, I don't lose it.
- If gnu elpa or melpa are down for any reason, I have multiple locations from which I can clone a full install of my .emacs.d.
- If I have an issue with a package, it's very easy for me to rollback to a specific version.
    - Thanks to my new git elpa utility, it's now even easier. I can cherry pick a single version of a package, without external dependency on the package's own repo, etc.  I can see the history of the package's main elisp file locally too.
- If I wish to patch a package directly (obviously better to use defadvice, but that point aside) I can, however unlikely that may be.
- bisecting the config is MUCH easier so bug discovery and fixing is improved.

#### Git elpa usage

After doing a package install / upgrade from within Emacs, `git-elpa` (`~/.emacs.d/bin/git-elpa`) will be able to list and commit the newly added/upgraded packages.

- List all new / upgraded packages

        git elpa -l

- Commit a single package addition / upgrade

        git elpa -c PACKAGENAME

    `PACKAGENAME` would be the name of the package without version info or `.el` file extension etc.

    e.g.

        git elpa -c magit

    To perform the commit of the last magit version upgrade, add new files and remove old ones (from the git index)     and commit them with an auto-generated commit message

- Commit all new / upgraded packages

        git elpa -a

    All packages will be commited in individual git commits, using auto-generated commit messages.
