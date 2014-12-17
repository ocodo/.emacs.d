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


    # Make sure we're in our own master branch
    git checkout master

    # Grab all the upstream changes
    git rebase upstream/master

If you've modified shared resources, you'll have conflicts to deal
with.

If you wish to maintain upstream update compatibility, only update
your `local-init.el`

### Breaking away

Any time you feel like breaking away, you can just start updating your
own packages and writing your own handy tools for day to day use.

My general update procedure is to jump onto a remote server, update
all packages, and push them to upstream.  Any extension work I do
which might be helpful for general use will be released to MELPA.

These steps will update and store all changes to git.

    M-x package-list-packages U x y

When prompted to delete old packages, `y`.

Exit Emacs, from `~/.emacs.d` :

    git add -A && git commit -m "Update Packages" && git push

### Where are you coming from?

Note that I'm restructuring this emacs config to be easy to use as a
default, in particular for users coming from SublimeText / TextMate.

Vim users, are recommended to try [**SpaceMacs**](https://github.com/syl20bnr/spacemacs)

Users of Eclipse, Visual Studio, XCode, IntelliJ (and other JetBrains
IDE's) are encouraged to use their current IDE as a build, debug
environment.  Emacs will be useful to you for major editing tasks,
org-mode, and as a curiosity.  Give it a bit of attention, but don't
try switching wholesale, until you've got over a few misconceptions.

Emacs is growing daily, and the in the last few years, this growth has
accelerated significantly.  Keep an eye on it, you will be rewarded by
it's power when you can wield it.

### Possible outliers...

The only place which might be worth looking at long term, is my
`/plugins` folder, there I have specifically hacked, and embryonic
packages, some of which have not been released for many years.  Some
may remain there, with small updates applied periodically, never
making it to MELPA.

### Custom

At the moment I use a version controlled custom file, I will make this user controlled.

My intended solution: Create a separate repo for use as `custom.el` (etc),
clone and symlink this custom repo to `~/.emacs.d/local/`.

Have `init.el` check the existence of `./local-custom/custom.el` and use
it instead of `./custom/custom.el`

Note `./custom/` has a set of `handy-functions.el` and a few key
binding files (mostly Mac OS X specific.)

### Sed and Moonscript modes

Two mode plugins in particular are `./plugins/sed-mode.el` and
`./plugins/moonscript-mode.el` these were not written by me, however I
seem to be one of the few places they can be found, if you're the
original author, I urge you to release them or ask around on emacs
mailing list or reddit/r/emacs for a maintainer.

# Color Theme Persistence

You color-theme selection is always stored outside this repository in
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
