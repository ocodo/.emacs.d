==============================================================
 Emacs Interface to `Ack <http://beyondgrep.com>`_-like Tools
==============================================================
 
This package brings the full power of `ack <http://beyondgrep.com>`_
to `emacs <http://www.gnu.org/software/emacs>`_ by allowing you to run
it seamlessly with its large set of options. Ack-like tools such as
`the silver searcher <https://github.com/ggreer/the_silver_searcher>`_
and ``git/hg/bzr grep`` are well supported too.

It is part of `GNU ELPA <http://elpa.gnu.org>`_ - the official package
archive for `emacs <http://www.gnu.org/software/emacs>`_. Patches,
feature requests and bug reports are welcome.

Features
--------

- Keep all features of `ack <http://beyondgrep.com>`_ and more
- Handle colors using the standard library ``ansi-color.el``
- Completion for ack options while reading from the minibuffer
- Support `the silver search (ag)
  <https://github.com/ggreer/the_silver_searcher>`_
- Support ``git grep``, ``hg grep`` and ``bzr grep``

Install
-------

``M-x package-install RET ack RET``

Screenshots
-----------

* ack

.. figure:: http://i.imgur.com/VwWyzAe.png
   :target: http://i.imgur.com/VwWyzAe.png
   :alt: ack.png

* git grep

.. figure:: http://i.imgur.com/rwjC4pa.png
   :target: http://i.imgur.com/rwjC4pa.png
   :alt: ack-git-grep.png

Usage
-----

- Type ``M-x ack`` and provide a pattern to search.
- Type ``C-u M-x ack`` to search from current project root.
- Type ``C-u C-u M-x ack`` to interactively choose a directory to search.

While reading ack command and args from the minibuffer, the following
key bindings may be useful:

- ``M-I`` => insert a template for case-insensitive file name search
- ``M-G`` => insert a template for ``git grep``, ``hg grep`` or ``bzr grep``
- ``M-Y`` => grab the symbol at point from the window before entering
  the minibuffer
- ``TAB`` => completion for ack options

Emacs23
-------

Check out the `emacs23
<https://github.com/leoliu/ack-el/tree/emacs23>`_ branch.

Bugs
----

https://github.com/leoliu/ack-el/issues

Contributors
------------
Phillip Lord
