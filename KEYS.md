# ocodo keys

Just a few key bindings, you'll find them in keys.el 

    Mac name    |  *Emacs name : Command/Description 
    -------------------------------------------------------------
    Cmd ]       |  s-]         : next buffer
    Cmd [       |  s-[         : previous buffer
    Cmd right   |  s-right     : next buffer
    Cmd left    |  s-left      : previous buffer
    Cmd b       |  s-b         : switch to buffer
    -------------------------------------------------------------
    Cmd -       |  s--         : text scale decrease
    Cmd =       |  s-=         : text scale increase
    -------------------------------------------------------------
    Cmd o       |  s-o         : find file
    -------------------------------------------------------------
    Cmd 0       |  s-0         : toggle line number mode
    -------------------------------------------------------------
    Cmd 1       |  s-1         : delete other windows
    Cmd 2       |  s-2         : split window horizontally
    Cmd 3       |  s-3         : split window vertically
    Cmd 4       |  s-4         : delete other windows vertically
    Cmd 5       |  s-5         : delete window
    Ctrl Opt ,  |  C-M-,       : shrink window horizontally
    Ctrl Opt .  |  C-M-.       : enlarge window horizontally
    Cmd `       |  s-`         : other window
    Cmd ~       |  s-~         : other frame
    -------------------------------------------------------------
    Cmd /       |  s-/         : hippie expand
    Ctrl TAB    |  C-TAB       : completion at point
    Cmd Return  |  s-return    : completion at point

> Emacs knows keys from Lisp machines, Hyper, Super, Meta (and some others) Keybindings in Emacs are shown like this... `C-x` meaning **Ctrl+x**. You will also see a lot of two stage keyboard shortcuts, such as `C-x C-c`, these are known as *chords*, however, unlike a mucical chord the keys are not hit at the same time, instead you perform the key combinations in sequence. `C-x C-f` is like saying `Ctrl+x then Ctrl+f` 

> If you start a chord and you need to quit halfway, you can press `C-g` *quit operation*, be aware that you might need to do it twice. (Modes shouldn't use C-g as part of a chord, that would be a **bad** thing to do.)

> On the Mac, within Emacs, your **Opt** (aka. **Alt**) key is the **Meta** key, the **Cmd** key is the **Super** key.

> So in the list above, `C-` is **Ctrl+**, `s-` is **Cmd** or what Emacs calls **Super**, and `M-` is **Alt+** or **Opt+** what Emacs calls **Meta**.

> It's worth noting Emacs also uses the **Esc** key as **Meta**, so you can press **Esc** once, and Emacs will act as if you're holding down **Opt/Alt**. This is a bit confusing to many, but it's a nice alternative to finger stretching, RSI inducing keyboard gymnastics.

> So `Esc, x` is the same as `M-x` for example.
