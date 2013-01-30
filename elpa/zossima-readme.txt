Jump to definition, driven by a live Ruby subprocess.

Explain to me again why doesn't this exist yet?
If you tell me "tags files" I'm going to kick you.

Install

M-x package-install zossima (j/k not yet)

Usage

(add-hook 'ruby-mode-hook 'zossima-mode)

 - M-. to jump to a definition
 - M-, to jump back

Before using `zossima-jump', call `run-ruby' or `rinari-console'.
