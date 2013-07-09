You can jump to or read the documentation for the method, module (jump only),
`super` or constructor definition at point.

ElDoc support and constant and method completion are also provided.

Usage

(add-hook 'ruby-mode-hook 'robe-mode)

 - M-. to jump to the definition
 - M-, to jump back
 - C-c C-d to see the documentation
 - C-c C-k to refresh Rails environment
 - C-M-i to complete the symbol at point

Before using any of these commands, call `run-ruby' or `rinari-console'.
