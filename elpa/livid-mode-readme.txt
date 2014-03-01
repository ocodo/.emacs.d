This mode tries to create an environment similar to some online code editors
(such as jsbin or codepen), where JavaScript code in a buffer is evaluated
automatically every time it changes. However, in contrast to those tools,
the browser's state is not refreshed. The code is just evaluated, as if via the
browser's console.

By default, a buffer's code is first checked for syntax errors (using the
external `js` command) before actually being sent for evaluation.

Depends on skewer-mode and s. If livid-mode is activated in a buffer which is
not in skewer-mode already, skewer-mode is called.

As an alternative to turning the mode on and off, livid can be "paused"
(postponing any eval) with livid-pause, bound by default to C-c C-p.
