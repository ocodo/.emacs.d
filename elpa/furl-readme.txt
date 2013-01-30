furl.el is a friendlier layer on top of url.el. It makes the most common use
cases (sending parameters and receiving a string response) easier, at the
expense of making less common use cases (non-default error handling, reading
response headers) somewhat more complex.

One simplification furl.el makes is not using cbargs parameters for
asynchronous functions. These are omitted because `lexical-let' from cl.el
can accomplish the same purpose when necessary.
