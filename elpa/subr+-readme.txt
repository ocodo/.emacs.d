   Extensions to standard library `subr.el'.

 This library extends `split-string' so that you can split a string
 based on text properties or a character predicate, not just just
 regexp matching.

 To take advantage of this, your code can conditionally test
 whether this library is loaded, or just test whether (fboundp
 'subr+-split-string).  That function is an alias for `split-string'.

 Buffer substring functions are also defined here, which return a
 buffer substring that includes or excludes characters that have a
 given text property.  In particular, `buffer-substring-of-visible'
 include only visible chars, and `buffer-substring-of-invisible'
 includes only invisible chars.


 Functions defined here:

   `buffer-substring-of-faced', `buffer-substring-of-invisible',
   `buffer-substring-of-propertied', `buffer-substring-of-unfaced',
   `buffer-substring-of-unpropertied',
   `buffer-substring-of-un/propertied-1',
   `buffer-substring-of-visible', `next-char-predicate-change',
   `split-string-by-predicate', `split-string-by-property',
   `split-string-by-regexp', `split-string-trim-omit-push',
   `subr+-split-string'.


 ***** NOTE: The following functions defined in `simple.el' have
             been REDEFINED HERE:

   `split-string' - Can also split by char property or predicate.
