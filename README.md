weshmek-skanner
===============

A regular-expression-tokenizer-generator written in racket

This project was started from my experience in CS 444. We struggled to write a decent scanner in Racket. Our scanner used the theoretical approach of converting the regular expressions into NFAs and the NFAs into DFAs. It was ugly (oh so ugly) and slow (oh so slow). Here's my attempt to correct our mistakes, using more of Racket's core strength, rather than attempting to use Racket as something it's not. 

This was also an attempt to learn more about macros in Racket, though I soon discovered that macros actually make the program slower. The non-macro version is much, much faster.

Feel free to test and extend.
