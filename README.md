# gap-parser

This is a parser for the GAP programming language, written in
Haskell.

There are some examples of code that this can parse in the `test/`
directory.

## Why?

GAP's parser is, at the moment, a bit hard for me to read, since it's
written in C. Instead of even making a cursory effort to understand
it, I'm reimplementing it.

The goal of this project is:

* To make a working parser
* To make it easy to read

Notice that I explicitly don't care about performance. I want this to
be easy to read and understand. I haven't achieved either of these
goals yet.
