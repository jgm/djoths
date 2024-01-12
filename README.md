# djoths

This is a Haskell library and command-line tool for parsing
the [djot] light markup language.

[djot]: https://djot.net

The parser reads a ByteString and builds an AST.  An HTML
renderer is included for testing.

For an example of its use, see `app/Main.hs`.

The parser makes use of András Kovács high-performance flatparse
library and has very good performance.  (For example, it is
around 5X faster than [commonmark-hs](https://github.com/jgm/commonmark-hs)
on comparable input with comparable extensions enabled.)

