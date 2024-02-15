# djoths

This is a Haskell library and command-line tool for parsing
the [djot] light markup language.

[djot]: https://djot.net

The parser reads a ByteString and builds an AST.  An HTML
renderer and a Djot renderer are included.

For an example of its use, see `app/Main.hs`.

The parser has very good performance. For example, it is
8X faster than [commonmark-hs](https://github.com/jgm/commonmark-hs)
on comparable input with comparable extensions enabled, and more than
twice as fast as [djot.js](https://github.com/jgm/djot.js).
