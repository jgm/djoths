# Revision history for djot

## 0.1.2.3 -- 2025-09-27

* Fix swallowing of indentation in code under blockquote (#11).

## 0.1.2.2 -- 2024-10-04

* Allow list items with blank lines between divs (#10).

* Fix parsing of indented tables (#8).

## 0.1.2.1 -- 2024-06-24

* Djot writer: include separator line in table when the table has
  non-default alignments but no header (#7).

## 0.1.2 -- 2024-05-10

* Allow `_` in symbols (see jgm/djot#296).

* Add Lift derivations to AST datatypes (#5, Gideon Farrell) [API change].

## 0.1.1.3 -- 2024-03-17

* Ensure that tables end when we hit a blank line (#4).

* Fix parsing of table immediately after list (#4).

## 0.1.1.2 -- 2024-03-14

* Fix bug parsing regular paragraphs after list (#4).

## 0.1.1.1 -- 2024-03-03

* Revert "Djot.Blocks: use ByteString directly in `toIdentifier` (#1)"
  This caused problems for UTF-8 sequences that contained the
  byte 0xa0, which B8.words treats as a space character.

* AST: avoid using B8.words in normalizeLabel.

* Avoid using isSpace in attribute parsing. isSpace matches a byte 0x0a,
  which can break up a UTF-8 sequence. Limit to ASCII whitespace.

  * Add test with UTF-8 identifier. See jgm/pandoc#9541.

## 0.1.1.0 -- 2024-02-29

* Add Data instances to everything in the AST [API change].

* Ensure that block attributes are indented on subsequent lines (#2).

* Djot.Blocks: use ByteString directly in `toIdentifier` (#1,
  Vaibhav Sagar).

## 0.1.0.0 -- 2024-02-14

* Initial release.

