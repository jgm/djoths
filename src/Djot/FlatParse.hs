{-# LANGUAGE BinaryLiterals #-}
module Djot.FlatParse
( fusedSatisfy',
  satisfy',
  satisfyAscii',
  skipSatisfy',
  skipSatisfyAscii',
  char',
  asciiChar',
  ws,
  spaceOrTab,
  endline,
  isWs,
  afterWs,
  lastChar,
  getIndent,
  restOfLine,
  module FlatParse.Stateful
)
where

import FlatParse.Stateful hiding (Span)
import Data.Bits
import Control.Monad (guard)
import Data.Char (ord, chr)
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)

-- We use the Stateful parser, which gives us one Int of state
-- We use this Int as follows:
-- bits 0-7 - indentation (column)
-- bit 8 - 0 if we're tracking indentation, 1 if we've gotten past column 0xFF
-- bit 9 - 0 if last parsed character was whitespace (space, tab, cr, or lf)
-- bits 10-19 - used by inline parser to track open emphasis etc.
-- bits 20-27 - last parsed octet
-- bits 28+ - reserved to count line offset

fusedSatisfy' :: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool)
              -> (Char -> Bool) -> ParserT st r e Char
fusedSatisfy' f1 f2 f3 f4  =
  fusedSatisfy f1 f2 f3 f4 >>= \c -> c <$ updateState c

updateState :: Char -> ParserT st r e ()
updateState c = do
  let octet = case ord c of
                n | n >= 0xff -> 0xff
                  | otherwise -> fromIntegral $ ord c
  modify $ \i' ->
    let i = (i' .&. (0b0000000011111111111111111111)) .|. (octet `shiftL` 20)
    in case c of
      '\r' -> clearBit i 9
      '\n' -> 0b111111111111111111111111111111111111111111111111111110000000000 .&. i
              -- clear bits 0-9
      '\t' | testBit i 8 -> clearBit i 9
           | otherwise   -> clearBit (i + (4 - (i `mod` 4))) 9
      ' '  | testBit i 8 -> clearBit i 9
           | otherwise   -> clearBit (i + 1) 9
      _    | testBit i 8 -> setBit i 9
           | otherwise   -> setBit (i + 1) 9

{-# INLINE isWs #-}
isWs :: Char -> Bool
isWs c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

afterWs :: ParserT st r e Bool
afterWs = do
  i <- get
  pure $ not $ testBit i 9

-- Returns '\xff' if the last character parsed was multibyte.
lastChar :: ParserT st r e Char
lastChar = do
  i <- get
  pure $ chr $ 0b11111111 .&. (i `shiftR` 20)

getIndent :: ParserT st re e (Maybe Int)
getIndent = do
  i <- get
  pure $ if testBit i 8
            then Nothing
            else Just $ i .&. 0xFF

ws :: ParserT s r e ()
ws = skipSatisfyAscii' isWs

{-# INLINE spaceOrTab #-}
spaceOrTab :: ParserT s r e ()
spaceOrTab = skipSatisfyAscii' (\c -> c == ' ' || c == '\t')

{-# INLINE endline #-}
endline :: ParserT s r e ()
endline =
  branch (asciiChar' '\r') (optional_ (asciiChar' '\n')) (asciiChar' '\n')

{-# INLINE satisfy' #-}
satisfy' :: (Char -> Bool) -> ParserT st r e Char
satisfy' f = satisfy f >>= \c -> c <$ updateState c

{-# INLINE satisfyAscii' #-}
satisfyAscii' :: (Char -> Bool) -> ParserT st r e Char
satisfyAscii' f = satisfyAscii f >>= \c -> c <$ updateState c

{-# INLINE skipSatisfy' #-}
skipSatisfy' :: (Char -> Bool) -> ParserT st r e ()
skipSatisfy' f = satisfy f >>= updateState

{-# INLINE skipSatisfyAscii' #-}
skipSatisfyAscii' :: (Char -> Bool) -> ParserT st r e ()
skipSatisfyAscii' f = satisfyAscii f >>= updateState

{-# INLINE char' #-}
char' :: Char -> ParserT st r e ()
char' c = skipSatisfy' (== c)

{-# INLINE asciiChar' #-}
asciiChar' :: Char -> ParserT st r e ()
asciiChar' c = skipSatisfyAscii' (== c)

{-# INLINE restOfLine #-}
restOfLine :: ParserT st r e ByteString
restOfLine = do
  bs <- lookahead takeRest
  guard $ not $ B8.null bs
  case B8.findIndex (== '\n') bs of
    Nothing -> takeRest
    Just i -> do
      updateState '\n' -- reset indent, etc.
      FlatParse.Stateful.take (i + 1)
