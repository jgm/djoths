{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE StrictData #-}
module Djot.Parse
(   Parser
  , parse
  , skip
  , asciiChar
  , satisfyAscii
  , skipSatisfyAscii
  , anyAsciiChar
  , satisfy
  , anyChar
  , skipMany
  , skipSome
  , eof
  , getState
  , updateState
  , lookahead
  , peek
  , peekBack
  , fails
  , failed
  , withByteString
  , byteStringOf
  , notFollowedBy
  , optional_
  , byteString
  , someAsciiWhile
  , manyAsciiWhile
  , skipAsciiWhile
  , takeRest
  , getOffset
  , sourceLine
  , sourceColumn
  , branch
  , endline
  , restOfLine
  , isWs
  , ws
  , spaceOrTab
)
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import Control.Applicative
import Control.Monad (void, MonadPlus(..))
import Data.Bifunctor (first)
import Data.Char (chr)
import Data.Bits
import Data.Maybe (fromMaybe)
-- import Text.Printf
-- import Debug.Trace

newtype Parser s a =
  Parser{ runParser :: ParserState s -> Maybe (ParserState s, a) }

instance Functor (Parser s) where
  fmap f g = Parser $ \s -> case runParser g s of
                                 Nothing -> Nothing
                                 Just (s', x) -> Just (s', f x)

instance Applicative (Parser s) where
  pure x = Parser (\s -> Just (s, x))
  liftA2 f g h = Parser $ \s ->
    case runParser g s of
      Nothing -> Nothing
      Just (s', x) ->
        case runParser h s' of
          Nothing -> Nothing
          Just (s'', y) -> Just (s'', f x y)

instance Monad (Parser s) where
  return = pure
  f >>= g = Parser $ \s ->
    case runParser f s of
      Nothing -> Nothing
      Just (s', x) -> runParser (g x) s'

instance Alternative (Parser s) where
  empty = Parser (const Nothing)
  f <|> g = Parser $ \s ->
    case runParser f s of
      Just (s', x) -> Just (s', x)
      Nothing -> runParser g s

instance MonadPlus (Parser s) where
  mzero = empty
  mplus = (<|>)

data ParserState a =
  ParserState
  { subject :: ByteString
  , offset :: Int
  , line :: Int
  , column :: Int
  , userState :: a
  }
  deriving (Show)

-- | Apply a parser to a bytestring with a given user state.
-- Returns @Nothing@ on failure, @Just (byteOffset, result)@
-- on success.
parse :: Parser s a -> s -> ByteString -> Maybe (Int, a)
parse parser ustate bs =
  first offset <$>
    runParser parser ParserState { subject = bs
                                 , offset = 0
                                 , line = 1
                                 , column = 0
                                 , userState = ustate }

-- | Given a number of bytes, advances the offset and updates line/column.
advance :: Int -> ParserState s -> ParserState s
advance n st' =
  B.foldl' go st' bs
 where
   bs = B8.take n (B8.drop (offset st') (subject st'))
   -- newline
   go st 10 = st{ offset = offset st + 1
                , line = line st + 1
                , column = 0 }
   -- tab
   go st 9 = st{ offset = offset st + 1
               , column = column st + (4 - (column st `mod` 4)) }
   go st w
     | w < 0x80 = st{ offset = offset st + 1
                    , column = column st + 1 }
     -- utf8 multibyte: only count byte 1:
     | w >= 0b11000000 = st{ offset = offset st + 1
                           , column = column st + 1 }
     | otherwise = st{ offset = offset st + 1 }

-- | Returns current byte as Char.
peek :: ParserState s -> Maybe Char
peek st = subject st B8.!? offset st

-- | Returns previous byte as Char.
peekBack :: ParserState s -> Maybe Char
peekBack st = subject st B8.!? (offset st - 1)

-- | Skip $n$ bytes.
skip :: Int -> Parser s ()
skip n = Parser $ \st ->
  if offset st + n <= B8.length (subject st)
     then Just (advance n st, ())
     else Nothing

-- | Parse an ASCII Char satisfying a predicate.
satisfyAscii :: (Char -> Bool) -> Parser s Char
satisfyAscii f = Parser $ \st ->
  case peek st of
    Just c | f c -> Just (advance 1 st, c)
    _ -> Nothing

-- | Skip an ASCII Char satisfying a predicate.
skipSatisfyAscii :: (Char -> Bool) -> Parser s ()
skipSatisfyAscii f = Parser $ \st ->
  case peek st of
    Just c | f c -> Just (advance 1 st, ())
    _ -> Nothing

-- | Parse a (possibly multibyte) Char satisfying a predicate.
-- Assumes UTF-8 encoding.
satisfy :: (Char -> Bool) -> Parser s Char
satisfy f = Parser $ \st ->
  let b1 = fromMaybe 0 $ peekWord 0 st
      b2 = fromMaybe 0 $ peekWord 1 st
      b3 = fromMaybe 0 $ peekWord 2 st
      b4 = fromMaybe 0 $ peekWord 3 st
  in case peekWord 0 st of
    Nothing -> Nothing
    Just w
      | w < 0b10000000
      , c <- chr (fromIntegral w)
      , f c -> Just (advance 1 st, chr (fromIntegral w))
      | b1 .&. 0b11100000 == 0b11000000
      , b2 >= 0b10000000 -> Just (advance 2 st, chr (toCodePoint2 b1 b2))
      | b1 .&. 0b11110000 == 0b11100000
      , b2 >= 0b10000000
      , b3 >= 0b10000000 -> Just (advance 3 st, chr (toCodePoint3 b1 b2 b3))
      | b1 .&. 0b11111000 == 0b11110000
      , b2 >= 0b10000000
      , b3 >= 0b10000000
      , b4 >= 0b10000000 -> Just (advance 4 st, chr (toCodePoint4 b1 b2 b3 b4))
    _ -> Nothing
 where
  toCodePoint2 a b =
    (fromIntegral (a .&. 0b00011111) `shiftL` 6) +
     fromIntegral (b .&. 0b00111111)
  toCodePoint3 a b c =
    (fromIntegral (a .&. 0b00001111) `shiftL` 12) +
    (fromIntegral (b .&. 0b00111111) `shiftL` 6) +
     fromIntegral (c .&. 0b00111111)
  toCodePoint4 a b c d =
    (fromIntegral (a .&. 0b00000111) `shiftL` 18) +
    (fromIntegral (b .&. 0b00111111) `shiftL` 12) +
    (fromIntegral (c .&. 0b00111111) `shiftL` 6) +
     fromIntegral (d .&. 0b00111111)
  peekWord n st' = subject st' B.!? (offset st' + n)

-- | Parse any character. Assumes UTF-8 encoding.
anyChar :: Parser s Char
anyChar = satisfy (const True)

-- | Parse an ASCII character.
asciiChar :: Char -> Parser s ()
asciiChar c = Parser $ \st ->
  case peek st of
    Just d | d == c -> Just (advance 1 st, ())
    _ -> Nothing

-- | Parse any ASCII character.
anyAsciiChar :: Parser s Char
anyAsciiChar = satisfyAscii (const True)

-- | Apply parser 0 or more times, discarding result.
skipMany :: Parser s a -> Parser s ()
skipMany parser = Parser go
 where
   go st = case runParser parser st of
             Nothing -> Just (st, ())
             Just (st',_) -> go st'

-- | Apply parser 1 or more times, discarding result.
skipSome :: Parser s a -> Parser s ()
skipSome parser = parser *> skipMany parser

-- | Succeeds if no more input.
eof :: Parser s ()
eof = Parser $ \st ->
  if offset st >= B8.length (subject st)
     then Just (st, ())
     else Nothing

-- | Returns current user state.
getState :: Parser s s
getState = Parser $ \st -> Just (st, userState st)

-- | Updates user state.
updateState :: (s -> s) -> Parser s ()
updateState f = Parser $ \st ->
  Just (st{ userState = f (userState st) }, ())

-- | Apply a parser, returning its result but not changing state
-- or advancing.
lookahead :: Parser s a -> Parser s a
lookahead pa = Parser $ \st ->
  case runParser pa st of
    Just (_, x) -> Just (st, x)
    Nothing -> Nothing

-- | Succeeds if parser fails.
fails :: Parser s a -> Parser s ()
fails pa = Parser $ \st ->
  case runParser pa st of
    Just _ -> Nothing
    Nothing -> Just (st, ())

-- | Always fails.
failed :: Parser s ()
failed = Parser $ const Nothing

-- | Returns result of parse together with the bytestring
-- consumed.
withByteString :: Parser s a -> Parser s (a, ByteString)
withByteString pa = Parser $ \st ->
  case runParser pa st of
    Just (st', x) -> Just (st', (x, B8.take (offset st' - offset st)
                                    (B8.drop (offset st) (subject st))))
    Nothing -> Nothing

-- | Returns bytestring consumed by parse.
byteStringOf :: Parser s a -> Parser s ByteString
byteStringOf pa = Parser $ \st ->
  case runParser pa st of
    Just (st', _) -> Just (st', B8.take (offset st' - offset st)
                                    (B8.drop (offset st) (subject st)))
    Nothing -> Nothing

-- | Succeeds if first parser succeeds and second fails, returning
-- first parser's value.
notFollowedBy :: Parser s a -> Parser s b -> Parser s a
notFollowedBy pa pb = pa <* fails pb

-- | Apply parser but still succeed if it doesn't succeed.
optional_ :: Parser s a -> Parser s ()
optional_ pa = void pa <|> pure ()

-- | Parse a bytestring.
byteString :: ByteString -> Parser s ()
byteString bs = Parser $ \st ->
  if bs `B8.isPrefixOf` (subject st)
     then Just (advance (B8.length bs) st, ())
     else Nothing

-- | Consume 0 or more ASCII characters matching a predicate,
-- returning the bytestring consumed.
manyAsciiWhile :: (Char -> Bool) -> Parser s ByteString
manyAsciiWhile f = Parser $ \st ->
  let bs = B8.takeWhile f (subject st)
  in  Just (advance (B8.length bs) st, bs)

-- | Skip 0 or more ASCII characters matching a predicate.
skipAsciiWhile :: (Char -> Bool) -> Parser s ()
skipAsciiWhile f = Parser $ \st ->
  case B8.findIndex (not . f) (B8.drop (offset st) (subject st)) of
    Nothing -> Nothing
    Just i -> Just (advance i st, ())

-- | Consume 1 or more ASCII characters matching a predicate,
-- returning the bytestring consumed.
someAsciiWhile :: (Char -> Bool) -> Parser s ByteString
someAsciiWhile f = Parser $ \st ->
  if fmap f (peek st) == Just True
     then
       let bs = B8.takeWhile f (subject st)
       in  Just (advance (B8.length bs) st, bs)
     else Nothing

-- | Returns rest of input and moves to eof.
takeRest :: Parser s ByteString
takeRest = Parser $ \st -> Just (st{ offset = B8.length (subject st) },
                                 B8.drop (offset st) (subject st) )

-- | Returns byte offset in input.
getOffset :: Parser s Int
getOffset = Parser $ \st -> Just (st, offset st)

-- | Returns the line number.
sourceLine :: Parser s Int
sourceLine = Parser $ \st -> Just (st, line st)

-- | Returns the source column number. (Tab stop is computed at 4.)
sourceColumn :: Parser st Int
sourceColumn = Parser $ \st -> Just (st, column st)

-- | Try the first parser: if it succeeds, apply the second,
-- returning its result, otherwise the third.
branch :: Parser s b -> Parser s a -> Parser s a -> Parser s a
branch pa pb pc = Parser $ \st ->
  case runParser pa st of
    Just (st',_) -> runParser pb st'
    Nothing -> runParser pc st

-- | Parse an end of line sequence.
endline :: Parser s ()
endline = branch (asciiChar '\r') (optional_ (asciiChar '\n')) (asciiChar '\n')

-- | Return the rest of line (not including the end of line, though it is
-- consumed).
restOfLine :: Parser s ByteString
restOfLine = manyAsciiWhile (\c -> c /= '\n' && c /= '\r') <* endline

{-# INLINE isWs #-}
-- | Is space, tab, `\r`, or `\n`.
isWs :: Char -> Bool
isWs c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

{-# INLINE spaceOrTab #-}
-- | Skip one space or tab.
spaceOrTab :: Parser s ()
spaceOrTab = skipSatisfyAscii (\c -> c == ' ' || c == '\t')

-- | Skip ASCII whitespace.
ws :: Parser s ()
ws = skipAsciiWhile isWs
