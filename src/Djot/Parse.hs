{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
module Djot.Parse
(   Parser
  , parse
  , skipBytes
  , asciiChar
  , satisfyByte
  , skipSatisfyByte
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
  , takeRest
  , getOffset
  , sourceLine
  , sourceColumn
  , branch
  , endline
  , restOfLine
  , ws
  , followedByWhitespace
  , followedByBlankLine
  , spaceOrTab
  , isWs
  , strToUtf8
  , utf8ToStr
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
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text as T
-- import Text.Printf
-- import Debug.Trace

newtype Parser s a =
  Parser{ runParser :: ParserState s -> Maybe (ParserState s, a) }

instance Functor (Parser s) where
  fmap f g = Parser $ \s -> case runParser g s of
                                 Nothing -> Nothing
                                 Just (s', !x) -> Just (s', f x)

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
  { subject :: !ByteString
  , offset :: !Int
  , line :: !Int
  , column :: !Int
  , userState :: !a
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
unsafeAdvance :: Int -> ParserState s -> ParserState s
unsafeAdvance 0 = id
unsafeAdvance !n = unsafeAdvance (n - 1) . unsafeAdvanceByte

-- | Advance the offset and line/column for consuming a given byte.
unsafeAdvanceByte :: ParserState s -> ParserState s
unsafeAdvanceByte st =
  case B.index (subject st) (offset st) of
    10 -> st{ offset = offset st + 1
            , line = line st + 1
            , column = 0 }
    9 -> st{ offset = offset st + 1
           , column = column st + (4 - (column st `mod` 4)) }
    !w | w < 0x80 -> st{ offset = offset st + 1
                       , column = column st + 1 }
       -- utf8 multibyte: only count byte 1:
       | w >= 0b11000000 -> st{ offset = offset st + 1
                              , column = column st + 1 }
       | otherwise -> st{ offset = offset st + 1 }

-- | Returns current byte as Char.
current :: ParserState s -> Maybe Char
current st = subject st B8.!? offset st

-- | Returns current byte as Char.
peek :: Parser s (Maybe Char)
peek = Parser $ \st -> Just (st, current st)

-- | Returns previous byte as Char.
peekBack :: Parser s (Maybe Char)
peekBack = Parser $ \st -> Just (st, subject st B8.!? (offset st - 1))

-- | Skip n bytes.
skipBytes :: Int -> Parser s ()
skipBytes !n = Parser $ \st ->
  if offset st + n <= B8.length (subject st)
     then Just (unsafeAdvance n st, ())
     else Nothing

-- | Parse a byte satisfying a predicate.
satisfyByte :: (Char -> Bool) -> Parser s Char
satisfyByte f = Parser $ \st ->
  case current st of
    Just c | f c -> Just (unsafeAdvanceByte st, c)
    _ -> Nothing

-- | Skip byte satisfying a predicate.
skipSatisfyByte :: (Char -> Bool) -> Parser s ()
skipSatisfyByte f = Parser $ \st ->
  case current st of
    Just c | f c -> Just (unsafeAdvanceByte st, ())
    _ -> Nothing

-- | Parse a (possibly multibyte) Char satisfying a predicate.
-- Assumes UTF-8 encoding.
satisfy :: (Char -> Bool) -> Parser s Char
satisfy f = Parser $ \st ->
  let peekWord !n = subject st B.!? (offset st + n)
      b2 = fromMaybe 0 $ peekWord 1
      b3 = fromMaybe 0 $ peekWord 2
      b4 = fromMaybe 0 $ peekWord 3
  in case peekWord 0 of
    Nothing -> Nothing
    Just b1
      | b1 < 0b10000000
      , !c <- chr (fromIntegral b1)
      , f c -> Just (unsafeAdvanceByte st, c)
      | b1 .&. 0b11100000 == 0b11000000
      , b2 >= 0b10000000
      , !c <- chr (toCodePoint2 b1 b2)
      , f c -> Just (unsafeAdvance 2 st, c)
      | b1 .&. 0b11110000 == 0b11100000
      , b2 >= 0b10000000
      , b3 >= 0b10000000
      , !c <- chr (toCodePoint3 b1 b2 b3)
      , f c -> Just (unsafeAdvance 3 st, c)
      | b1 .&. 0b11111000 == 0b11110000
      , b2 >= 0b10000000
      , b3 >= 0b10000000
      , b4 >= 0b10000000
      , !c <- chr (toCodePoint4 b1 b2 b3 b4)
      , f c -> Just (unsafeAdvance 4 st, c)
      | otherwise -> Nothing
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

-- | Parse any character. Assumes UTF-8 encoding.
anyChar :: Parser s Char
anyChar = satisfy (const True)

-- | Parse an ASCII character.
asciiChar :: Char -> Parser s ()
asciiChar !c = Parser $ \st ->
  case current st of
    Just d | d == c -> Just (unsafeAdvanceByte st, ())
    _ -> Nothing

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
  case current st of
    Nothing -> Just (st, ())
    Just _ -> Nothing

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
failed :: Parser s a
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
  if bs `B8.isPrefixOf` B8.drop (offset st) (subject st)
     then Just (unsafeAdvance (B.length bs) st, ())
     else Nothing

-- | Returns rest of input and moves to eof.
takeRest :: Parser s ByteString
takeRest = Parser $ \st ->
  Just (unsafeAdvance (B8.length (subject st) - offset st) st,
         B8.drop (offset st) (subject st))

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

-- | Return the rest of line (including the end of line).
restOfLine :: Parser s ByteString
restOfLine =
  byteStringOf $
    skipMany (skipSatisfyByte (\c -> c /= '\n' && c /= '\r'))
      <* optional_ endline

{-# INLINE isWs #-}
-- | Is space, tab, `\r`, or `\n`.
isWs :: Char -> Bool
isWs c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

-- | Skip one space or tab.
spaceOrTab :: Parser s ()
spaceOrTab = Parser $ \st ->
  case current st of
    Just ' ' -> Just (unsafeAdvanceByte st, ())
    Just '\t' -> Just (unsafeAdvanceByte st, ())
    _ -> Nothing

-- | Skip 1 or more ASCII whitespace.
ws :: Parser s ()
ws = skipSome (satisfyByte isWs)

-- | Next character is ASCII whitespace.
followedByWhitespace :: Parser s ()
followedByWhitespace = Parser $ \st ->
  case current st of
    Just c | isWs c -> Just (st, ())
    _ -> Nothing

-- | Followed by 0 or more spaces/tabs and endline or eof.
followedByBlankLine :: Parser s ()
followedByBlankLine = Parser $ \st ->
  let subj = subject st
      !len = B8.length subj
      go !off
        | off >= len
          = Just (st, ())
        | otherwise
          = case B8.index subj off of
              ' ' -> go (off + 1)
              '\r' -> go (off + 1)
              '\t' -> go (off + 1)
              '\n' -> Just (st, ())
              _ -> Nothing
  in go (offset st)

strToUtf8 :: String -> ByteString
strToUtf8 = encodeUtf8 . T.pack

utf8ToStr :: ByteString -> String
utf8ToStr = T.unpack . decodeUtf8With lenientDecode
