{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE StrictData #-}
module Djot.Parse
(   Parser
  , parse
  , skip
  , asciiChar
  , satisfyAsciiChar
  , skipSatisfyAsciiChar
  , anyAsciiChar
  , satisfyChar
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

-- Returns final byte offset and result.
parse :: Parser s a -> s -> ByteString -> Maybe (Int, a)
parse parser ustate bs =
  first offset <$>
    runParser parser ParserState { subject = bs
                                 , offset = 0
                                 , line = 1
                                 , column = 0
                                 , userState = ustate }

-- Given a number of bytes, advances the offset and updates source positions.
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

peek :: ParserState s -> Maybe Char
peek st = subject st B8.!? offset st

peekBack :: ParserState s -> Maybe Char
peekBack st = subject st B8.!? (offset st - 1)

skip :: Int -> Parser s ()
skip n = Parser $ \st ->
  if offset st + n <= B8.length (subject st)
     then Just (advance n st, ())
     else Nothing

satisfyAsciiChar :: (Char -> Bool) -> Parser s Char
satisfyAsciiChar f = Parser $ \st ->
  case peek st of
    Just c | f c -> Just (advance 1 st, c)
    _ -> Nothing

skipSatisfyAsciiChar :: (Char -> Bool) -> Parser s ()
skipSatisfyAsciiChar f = Parser $ \st ->
  case peek st of
    Just c | f c -> Just (advance 1 st, ())
    _ -> Nothing

satisfyChar :: (Char -> Bool) -> Parser s Char
satisfyChar f = Parser $ \st ->
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

anyChar :: Parser s Char
anyChar = satisfyChar (const True)

asciiChar :: Char -> Parser s ()
asciiChar c = Parser $ \st ->
  case peek st of
    Just d | d == c -> Just (advance 1 st, ())
    _ -> Nothing

anyAsciiChar :: Parser s Char
anyAsciiChar = satisfyAsciiChar (const True)

skipMany :: Parser s a -> Parser s ()
skipMany parser = Parser go
 where
   go st = case runParser parser st of
             Nothing -> Just (st, ())
             Just (st',_) -> go st'

skipSome :: Parser s a -> Parser s ()
skipSome parser = parser *> skipMany parser

eof :: Parser s ()
eof = Parser $ \st ->
  if offset st >= B8.length (subject st)
     then Just (st, ())
     else Nothing

getState :: Parser s s
getState = Parser $ \st -> Just (st, userState st)

updateState :: (s -> s) -> Parser s ()
updateState f = Parser $ \st ->
  Just (st{ userState = f (userState st) }, ())

lookahead :: Parser s a -> Parser s a
lookahead pa = Parser $ \st ->
  case runParser pa st of
    Just (_, x) -> Just (st, x)
    Nothing -> Nothing

fails :: Parser s a -> Parser s ()
fails pa = Parser $ \st ->
  case runParser pa st of
    Just _ -> Nothing
    Nothing -> Just (st, ())

failed :: Parser s ()
failed = Parser $ const Nothing

withByteString :: Parser s a -> Parser s (a, ByteString)
withByteString pa = Parser $ \st ->
  case runParser pa st of
    Just (st', x) -> Just (st', (x, B8.take (offset st' - offset st)
                                    (B8.drop (offset st) (subject st))))
    Nothing -> Nothing

byteStringOf :: Parser s a -> Parser s ByteString
byteStringOf pa = Parser $ \st ->
  case runParser pa st of
    Just (st', _) -> Just (st', B8.take (offset st' - offset st)
                                    (B8.drop (offset st) (subject st)))
    Nothing -> Nothing

notFollowedBy :: Parser s a -> Parser s b -> Parser s a
notFollowedBy pa pb = pa <* fails pb

optional_ :: Parser s a -> Parser s ()
optional_ pa = void pa <|> pure ()

byteString :: ByteString -> Parser s ()
byteString bs = Parser $ \st ->
  if bs `B8.isPrefixOf` (subject st)
     then Just (advance (B8.length bs) st, ())
     else Nothing

manyAsciiWhile :: (Char -> Bool) -> Parser s ByteString
manyAsciiWhile f = Parser $ \st ->
  let bs = B8.takeWhile f (subject st)
  in  Just (advance (B8.length bs) st, bs)

skipAsciiWhile :: (Char -> Bool) -> Parser s ()
skipAsciiWhile f = Parser $ \st ->
  case B8.findIndex (not . f) (B8.drop (offset st) (subject st)) of
    Nothing -> Nothing
    Just i -> Just (advance i st, ())

someAsciiWhile :: (Char -> Bool) -> Parser s ByteString
someAsciiWhile f = Parser $ \st ->
  if fmap f (peek st) == Just True
     then
       let bs = B8.takeWhile f (subject st)
       in  Just (advance (B8.length bs) st, bs)
     else Nothing

takeRest :: Parser s ByteString
takeRest = Parser $ \st -> Just (st{ offset = B8.length (subject st) },
                                 B8.drop (offset st) (subject st) )

getOffset :: Parser s Int
getOffset = Parser $ \st -> Just (st, offset st)

sourceLine :: Parser s Int
sourceLine = Parser $ \st -> Just (st, line st)

sourceColumn :: Parser st Int
sourceColumn = Parser $ \st -> Just (st, column st)

branch :: Parser s a -> Parser s a -> Parser s a -> Parser s a
branch pa pb pc = Parser $ \st ->
  case runParser pa st of
    Just (st',_) -> runParser pb st'
    Nothing -> runParser pc st

endline :: Parser s ()
endline = branch (asciiChar '\r') (optional_ (asciiChar '\n')) (asciiChar '\n')

restOfLine :: Parser s ByteString
restOfLine = manyAsciiWhile (\c -> c /= '\n' && c /= '\r') <* endline

{-# INLINE isWs #-}
isWs :: Char -> Bool
isWs c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

{-# INLINE spaceOrTab #-}
spaceOrTab :: Parser s ()
spaceOrTab = skipAsciiWhile (\c -> c == ' ' || c == '\t')

ws :: Parser s ()
ws = skipAsciiWhile isWs
