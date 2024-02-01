{-# LANGUAGE StrictData #-}
module Djot.Parse
(   Parser
  , parse
  , skip
  , asciiChar
  , satisfyAsciiChar
  , anyAsciiChar
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
  , takeRest
  , getOffset
  , branch
  , endline
  , takeLine
)
where

import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import Control.Applicative
import Control.Monad (void)
import Data.Bifunctor (first)

-- TODO track indentation, taking into account tabs?

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

data ParserState a =
  ParserState
  { subject :: ByteString
  , offset :: Int
  , userState :: a }
  deriving (Show)

-- Returns final byte offset and result.
parse :: Parser s a -> s -> ByteString -> Maybe (Int, a)
parse parser ustate bs =
  first offset <$>
    runParser parser ParserState { subject = bs
                                 , offset = 0
                                 , userState = ustate }

advance :: Int -> ParserState s -> ParserState s
advance n st =
  st{ offset = offset st + n } -- TODO add indentation tracking

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

branch :: Parser s a -> Parser s a -> Parser s a -> Parser s a
branch pa pb pc = Parser $ \st ->
  case runParser pa st of
    Just (st',_) -> runParser pb st'
    Nothing -> runParser pc st

endline :: Parser s ()
endline = branch (asciiChar '\r') (optional_ (asciiChar '\n')) (asciiChar '\n')

takeLine :: Parser s ByteString
takeLine = manyAsciiWhile (\c -> c /= '\n' && c /= '\r') <* endline
