{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
module Djot.Attributes
  ( pAttributes
  )
where
import Data.Char (isAlphaNum, isSpace, isPunctuation)
import Djot.AST (Attr(..))
import Djot.FlatParse
import Data.ByteString (ByteString)

--  attributes { id = "foo", class = "bar baz",
--               key1 = "val1", key2 = "val2" }
--  syntax:
--
--  attributes <- '{' (ignorable attribute)* ignorable* '}'
--  attribute <- identifier | class | keyval
--  identifier <- '#' name
--  class <- '.' name
--  name <- (nonspace, nonpunctuation other than ':', '_', '-')+
--  keyval <- key '=' val
--  key <- (ASCII_ALPHANUM | ':' | '_' | '-')+
--  val <- bareval | quotedval
--  bareval <- (ASCII_ALPHANUM | ':' | '_' | '-')+
--  quotedval <- '"' ([^"] | '\"') '"'
--  ignorable <- whitespace | comment
--  comment <- '%' [^%}]* '%'

{- TODO: resumable parser:

data AState =
    SCANNING
  | SCANNING_ID
  | SCANNING_CLASS
  | SCANNING_KEY
  | SCANNING_VALUE
  | SCANNING_BARE_VALUE
  | SCANNING_QUOTED_VALUE
  | SCANNING_QUOTED_VALUE_CONTINUATION
  | SCANNING_ESCAPED
  | SCANNING_ESCAPED_IN_CONTINUATION
  | SCANNING_COMMENT
  | FAIL
  | DONE
  | START
  deriving (Eq, Ord, Show)

data Event =
    AttrId ByteString
  | AttrClass ByteString
  | AttrKey ByteString
  | AttrValue ByteString
  deriving (Eq, Ord, Show)

eventsToAttr :: [Event] -> Attr
eventsToAttr = foldr go mempty
 where
   go (AttrId bs) attr = Attr [("id",bs)] <> attr
   go (AttrClass bs) attr = Attr [("class",bs)] <> attr
   go (AttrKey bs) attr = Attr [(k,"")] <> attr
   go (AttrValue bs) attr =
     case attr of
       Attr ((k,v) : rest) -> Attr ((k, v <> bs) : rest)
       Attr [] -> Attr []

-- resumable parser
pAttrEvents :: AState -> ParserT m AState (AState, [Event])
pAttrEvents = undefined

-}

pAttributes :: ParserT m s String Attr
pAttributes = try $ do
  asciiChar' '{'
  atts <- (do
    skipMany pIgnorable
    x <- pAttribute
    xs <- many (try (skipSome pIgnorable *> pAttribute))
    pure $ mconcat (x:xs)) <|> pure mempty
  skipMany pIgnorable
  asciiChar' '}'
  pure atts

pAttribute :: ParserT m s String Attr
pAttribute = pIdentifier <|> pClass <|> pKeyVal

pIdentifier :: ParserT m s String Attr
pIdentifier = do
  asciiChar' '#'
  name <- pName
  pure $ Attr [("id", name)]

pClass :: ParserT m s String Attr
pClass = do
  asciiChar' '.'
  name <- pName
  pure $ Attr [("class", name)]

pKeyVal :: ParserT m s String Attr
pKeyVal = do
  key <- pKey
  asciiChar' '='
  val <- pVal
  pure $ Attr [(key, val)]

pName :: ParserT m s String ByteString
pName = byteStringOf $ skipSome $
  skipSatisfy (\c -> not (isSpace c)
    && (not (isPunctuation c) || c == ':' || c == '_' || c == '-'))

pKey :: ParserT m s String ByteString
pKey =
  byteStringOf $ skipSome $ skipSatisfyAscii
    (\c -> isAlphaNum c || c == ':' || c == '_' || c == '-')

pVal :: ParserT m s String ByteString
pVal = pQuotedVal <|> pBareVal

pQuotedVal :: ParserT m s String ByteString
pQuotedVal = do
  asciiChar' '"'
  result <- some $
                (asciiChar' '\\' *> satisfy' (const True))
            <|> (' ' <$ skipSome ws)
            <|> satisfy' (/= '"')
  asciiChar' '"'
  pure $ strToUtf8 result

pBareVal :: ParserT m s String ByteString
pBareVal = pKey

pComment :: ParserT m s String ()
pComment = do
  asciiChar' '%'
  skipMany $ satisfy (\c -> c /= '}' && c /= '%')
  asciiChar' '%' <|> lookahead (asciiChar' '}')

pIgnorable :: ParserT m s String ()
pIgnorable = ws <|> pComment
