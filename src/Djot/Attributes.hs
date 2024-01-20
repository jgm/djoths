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
--  attributes <- '{' ignorable* attribute (ignorable attribute)* ignorable* '}'
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

pAttributes :: ParserT m s String Attr
pAttributes = try $ do
  asciiChar' '{'
  skipMany pIgnorable
  x <- pAttribute
  xs <- many (skipSome pIgnorable *> pAttribute)
  skipMany pIgnorable
  asciiChar' '}'
  pure $ mconcat (x:xs)

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
