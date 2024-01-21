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

type AttrParserState = (AState, [AttrPart])

data AState =
    SCANNING
  | SCANNING_VALUE
  | SCANNING_QUOTED_VALUE
  | SCANNING_COMMENT
  | FAIL
  | DONE
  | START
  deriving (Eq, Ord, Show)

data AttrPart =
    AttrId ByteString
  | AttrClass ByteString
  | AttrKey ByteString
  | AttrValue ByteString
  deriving (Eq, Ord, Show)

attrParserStateToAttr :: AttrParserState -> Attr
attrParserStateToAttr = go . snd
 where
   go [] = Attr []
   go (AttrId bs : xs) = (<> Attr [("id",bs)]) $ go xs
   go (AttrClass bs : xs) = (<> Attr [("class",bs)]) $ go xs
   go zs =
     case break isAttrKey zs of
       (vs, AttrKey bs : xs) ->
         (<> Attr [(bs, mconcat (map getAttrVal vs))]) $ go xs
       _ -> Attr [] -- should not happen
   isAttrKey (AttrKey _) = True
   isAttrKey _ = False
   getAttrVal (AttrValue bs) = bs
   getAttrVal _ = mempty

-- resumable parser -- parts in reverse order
pAttrParts :: AttrParserState -> ParserT m s String AttrParserState
pAttrParts = go
 where
   go (state, parts) =
     -- if no more input, return intermediate state
     (eof *> pure (state, parts)) <|>
       (case state of
         SCANNING ->
           (asciiChar' '}' *> pure (DONE, parts))
           <|>
           (do asciiChar' '#'
               ident <- pName
               go (SCANNING, (AttrId ident : parts)))
           <|>
           (do asciiChar' '.'
               cls <- pName
               go (SCANNING, (AttrClass cls : parts)))
           <|>
           (asciiChar' '%' *> go (SCANNING_COMMENT, parts))
           <|>
           (do key <- pKey
               asciiChar' '='
               go (SCANNING_VALUE, (AttrKey key : parts)))
           <|>
           (skipSome ws *> go (SCANNING, parts))
           <|>
           pure (FAIL, parts)
         SCANNING_VALUE ->
           (asciiChar' '"' *> go (SCANNING_QUOTED_VALUE, parts))
           <|>
           (do val <- pBareVal
               go (SCANNING, AttrValue val : parts))
           <|>
           pure (FAIL, parts)
         SCANNING_QUOTED_VALUE ->
           (asciiChar' '"' *> go (SCANNING, parts))
           <|>
           (do s <- some $ (asciiChar' '\\' *> satisfy' (const True))
                        <|> (' ' <$ skipSome ws)
                        <|> satisfy' (/= '"')
               go (SCANNING_QUOTED_VALUE, AttrValue (strToUtf8 s) : parts))
         SCANNING_COMMENT ->
           (asciiChar' '%' *> go (SCANNING, parts))
           <|>
           (asciiChar' '}' *> pure (DONE, parts))
           <|>
           (skipSome (satisfyAscii' (\c -> c /= '%' && c /= '}')) *>
             go (SCANNING_COMMENT, parts))
         FAIL -> pure (FAIL, parts)
         DONE -> pure (DONE, parts)
         START -> (asciiChar' '{' *> go (SCANNING, [])) <|> pure (FAIL, []))
          <|> pure (FAIL, parts)

pAttributes :: ParserT m s String Attr
pAttributes = try $ do
  state@(st, _) <- pAttrParts (START, [])
  case st of
    DONE -> pure $ attrParserStateToAttr state
    FAIL -> err "Attribute parsing failed"
    _ -> err $ "Attribute parser returned " <> show st

--- old parser ---

pAttributes' :: ParserT m s String Attr
pAttributes' = try $ do
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
pKey = byteStringOf $ skipSome $ skipSatisfyAscii isKeyChar

isKeyChar :: Char -> Bool
isKeyChar c = isAlphaNum c || c == ':' || c == '_' || c == '-'

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
