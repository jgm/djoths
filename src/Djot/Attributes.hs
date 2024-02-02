{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Djot.Attributes
  ( pAttributes
  , parseAttributes
  , AttrParserState  -- opaque
  , AttrParseResult(..)
  )
where
import Data.Char (isAlphaNum, isSpace, isPunctuation)
import Djot.AST (Attr(..))
import Djot.Parse
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Char8 ( (!?) )
import Data.Typeable (Typeable)
import Data.Maybe (fromMaybe)
-- import Debug.Trace

pAttributes :: Parser s Attr
pAttributes = do
  bs <- lookahead takeRest
  case parseAttributes Nothing bs of
    Done (attr, off) -> attr <$ skip off
    _ -> failed

data AttrParseResult =
    Done (Attr, Int) -- result and byte offset
  | Failed Int -- byte offset of failure
  | Partial AttrParserState -- entire bytestring consumed
  deriving (Show, Typeable)

data AttrParserState =
  AttrParserState
  { aState :: AState
  , subject :: ByteString
  , offset :: Int
  , parts :: [AttrPart] }
  deriving (Show, Typeable)

data AState =
    SCANNING
  | AFTER_KEY
  | SCANNING_VALUE
  | SCANNING_QUOTED_VALUE
  | SCANNING_ESCAPE
  | SCANNING_COMMENT
  | FAIL
  | DONE
  | START
  deriving (Eq, Ord, Show, Typeable)

data AttrPart =
    AttrId ByteString
  | AttrClass ByteString
  | AttrKey ByteString
  | AttrValue ByteString
  deriving (Eq, Ord, Show, Typeable)

-- resumable parser -- parts in reverse order
parseAttributes :: Maybe AttrParserState -> ByteString -> AttrParseResult
parseAttributes mbState bs =
  case go (fromMaybe AttrParserState{ aState = START
                                    , subject = bs
                                    , offset = 0
                                    , parts = [] } mbState) of
    AttrParserState{ aState = DONE, parts = attparts, offset = off } ->
      Done (attrPartsToAttr attparts, off)
    AttrParserState{ aState = FAIL, offset = off } -> Failed off
    st -> Partial st
 where
  go :: AttrParserState -> AttrParserState
  go st@(AttrParserState _ subj off _) = -- trace (show st) $
     case subj !? off of
       Nothing -> st
       Just nextc ->
        case aState st of
         SCANNING ->
           case nextc of
             '}' -> go st{ aState = DONE, offset = off + 1 }
             '%' -> go st{ aState = SCANNING_COMMENT, offset = off + 1 }
             '#' -> go $ takePart isNameChar AttrId SCANNING st{ offset = off + 1 }
             '.' -> go $ takePart isNameChar AttrClass SCANNING st{ offset = off + 1 }
             c | isKeyChar c -> go $ takePart isKeyChar AttrKey AFTER_KEY st
             c | isWs c -> go $ skipWhile isWs st
             _ -> st{ aState = FAIL }
         AFTER_KEY ->
           case nextc of
             '=' -> go st{ aState = SCANNING_VALUE, offset = off + 1 }
             _ -> st{ aState = FAIL }
         SCANNING_VALUE ->
           case nextc of
             '"' -> go st{ aState = SCANNING_QUOTED_VALUE, offset = off + 1 }
             c | isBareValChar c -> go $ takePart isBareValChar AttrValue SCANNING st
             _ -> st{ aState = FAIL }
         SCANNING_QUOTED_VALUE ->
           case nextc of
             '"' -> go st{ aState = SCANNING, offset = off + 1 }
             '\\' -> go st{ aState = SCANNING_ESCAPE, offset = off + 1 }
             c | isWs c ->
                 let st' = skipWhile isWs st
                   in go st'{ parts = AttrValue " " : parts st' }
             _ -> go $ takePart (\c -> not (isWs c || c == '"' || c == '\\'))
                        AttrValue SCANNING_QUOTED_VALUE st
         SCANNING_ESCAPE ->
           go st{ aState = SCANNING_QUOTED_VALUE, offset = off + 1,
                  parts = AttrValue (B8.singleton nextc) : parts st }
         SCANNING_COMMENT ->
           case nextc of
             '%' -> go st{ aState = SCANNING, offset = off + 1 }
             '}' -> st{ aState = DONE, offset = off + 1 }
             _ -> go $ skipWhile (\c -> not (c == '%' || c == '}')) st
         FAIL -> st
         DONE -> st
         START ->
           case nextc of
             '{' -> go st{ aState = SCANNING, offset = off + 1 }
             _ -> st{ aState = FAIL }

takePart :: (Char -> Bool) -> (ByteString -> AttrPart) -> AState ->
            AttrParserState -> AttrParserState
takePart charP partConstructor nextstate st =
  case subject st !? offset st of
    Just c | charP c ->
      let val = B8.takeWhile charP (B8.drop (offset st) (subject st))
      in  st{ aState = nextstate,
              offset = offset st + B8.length val,
              parts = partConstructor val : parts st }
    _ -> st{ aState = FAIL }

skipWhile :: (Char -> Bool) -> AttrParserState -> AttrParserState
skipWhile charP st =
  case B8.findIndex (not . charP) (B8.drop (offset st) (subject st)) of
    Nothing -> st{ offset = B8.length (subject st) }
    Just i -> st{ offset = offset st + i }


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

attrPartsToAttr :: [AttrPart] -> Attr
attrPartsToAttr = go
 where
   go [] = Attr []
   go (AttrId bs : xs) = (<> Attr [("id",bs)]) $ go xs
   go (AttrClass bs : xs) = (<> Attr [("class",bs)]) $ go xs
   go zs =
     case break isAttrKey zs of
       (vs, AttrKey bs : xs) ->
         (<> Attr [(bs, mconcat (reverse $ map getAttrVal vs))]) $ go xs
       _ -> Attr [] -- should not happen
   isAttrKey (AttrKey _) = True
   isAttrKey _ = False
   getAttrVal (AttrValue bs) = bs
   getAttrVal _ = mempty

isNameChar :: Char -> Bool
isNameChar c = not (isSpace c)
    && (not (isPunctuation c) || c == ':' || c == '_' || c == '-')

isKeyChar :: Char -> Bool
isKeyChar c = isAlphaNum c || c == ':' || c == '_' || c == '-'

isBareValChar :: Char -> Bool
isBareValChar c = isAlphaNum c || c == ':' || c == '_' || c == '-'
