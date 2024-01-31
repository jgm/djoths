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
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Char8 ( (!?) )
-- import Debug.Trace

pAttributes :: ParserT m s String Attr
pAttributes = do
  bs <- lookahead takeRest
  let endState = pAttrParts
                 AttrParserState{ aState = START
                                , subject = bs
                                , offset = 0
                                , parts = [] }
  case aState endState of
    DONE -> attrPartsToAttr (parts endState)
               <$ skip (offset endState)
    _ -> failed

-- resumable parser -- parts in reverse order
pAttrParts :: AttrParserState -> AttrParserState
pAttrParts = go
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
             '#' ->
               if B8.null ident
                  then go st{ aState = FAIL, offset = off + 1 }
                  else go st{ offset = off + 1 + B8.length ident,
                              parts = AttrId ident : parts st }
               where
                 ident = B8.takeWhile isNameChar (B8.drop (off + 1) subj)
             '.' ->
               if B8.null cls
                  then go st{ aState = FAIL, offset = off + 1 }
                  else go st{ offset = off + 1 + B8.length cls,
                              parts = AttrClass cls : parts st }
               where
                 cls = B8.takeWhile isNameChar (B8.drop (off + 1) subj)
             c | isKeyChar c ->
               case subj !? (off + B8.length key) of
                 Just '=' ->
                    go st{ aState = SCANNING_VALUE, offset = off + 1 + B8.length key,
                           parts = AttrKey key : parts st }
                 _ -> st{ aState = FAIL }
               where
                 key = B8.takeWhile isKeyChar (B8.drop off subj)
             c | isWs c ->
                 case B8.findIndex (not . isWs) (B8.drop off subj) of
                   Nothing -> go st{ offset = B8.length subj }
                   Just i -> go st{ offset = off + i }
             _ -> st{ aState = FAIL }
         SCANNING_VALUE ->
           case nextc of
             '"' -> go st{ aState = SCANNING_QUOTED_VALUE, offset = off + 1 }
             c | isBareValChar c ->
                 go st{ aState = SCANNING, offset = off + B8.length val,
                        parts = AttrValue val : parts st }
               where
                 val = B8.takeWhile isBareValChar (B8.drop off subj)
             _ -> st{ aState = FAIL }
         SCANNING_QUOTED_VALUE ->
           case nextc of
             '"' -> go st{ aState = SCANNING, offset = off + 1 }
             '\\' -> go st{ aState = SCANNING_ESCAPE, offset = off + 1 }
             c | isWs c ->
                 case B8.findIndex (not . isWs) (B8.drop off subj) of
                   Nothing -> go st{ offset = off + B8.length subj }
                   Just i -> go st{ offset = off + i,
                                    parts = AttrValue " " : parts st }
             _ -> go st{ offset = off + B8.length val
                       , parts = AttrValue val : parts st }
               where
                 val = B8.takeWhile
                        (\c -> not (isWs c || c == '"' || c == '\\'))
                        (B8.drop off subj)
         SCANNING_ESCAPE ->
           go st{ aState = SCANNING_QUOTED_VALUE, offset = off + 1,
                  parts = AttrValue (B8.singleton nextc) : parts st }
         SCANNING_COMMENT ->
           case nextc of
             '%' -> go st{ aState = SCANNING, offset = off + 1 }
             '}' -> st{ aState = DONE, offset = off + 1 }
             _ -> case B8.findIndex (\c -> c == '%' || c == '}') (B8.drop off subj) of
                    Nothing -> go st{ offset = B8.length subj }
                    Just i -> go st{ offset = off + i }
         FAIL -> st
         DONE -> st
         START ->
           case nextc of
             '{' -> go st{ aState = SCANNING, offset = off + 1 }
             _ -> st{ aState = FAIL }



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

data AttrParserState =
  AttrParserState
  { aState :: AState
  , subject :: ByteString
  , offset :: Int
  , parts :: [AttrPart] }
  deriving (Show)

data AState =
    SCANNING
  | SCANNING_VALUE
  | SCANNING_QUOTED_VALUE
  | SCANNING_ESCAPE
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
