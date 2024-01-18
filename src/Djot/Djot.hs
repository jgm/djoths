{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Strict #-}
module Djot.Djot
  ( renderDjot
  )
where

import Djot.AST
import Data.Tuple (swap)
import Data.Char (ord)
import Djot.FlatParse (strToUtf8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.List (sort)
import Control.Monad
import Control.Monad.State
import qualified Data.Foldable as F
import Text.DocLayout hiding (Doc)
import qualified Text.DocLayout as Layout
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.IntMap.Strict as IntMap

renderDjot :: Doc -> Layout.Doc Text
renderDjot doc = evalState (do body <- toLayout (docBlocks doc)
                               refs <- toReferences
                               notes <- toNotes
                               pure $ body $$ refs $$ notes)
                         BState{ noteMap = docFootnotes doc
                               , noteOrder = mempty
                               , referenceMap = docReferences doc
                               , afterSpace = True
                               , nestings = IntMap.fromList
                                  -- anything not in this list
                                  -- will ALWAYS get {}:
                                  [(ord '_', 0)
                                  ,(ord '*', 0)
                                  ,(ord '~', 0)
                                  ,(ord '^', 0)]
                               }

data BState =
  BState { noteMap :: NoteMap
         , noteOrder :: M.Map ByteString Int
         , referenceMap :: ReferenceMap
         , afterSpace :: Bool
         , nestings :: IntMap.IntMap Int
         }

toReferences :: State BState (Layout.Doc Text)
toReferences = do
  st <- get
  let refs = referenceMap st
  pure mempty -- TODO implement this

toNotes :: State BState (Layout.Doc Text)
toNotes = do
  noterefs <- gets noteOrder
  let labels = map snd $ sort $ map swap $ M.toList noterefs
  vsep <$> mapM toNote labels

toNote :: ByteString -> State BState (Layout.Doc Text)
toNote label = do
  notes <- gets noteMap
  case lookupNote label notes of
    Nothing -> pure mempty
    Just bls ->
      hang 4 (toNoteRef label <> ":" <> space) <$> toLayout bls

fromUtf8 :: ByteString -> Text
fromUtf8 = decodeUtf8With lenientDecode

data EscapeContext = Normal | Attribute

{-# INLINE escapeDjot #-}
escapeDjot :: EscapeContext -> Text -> Text
escapeDjot context t = t -- TODO

{-# SPECIALIZE toLayout :: Blocks -> State BState (Layout.Doc Text) #-}
{-# SPECIALIZE toLayout :: Inlines -> State BState (Layout.Doc Text) #-}
{-# SPECIALIZE toLayout :: Attr -> State BState (Layout.Doc Text) #-}
class ToLayout a where
  toLayout :: a -> State BState (Layout.Doc Text)

instance ToLayout Inlines where
  toLayout = fmap F.fold . mapM toLayout . unInlines

instance ToLayout Blocks where
  toLayout = fmap F.fold . mapM toLayout . unBlocks

instance ToLayout Attr where
  toLayout (Attr kvs)
    | null kvs = pure mempty
    | otherwise = do
        let ident' = maybe [] ((:[]) . literal . ("#" <>) . fromUtf8)
                        (lookup "id" kvs)
            classes' = maybe []
                        (map (("." <>) . literal) . T.words . fromUtf8)
                        (lookup "class" kvs)
            kvs' = [ literal (fromUtf8 k) <> "=" <>
                       doubleQuotes
                            (literal (escapeDjot Attribute (fromUtf8 v)))
                       | (k,v) <- kvs
                       , k /= "id" && k /= "class" ]
        pure $ "{" <> hsep (ident' ++ classes' ++ kvs') <> "}"

instance ToLayout (Node Block) where
  toLayout (Node attr bl) =
    ($$) <$> toLayout attr
         <*> case bl of
               Para ils -> ($$ blankline) <$> toLayout ils
               Heading lev ils -> do
                 contents <- toLayout ils
                 pure $ literal (T.replicate lev "#") <+> contents $$ blankline
               Section bls -> ($$ blankline) <$> toLayout bls
               ThematicBreak -> pure $ literal "* * * *" $$ blankline
               BulletList listSpacing items ->
                 (case listSpacing of
                    Tight -> vcat . map chomp
                    Loose -> vsep) <$>
                 mapM (fmap (hang 2 "-") . toLayout) items
               OrderedList listAttr listSpacing items ->
                 (case listSpacing of
                    Tight -> vcat . map chomp
                    Loose -> vsep) <$>
                 zipWithM (toOrderedListItem listAttr)
                          [(orderedListStart listAttr)..]
                          items
               DefinitionList listSpacing items ->
                 (case listSpacing of
                    Tight -> vcat . map chomp
                    Loose -> vsep) <$>
                 mapM toDefinitionListItem items
               TaskList listSpacing items ->
                 (case listSpacing of
                    Tight -> vcat . map chomp
                    Loose -> vsep) <$>
                 mapM toTaskListItem items
               Div bls -> undefined
               BlockQuote bls -> prefixed "> " <$> toLayout bls
               CodeBlock lang bs -> undefined
               Table mbCaption rows -> undefined
               RawBlock (Format "djot") bs ->
                 pure $ literal (fromUtf8 bs) $$ blankline
               RawBlock _ _ -> pure mempty
         <* modify (\st -> st{ afterSpace = True })

toDefinitionListItem :: (Inlines, Blocks) -> State BState (Layout.Doc Text)
toDefinitionListItem = undefined -- TODO

toTaskListItem :: (TaskStatus, Blocks) -> State BState (Layout.Doc Text)
toTaskListItem = undefined -- TODO

toOrderedListItem :: OrderedListAttributes -> Int -> Blocks
                  -> State BState (Layout.Doc Text)
toOrderedListItem listAttr num bs = undefined -- TODO

toRow :: [Cell] -> State BState (Layout.Doc Text)
toRow cells = undefined

toCell :: Cell -> State BState (Layout.Doc Text)
toCell (Cell cellType align ils) = undefined

instance ToLayout (Node Inline) where
  toLayout (Node attr il) = (<>)
    <$> case il of
          Str bs -> do
            let chunks =
                  T.groupBy
                   (\c d -> c /= ' ' && d /= ' ')
                   (escapeDjot Normal $ fromUtf8 bs)
            let toChunk ch = if T.all (== ' ') ch
                                then space
                                else literal $ ch
            pure $ hcat $ map toChunk chunks
          SoftBreak -> pure cr
          HardBreak -> pure (literal "\\" <> cr)
          NonBreakingSpace -> pure "\\ "
          Emph ils -> surround '_' ils
          Strong ils -> surround '*' ils
          Highlight ils -> surround '=' ils
          Insert ils -> surround '+' ils
          Delete ils -> surround '-' ils
          Superscript ils -> surround '^' ils
          Subscript ils -> surround '~' ils
          Verbatim bs -> pure $ toVerbatimSpan bs
          Math mt bs -> do
            let suffix = toVerbatimSpan bs
            let prefix = case mt of
                            DisplayMath -> "$$"
                            InlineMath -> "$"
            pure $ prefix <> suffix
          Symbol bs -> pure $ ":" <> literal (fromUtf8 bs) <> ":"
          Span ils -> do
            contents <- toLayout ils
            pure $ "[" <> contents <> "]" <>
                    case attr of  -- there must be attributes for it to be a span
                      Attr [] -> "{}"
                      _ -> mempty
          Link ils target -> undefined
          Image ils target -> undefined
          EmailLink email -> pure $ "<" <> literal (fromUtf8 email) <> ">"
          UrlLink url -> pure $ "<" <> literal (fromUtf8 url) <> ">"
          RawInline (Format "djot") bs -> pure $ literal (fromUtf8 bs)
          RawInline _ _ -> pure mempty
          FootnoteReference label -> do
            order <- gets noteOrder
            case M.lookup label order of
              Nothing -> modify $ \st ->
                            st{ noteOrder =
                                  M.insert label (M.size order + 1) order }
              Just _ -> pure ()
            pure $ toNoteRef label
   <*> toLayout attr
    <* modify (\st ->
                 st{ afterSpace =
                      case il of
                         Str bs | isWhite (B8.takeEnd 1 bs) -> True
                         SoftBreak -> True
                         HardBreak -> True
                         NonBreakingSpace -> True
                         _ -> False })

toVerbatimSpan :: ByteString -> Layout.Doc Text
toVerbatimSpan bs = undefined

isWhite :: ByteString -> Bool
isWhite " " = True
isWhite "\t" = True
isWhite _ = False

surround :: Char -> Inlines -> State BState (Layout.Doc Text)
surround c ils = do
  startAfterSpace <- gets afterSpace
  let startBeforeSpace =
        case Seq.viewl (unInlines ils) of
                Node _ (Str bs) Seq.:< _ ->
                    isWhite (B8.take 1 bs)
                _ -> False
  modify $ \st -> st{ nestings = IntMap.adjust (+ 1) (ord c) (nestings st)}
  contents <- toLayout ils
  modify $ \st -> st{ nestings = IntMap.adjust (\x -> x - 1)
                        (ord c) (nestings st)}
  endAfterSpace <- gets afterSpace
  nestingLevel <- gets (fromMaybe 1 . IntMap.lookup (ord c) . nestings)
  let core = char c <> contents <> char c
  pure $
    if nestingLevel == 0 &&
         startAfterSpace &&
         not (startBeforeSpace || endAfterSpace)
       then core
       else char '{' <> core <> char '}'

toNoteRef :: ByteString -> Layout.Doc Text
toNoteRef bs = literal ("[^" <> fromUtf8 bs <> "]")
