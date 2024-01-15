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
import Djot.FlatParse (strToUtf8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.List (sort)
import Control.Monad.State
import qualified Data.Foldable as F
import Text.DocLayout hiding (Doc)
import qualified Text.DocLayout as Layout
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

renderDjot :: Doc -> Layout.Doc Text
renderDjot doc = evalState ( (<>) <$> toLayout (docBlocks doc)
                                 <*> toNotes )
                         BState{ noteMap = docFootnotes doc
                               , noteRefs = mempty
                               , renderedNotes = mempty
                               , referenceMap = docReferences doc }

toNotes :: State BState (Layout.Doc Text)
toNotes = do
  st <- get
  let noterefs = noteRefs st
  let numnotes = M.size noterefs
  let revnoterefs = sort $ map swap $ M.toList noterefs
  undefined

{-# INLINE escapeDjot #-}
escapeDjot :: ByteString -> Layout.Doc Text
escapeDjot bs =
  undefined
--  if hasEscapable bs
--     then B.foldl' go mempty bs
--     else byteString bs
-- where --TODO
--  hasEscapable = B.any (\w -> w == 38 || w == 60 || w == 62)
--  go b 38 = b <> byteString "&amp;"
--  go b 60 = b <> byteString "&lt;"
--  go b 62 = b <> byteString "&gt;"
--  go b c  = b <> word8 c

{-# INLINE escapeDjotAttribute #-}
escapeDjotAttribute :: ByteString -> Layout.Doc Text
escapeDjotAttribute bs =
  undefined
--  if hasEscapable bs
--     then B.foldl' go mempty bs
--     else byteString bs
-- where
--  hasEscapable = B.any (\w -> w == 38 || w == 60 || w == 62 || w == 34)
--  go b 38 = b <> byteString "&amp;"
--  go b 60 = b <> byteString "&lt;"
--  go b 62 = b <> byteString "&gt;"
--  go b 34 = b <> byteString "&quot;"
--  go b c  = b <> word8 c

data BState =
  BState { noteMap :: NoteMap
         , noteRefs :: M.Map ByteString Int
         , renderedNotes :: M.Map ByteString (Layout.Doc Text)
         , referenceMap :: ReferenceMap
         }

{-# SPECIALIZE toLayout :: Blocks -> State BState (Layout.Doc Text) #-}
{-# SPECIALIZE toLayout :: Inlines -> State BState (Layout.Doc Text) #-}
class ToLayout a where
  toLayout :: a -> State BState (Layout.Doc Text)

instance ToLayout Inlines where
  toLayout = fmap F.fold . mapM toLayout . unInlines

instance ToLayout Blocks where
  toLayout = fmap F.fold . mapM toLayout . unBlocks

instance ToLayout (Node Block) where
  toLayout (Node attr bl) =
    case bl of
      Para ils -> undefined
      Heading lev ils -> undefined
      Section bls -> undefined
      ThematicBreak -> undefined
      BulletList listSpacing items -> undefined
      OrderedList listAttr listSpacing items -> undefined
      DefinitionList listSpacing defs -> undefined
      TaskList listSpacing items -> undefined
      Div bls -> undefined
      BlockQuote bls -> undefined
      CodeBlock lang bs -> undefined
      Table mbCaption rows -> undefined
      RawBlock (Format "djot") bs -> undefined
      RawBlock _ _ -> pure mempty

toRow :: [Cell] -> State BState (Layout.Doc Text)
toRow cells = undefined

toCell :: Cell -> State BState (Layout.Doc Text)
toCell (Cell cellType align ils) = undefined

toItemContents :: ListSpacing -> Blocks -> State BState (Layout.Doc Text)
toItemContents listSpacing = fmap F.fold . mapM go . unBlocks
 where
   go (Node attr bl) =
    case bl of
      Para ils
        | listSpacing == Tight ->
            if attr == mempty
               then toLayout ils
               else {- TODO -} toLayout ils
        | otherwise -> toLayout (Node attr bl)
      _ -> toLayout (Node attr bl)

toTaskListItem :: ListSpacing -> (TaskStatus, Blocks)
               -> State BState (Layout.Doc Text)
toTaskListItem listSpacing (status, bs) = undefined

toDefinition :: ListSpacing -> (Inlines, Blocks)
             -> State BState (Layout.Doc Text)
toDefinition listSpacing (term, defn) = undefined

instance ToLayout (Node Inline) where
  toLayout (Node attr il) =
    case il of
      Str bs -> pure $ escapeDjot bs
      SoftBreak -> undefined
      HardBreak -> undefined
      NonBreakingSpace -> undefined
      Emph ils -> undefined
      Strong ils -> undefined
      Highlight ils -> undefined
      Insert ils -> undefined
      Delete ils -> undefined
      Superscript ils -> undefined
      Subscript ils -> undefined
      Verbatim bs -> undefined
      Math mt bs -> undefined
      Symbol bs -> undefined
      Span ils -> undefined
      Link ils target -> undefined
      Image ils target -> undefined
      EmailLink email -> undefined
      UrlLink url -> undefined
      RawInline (Format "djot") bs -> undefined
      RawInline _ _ -> pure mempty
      FootnoteReference label -> do
        noterefs <- gets noteRefs
        notemap <- gets noteMap
        num <- case M.lookup label noterefs of
                 Just num -> pure num
                 Nothing -> do
                   let num = M.size noterefs + 1
                   modify $ \st -> st{ noteRefs = M.insert label num noterefs }
                   renderedNotesMap <- gets renderedNotes
                   case M.lookup label renderedNotesMap of
                     Just _ -> pure ()
                     Nothing -> do -- render the note and add to renderedNotes
                       let num' = B8.pack (show num)
                       undefined
                   pure num
        let num' = B8.pack $ show num
        undefined

{-# INLINE attrToLayout #-}
attrToLayout :: Attr -> Layout.Doc Text
attrToLayout (Attr pairs) = undefined

