{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Strict #-}
module Djot.Html
  ( inlinesToByteString
  , renderHtml
  , RenderOptions(..)
  )
where

import Djot.AST
import Data.Tuple (swap)
import Djot.Parse (strToUtf8)
import Djot.Options (RenderOptions(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Builder (Builder, byteString, word8, intDec)
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.List (sort)
import Control.Monad.State
import qualified Data.Foldable as F

renderHtml :: RenderOptions -> Doc -> Builder
renderHtml opts doc = evalState
                          ( (<>) <$> toBuilder (docBlocks doc)
                                 <*> toNotes )
                         BState{ noteMap = docFootnotes doc
                               , noteRefs = mempty
                               , renderedNotes = mempty
                               , referenceMap = docReferences doc
                                             <> docAutoReferences doc
                               , options = opts
                               }

toNotes :: State BState Builder
toNotes = do
  st <- get
  let noterefs = noteRefs st
  let numnotes = M.size noterefs
  let revnoterefs = sort $ map swap $ M.toList noterefs
  let toNote (num, lab) =
        let num' = B8.pack (show num)
        in  inTags "li" NoPos (Attr [("id", "fn" <> num')])
              ("\n" <> fromMaybe mempty (M.lookup lab (renderedNotes st)))
             <> "\n"
  if numnotes < 1
     then pure mempty
     else pure $
           inTags "section" NoPos (Attr [("role", "doc-endnotes")])
            ("\n" <> singleTag "hr" NoPos mempty <> "\n" <>
              inTags "ol" NoPos mempty ("\n" <> foldMap toNote revnoterefs) <> "\n")
             <> "\n"

addBackref :: ByteString -> Blocks -> Blocks
addBackref num (Many bls) =
    Many $
      case Seq.viewr bls of
          rest Seq.:> Node pos attr (Para ils) ->
            rest Seq.|> Node pos attr (Para (ils <> backlink))
          _ -> bls Seq.|> Node NoPos mempty (Para backlink)
 where
   backlink = Many $ Seq.singleton $
               Node NoPos (Attr [("role", "doc-backlink")])
                 (Link (str (strToUtf8 "\8617\65038"))
                 (Direct ("#fnref" <> num)))

{-# INLINE escapeHtml #-}
escapeHtml :: ByteString -> Builder
escapeHtml bs =
  if hasEscapable bs
     then B.foldl' go mempty bs
     else byteString bs
 where
  hasEscapable = B.any (\w -> w == 38 || w == 60 || w == 62)
  go b 38 = b <> byteString "&amp;"
  go b 60 = b <> byteString "&lt;"
  go b 62 = b <> byteString "&gt;"
  go b c  = b <> word8 c

{-# INLINE escapeHtmlAttribute #-}
escapeHtmlAttribute :: ByteString -> Builder
escapeHtmlAttribute bs =
  if hasEscapable bs
     then B.foldl' go mempty bs
     else byteString bs
 where
  hasEscapable = B.any (\w -> w == 38 || w == 60 || w == 62 || w == 34)
  go b 38 = b <> byteString "&amp;"
  go b 60 = b <> byteString "&lt;"
  go b 62 = b <> byteString "&gt;"
  go b 34 = b <> byteString "&quot;"
  go b c  = b <> word8 c

data BState =
  BState { noteMap :: NoteMap
         , noteRefs :: M.Map ByteString Int
         , renderedNotes :: M.Map ByteString Builder
         , referenceMap :: ReferenceMap
         , options :: RenderOptions
         }

{-# SPECIALIZE toBuilder :: Blocks -> State BState Builder #-}
{-# SPECIALIZE toBuilder :: Inlines -> State BState Builder #-}
class ToBuilder a where
  toBuilder :: a -> State BState Builder

instance ToBuilder Inlines where
  toBuilder = fmap F.fold . mapM toBuilder . unMany

instance ToBuilder Blocks where
  toBuilder = fmap F.fold . mapM toBuilder . unMany

instance ToBuilder (Node Block) where
  toBuilder (Node pos attr bl) =
    let addNl = (<> "\n") in
    case bl of
      Para ils -> addNl . inTags "p" pos attr <$> toBuilder ils
      Heading lev ils ->
        let tag = case lev of
                    1 -> "h1"
                    2 -> "h2"
                    3 -> "h3"
                    4 -> "h4"
                    5 -> "h5"
                    6 -> "h6"
                    _ -> "p"
        in  addNl . inTags tag pos attr <$> toBuilder ils
      Section bls -> do
        contents <- toBuilder bls
        pure $ addNl $ inTags "section" pos attr $ "\n" <> contents
      ThematicBreak -> pure $ addNl $ singleTag "hr" NoPos attr
      BulletList listSpacing items ->
        addNl . inTags "ul" pos attr . ("\n" <>) . mconcat <$> mapM toLi items
          where
            toLi bls = addNl . inTags "li" NoPos mempty . ("\n" <>) <$>
                          toItemContents listSpacing bls
      OrderedList listAttr listSpacing items ->
        addNl . inTags "ol" pos (Attr [("start", strToUtf8 (show start))
                                  | start /= 1]
                              <> Attr [("type", typ) | typ /= "1"] <> attr)
         . ("\n" <>) . mconcat <$> mapM toLi items
          where
            typ = case orderedListStyle listAttr of
                    Decimal -> "1"
                    LetterUpper -> "A"
                    LetterLower -> "a"
                    RomanUpper -> "I"
                    RomanLower -> "i"
            start = orderedListStart listAttr
            toLi bls = addNl . inTags "li" NoPos mempty . ("\n" <>)
                          <$> toItemContents listSpacing bls
      DefinitionList listSpacing defs ->
        addNl . inTags "dl" pos attr . ("\n" <>) . mconcat
          <$> mapM (toDefinition listSpacing) defs
      TaskList listSpacing items ->
        addNl . inTags "ul" pos (Attr [("class", "task-list")] <> attr)
          . ("\n" <>) . mconcat <$> mapM (toTaskListItem listSpacing) items
      Div bls -> addNl . inTags "div" pos attr . ("\n" <>) <$> toBuilder bls
      BlockQuote bls ->
        addNl . inTags "blockquote" pos attr . ("\n" <>) <$> toBuilder bls
      CodeBlock lang bs -> pure $
        inTags "pre" pos attr (inTags "code" NoPos codeattr (escapeHtml bs))
          <> "\n"
         where
           codeattr = if B.null lang
                         then mempty
                         else Attr [("class", "language-" <> lang)]
      Table mbCaption rows -> do
        rows' <- mapM toRow rows
        capt <- case mbCaption of
                   Nothing -> pure mempty
                   Just (Caption bs) ->
                     addNl . inTags "caption" NoPos mempty
                       <$> case F.toList (unMany bs) of
                              [Node _pos at (Para ils)] | at == mempty
                                 -> toBuilder ils
                              _ -> ("\n" <>) <$> toBuilder bs
        pure $ addNl . inTags "table" pos attr . ("\n" <>) $ capt <> mconcat rows'
      RawBlock (Format "html") bs -> pure $ byteString bs
      RawBlock _ _ -> pure mempty

toRow :: [Cell] -> State BState Builder
toRow cells = (<> "\n") . inTags "tr" NoPos mempty . ("\n" <>) . mconcat
                <$> mapM toCell cells

toCell :: Cell -> State BState Builder
toCell (Cell cellType align ils) =
  (<> "\n") . inTags (if cellType == HeadCell
                         then "th"
                         else "td") NoPos attr <$> toBuilder ils
 where
   attr = Attr $ case align of
                   AlignDefault -> []
                   AlignLeft -> [("style", "text-align: left;")]
                   AlignRight -> [("style", "text-align: right;")]
                   AlignCenter -> [("style", "text-align: center;")]


toItemContents :: ListSpacing -> Blocks -> State BState Builder
toItemContents listSpacing = fmap F.fold . mapM go . unMany
 where
   go (Node pos attr bl) =
    case bl of
      Para ils
        | listSpacing == Tight ->
            if attr == mempty
               then (<> "\n") <$> toBuilder ils
               else (<> "\n") . inTags "span" pos attr <$> toBuilder ils
        | otherwise -> toBuilder (Node pos attr bl)
      _ -> toBuilder (Node pos attr bl)

toTaskListItem :: ListSpacing -> (TaskStatus, Blocks) -> State BState Builder
toTaskListItem listSpacing (status, bs) = do
  body <- case Seq.viewl $ unMany bs of
            Node pos attr (Para ils) Seq.:< rest ->
              toItemContents listSpacing (Many
                (Node pos attr
                 (Para (rawInline (Format "html") ("<label>" <> input) <>
                         ils <>
                         rawInline (Format "html") "</label>")) Seq.<| rest))
            _ -> toBuilder $ rawBlock (Format "html") input <> bs
  pure $ inTags "li" NoPos (Attr [("class", if status == Complete
                                             then "checked"
                                             else "unchecked")])  ("\n" <> body) <> "\n"
 where
   inputattr = " type=\"checkbox\"" <>
               if status == Complete then " checked=\"\"" else ""
   input = "<input" <> inputattr <> " />"

toDefinition :: ListSpacing -> (Inlines, Blocks) -> State BState Builder
toDefinition listSpacing (term, defn) = (<>) <$>
  ((<> "\n") . inTags "dt" NoPos mempty <$> toBuilder term) <*>
  ((<> "\n") . inTags "dd" NoPos mempty . ("\n" <>) <$> toItemContents listSpacing defn)

instance ToBuilder (Node Inline) where
  toBuilder (Node pos attr il) =
    case il of
      Str bs -> case attr of
                   Attr [] | pos == NoPos -> pure $ escapeHtml bs
                   _ -> pure $ inTags "span" pos attr $ escapeHtml bs
      SoftBreak -> do
        opts <- gets options
        pure $ word8 $ if preserveSoftBreaks opts then 10 else 32
      HardBreak -> pure $ singleTag "br" NoPos attr <> "\n"
      NonBreakingSpace -> pure $ byteString "&nbsp;"
      Emph ils -> inTags "em" pos attr <$> toBuilder ils
      Strong ils -> inTags "strong" pos attr <$> toBuilder ils
      Highlight ils -> inTags "mark" pos attr <$> toBuilder ils
      Insert ils -> inTags "ins" pos attr <$> toBuilder ils
      Delete ils -> inTags "del" pos attr <$> toBuilder ils
      Superscript ils -> inTags "sup" pos attr <$> toBuilder ils
      Subscript ils -> inTags "sub" pos attr <$> toBuilder ils
      Quoted SingleQuotes ils -> inSingleQuotes <$> toBuilder ils
      Quoted DoubleQuotes ils -> inDoubleQuotes <$> toBuilder ils
      Verbatim bs -> pure $ inTags "code" pos attr (escapeHtml bs)
      Math DisplayMath bs -> pure $
        inTags "span" pos (Attr [("class", "math display")] <> attr)
          ("\\[" <> escapeHtml bs <> "\\]")
      Math InlineMath bs -> pure $
        inTags "span" pos (Attr [("class", "math inline")] <> attr)
          ("\\(" <> escapeHtml bs <> "\\)")
      Symbol bs -> pure $
        inTags "span" pos (Attr [("class", "symbol")] <> attr)
          (":" <> escapeHtml bs <> ":")
      Span ils -> inTags "span" pos attr <$> toBuilder ils
      Link ils target -> do
        attr' <- case target of
                   Direct u -> pure $ Attr [("href", u)]
                   Reference label -> do
                     rm <- gets referenceMap
                     case lookupReference label rm of
                       Nothing -> pure $ Attr [("href", "")]
                       Just (u, Attr as) -> pure $ Attr (("href",u):as)
        inTags "a" pos (attr' <> attr) <$> toBuilder ils
      Image ils target -> do
        attr' <- case target of
                   Direct u -> pure $ Attr [("src", u)]
                   Reference label -> do
                     rm <- gets referenceMap
                     case lookupReference label rm of
                       Nothing -> pure $ Attr [("src", "")]
                       Just (u, Attr as) -> pure $ Attr (("src",u):as)
        pure $ singleTag "img" pos
                 (Attr [("alt", inlinesToByteString ils)] <> attr' <> attr)
      EmailLink email ->
        toBuilder (Node pos attr (Link (str email) (Direct ("mailto:" <> email))))
      UrlLink url -> toBuilder (Node pos attr (Link (str url) (Direct url)))
      RawInline (Format "html") bs -> pure $ byteString bs
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
                       rendered <- maybe (toBuilder $ addBackref num' (mempty :: Blocks))
                                    (toBuilder . addBackref num') (lookupNote label notemap)
                       modify $ \st -> st{ renderedNotes =
                                             M.insert label rendered (renderedNotes st) }
                   pure num
        let num' = B8.pack $ show num
        pure $ inTags "a" pos (Attr [("id", "fnref" <> num'),
                                     ("href", "#fn" <> num'),
                                     ("role", "doc-noteref")] <> attr) $
                 inTags "sup" pos mempty (escapeHtml num')

{-# INLINE inTags #-}
inTags :: ByteString -> Pos -> Attr -> Builder -> Builder
inTags tag pos attr contents =
  "<" <> byteString tag <> posToBuilder pos
      <> attrToBuilder attr <> ">" <> contents
      <> "</" <> byteString tag <> ">"


{-# INLINE singleTag #-}
singleTag :: ByteString -> Pos -> Attr -> Builder
singleTag tag pos attr =
  "<" <> byteString tag <> posToBuilder pos <> attrToBuilder attr <> ">"

{-# INLINE attrToBuilder #-}
attrToBuilder :: Attr -> Builder
attrToBuilder (Attr pairs) = foldMap go pairs
 where
   go (k,v) = " " <> byteString k <> "=\"" <> escapeHtmlAttribute v <> "\""

{-# INLINE posToBuilder #-}
posToBuilder :: Pos -> Builder
posToBuilder NoPos = mempty
posToBuilder (Pos sl sc el ec) =
  " data-pos=\"" <> intDec sl <> ":" <> intDec sc <> "-" <>
     intDec el <> ":" <> intDec ec <> "\""

inSingleQuotes :: Builder -> Builder
inSingleQuotes x =
  byteString (strToUtf8 "\x2018") <> x <> byteString (strToUtf8 "\x2019")

inDoubleQuotes :: Builder -> Builder
inDoubleQuotes x =
  byteString (strToUtf8 "\x201C") <> x <> byteString (strToUtf8 "\x201D")
