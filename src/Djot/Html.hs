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
import Data.ByteString.Builder (Builder, byteString, word8)
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
        in  inTags "li" (Attr [("id", "fn" <> num')])
              ("\n" <> fromMaybe mempty (M.lookup lab (renderedNotes st)))
             <> "\n"
  if numnotes < 1
     then pure mempty
     else pure $
           inTags "section" (Attr [("role", "doc-endnotes")])
            ("\n" <> singleTag "hr" mempty <> "\n" <>
              inTags "ol" mempty ("\n" <> foldMap toNote revnoterefs) <> "\n")
             <> "\n"

addBackref :: ByteString -> Blocks -> Blocks
addBackref num (Many bls) =
    Many $
      case Seq.viewr bls of
          rest Seq.:> Node attr (Para ils) ->
            rest Seq.|> Node attr (Para (ils <> backlink))
          _ -> bls Seq.|> Node mempty (Para backlink)
 where
   backlink = Many $ Seq.singleton $
               Node (Attr [("role", "doc-backlink")])
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
  toBuilder (Node attr bl) =
    let addNl = (<> "\n") in
    case bl of
      Para ils -> addNl . inTags "p" attr <$> toBuilder ils
      Heading lev ils ->
        let tag = case lev of
                    1 -> "h1"
                    2 -> "h2"
                    3 -> "h3"
                    4 -> "h4"
                    5 -> "h5"
                    6 -> "h6"
                    _ -> "p"
        in  addNl . inTags tag attr <$> toBuilder ils
      Section bls -> do
        contents <- toBuilder bls
        pure $ addNl $ inTags "section" attr $ "\n" <> contents
      ThematicBreak -> pure $ addNl $ singleTag "hr" attr
      BulletList listSpacing items ->
        addNl . inTags "ul" attr . ("\n" <>) . mconcat <$> mapM toLi items
          where
            toLi bls = addNl . inTags "li" mempty . ("\n" <>) <$>
                          toItemContents listSpacing bls
      OrderedList listAttr listSpacing items ->
        addNl . inTags "ol" (Attr [("start", strToUtf8 (show start))
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
            toLi bls = addNl . inTags "li" mempty . ("\n" <>)
                          <$> toItemContents listSpacing bls
      DefinitionList listSpacing defs ->
        addNl . inTags "dl" attr . ("\n" <>) . mconcat
          <$> mapM (toDefinition listSpacing) defs
      TaskList listSpacing items ->
        addNl . inTags "ul" (Attr [("class", "task-list")] <> attr)
          . ("\n" <>) . mconcat <$> mapM (toTaskListItem listSpacing) items
      Div bls -> addNl . inTags "div" attr . ("\n" <>) <$> toBuilder bls
      BlockQuote bls ->
        addNl . inTags "blockquote" attr . ("\n" <>) <$> toBuilder bls
      CodeBlock lang bs -> pure $
        inTags "pre" attr (inTags "code" codeattr (escapeHtml bs))
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
                     addNl . inTags "caption" mempty
                       <$> case F.toList (unMany bs) of
                              [Node at (Para ils)] | at == mempty
                                 -> toBuilder ils
                              _ -> ("\n" <>) <$> toBuilder bs
        pure $ addNl . inTags "table" attr . ("\n" <>) $ capt <> mconcat rows'
      RawBlock (Format "html") bs -> pure $ byteString bs
      RawBlock _ _ -> pure mempty

toRow :: [Cell] -> State BState Builder
toRow cells = (<> "\n") . inTags "tr" mempty . ("\n" <>) . mconcat
                <$> mapM toCell cells

toCell :: Cell -> State BState Builder
toCell (Cell cellType align ils) =
  (<> "\n") . inTags (if cellType == HeadCell
                         then "th"
                         else "td") attr <$> toBuilder ils
 where
   attr = Attr $ case align of
                   AlignDefault -> []
                   AlignLeft -> [("style", "text-align: left;")]
                   AlignRight -> [("style", "text-align: right;")]
                   AlignCenter -> [("style", "text-align: center;")]


toItemContents :: ListSpacing -> Blocks -> State BState Builder
toItemContents listSpacing = fmap F.fold . mapM go . unMany
 where
   go (Node attr bl) =
    case bl of
      Para ils
        | listSpacing == Tight ->
            if attr == mempty
               then (<> "\n") <$> toBuilder ils
               else (<> "\n") . inTags "span" attr <$> toBuilder ils
        | otherwise -> toBuilder (Node attr bl)
      _ -> toBuilder (Node attr bl)

toTaskListItem :: ListSpacing -> (TaskStatus, Blocks) -> State BState Builder
toTaskListItem listSpacing (status, bs) = do
  body <- case Seq.viewl $ unMany bs of
            Node attr (Para ils) Seq.:< rest ->
              toItemContents listSpacing (Many
                (Node attr
                 (Para (rawInline (Format "html") ("<label>" <> input) <>
                         ils <>
                         rawInline (Format "html") "</label>")) Seq.<| rest))
            _ -> toBuilder $ rawBlock (Format "html") input <> bs
  pure $ inTags "li" (Attr [("class", if status == Complete
                                         then "checked"
                                         else "unchecked")])  ("\n" <> body) <> "\n"
 where
   inputattr = " type=\"checkbox\"" <>
               if status == Complete then " checked=\"\"" else ""
   input = "<input" <> inputattr <> " />"

toDefinition :: ListSpacing -> (Inlines, Blocks) -> State BState Builder
toDefinition listSpacing (term, defn) = (<>) <$>
  ((<> "\n") . inTags "dt" mempty <$> toBuilder term) <*>
  ((<> "\n") . inTags "dd" mempty . ("\n" <>) <$> toItemContents listSpacing defn)

instance ToBuilder (Node Inline) where
  toBuilder (Node attr il) =
    case il of
      Str bs -> case attr of
                   Attr [] -> pure $ escapeHtml bs
                   _ -> pure $ inTags "span" attr $ escapeHtml bs
      SoftBreak -> do
        opts <- gets options
        pure $ word8 $ if preserveSoftBreaks opts then 10 else 32
      HardBreak -> pure $ singleTag "br" attr <> "\n"
      NonBreakingSpace -> pure $ byteString "&nbsp;"
      Emph ils -> inTags "em" attr <$> toBuilder ils
      Strong ils -> inTags "strong" attr <$> toBuilder ils
      Highlight ils -> inTags "mark" attr <$> toBuilder ils
      Insert ils -> inTags "ins" attr <$> toBuilder ils
      Delete ils -> inTags "del" attr <$> toBuilder ils
      Superscript ils -> inTags "sup" attr <$> toBuilder ils
      Subscript ils -> inTags "sub" attr <$> toBuilder ils
      Quoted SingleQuotes ils -> inSingleQuotes <$> toBuilder ils
      Quoted DoubleQuotes ils -> inDoubleQuotes <$> toBuilder ils
      Verbatim bs -> pure $ inTags "code" attr (escapeHtml bs)
      Math DisplayMath bs -> pure $
        inTags "span" (Attr [("class", "math display")] <> attr)
          ("\\[" <> escapeHtml bs <> "\\]")
      Math InlineMath bs -> pure $
        inTags "span" (Attr [("class", "math inline")] <> attr)
          ("\\(" <> escapeHtml bs <> "\\)")
      Symbol bs -> pure $
        inTags "span" (Attr [("class", "symbol")] <> attr)
          (":" <> escapeHtml bs <> ":")
      Span ils -> inTags "span" attr <$> toBuilder ils
      Link ils target -> do
        attr' <- case target of
                   Direct u -> pure $ Attr [("href", u)]
                   Reference label -> do
                     rm <- gets referenceMap
                     case lookupReference label rm of
                       Nothing -> pure $ Attr [("href", "")]
                       Just (u, Attr as) -> pure $ Attr (("href",u):as)
        inTags "a" (attr' <> attr) <$> toBuilder ils
      Image ils target -> do
        attr' <- case target of
                   Direct u -> pure $ Attr [("src", u)]
                   Reference label -> do
                     rm <- gets referenceMap
                     case lookupReference label rm of
                       Nothing -> pure $ Attr [("src", "")]
                       Just (u, Attr as) -> pure $ Attr (("src",u):as)
        pure $ singleTag "img"
                 (Attr [("alt", inlinesToByteString ils)] <> attr' <> attr)
      EmailLink email ->
        toBuilder (Node attr (Link (str email) (Direct ("mailto:" <> email))))
      UrlLink url -> toBuilder (Node attr (Link (str url) (Direct url)))
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
        pure $ inTags "a" (Attr [("id", "fnref" <> num'),
                                 ("href", "#fn" <> num'),
                                 ("role", "doc-noteref")] <> attr) $
                 inTags "sup" mempty (escapeHtml num')

{-# INLINE inTags #-}
inTags :: ByteString -> Attr -> Builder -> Builder
inTags tag attr contents =
  "<" <> byteString tag <> attrToBuilder attr <> ">" <> contents
      <> "</" <> byteString tag <> ">"

{-# INLINE singleTag #-}
singleTag :: ByteString -> Attr -> Builder
singleTag tag attr =
  "<" <> byteString tag <> attrToBuilder attr <> ">"

{-# INLINE attrToBuilder #-}
attrToBuilder :: Attr -> Builder
attrToBuilder (Attr pairs) = foldMap go pairs
 where
   go ("_implicit",_) = mempty
   go ("_autogen",_) = mempty
   go (k,v) = " " <> byteString k <> "=\"" <> escapeHtmlAttribute v <> "\""

inSingleQuotes :: Builder -> Builder
inSingleQuotes x =
  byteString (strToUtf8 "\x2018") <> x <> byteString (strToUtf8 "\x2019")

inDoubleQuotes :: Builder -> Builder
inDoubleQuotes x =
  byteString (strToUtf8 "\x201C") <> x <> byteString (strToUtf8 "\x201D")
