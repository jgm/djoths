{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}
module Djot.Djot
  ( renderDjot
  )
where

import Djot.AST
import Data.Char (ord, chr)
import Djot.FlatParse (utf8ToStr)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.List (sortOn, intersperse, transpose)
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
                               refs <- gets referenceMap >>= toReferences
                               notes <- toNotes
                               pure $ body $$ refs $$ notes <> cr)
                         BState{ noteMap = docFootnotes doc
                               , noteOrder = mempty
                               , referenceMap = docReferences doc
                               , afterSpace = True
                               , divNestingLevel = 0
                               , nestings = IntMap.fromList
                                  -- anything not in this list
                                  -- will ALWAYS get {}:
                                  [(ord '_', 0)
                                  ,(ord '*', 0)
                                  ,(ord '~', 0)
                                  ,(ord '\'', 0)
                                  ,(ord '"', 0)
                                  ,(ord '^', 0)]
                               }

data BState =
  BState { noteMap :: NoteMap
         , noteOrder :: M.Map ByteString Int
         , referenceMap :: ReferenceMap
         , afterSpace :: Bool
         , divNestingLevel :: Int
         , nestings :: IntMap.IntMap Int
         }

toReferences :: ReferenceMap -> State BState (Layout.Doc Text)
toReferences (ReferenceMap refs) =
  (<> cr) . vcat <$> mapM toReference (M.toList refs)

toReference :: (ByteString, (ByteString, Attr)) -> State BState (Layout.Doc Text)
toReference (label, (url, attr)) =
  case attr of
    Attr [("_implicit",_)] -> pure mempty  -- implicit reference
    _ -> do
       attr' <- toLayout attr
       let ref = "[" <> literal (fromUtf8 label) <> "]:" <+> literal (fromUtf8 url)
       pure $ attr' $$ ref

toNotes :: State BState (Layout.Doc Text)
toNotes = do
  noterefs <- gets noteOrder
  allLabels <- M.keys . unNoteMap <$> gets noteMap
  let sortedLabels = sortOn (\label -> M.lookup label noterefs) allLabels
  (<> cr) . vsep <$> mapM toNote sortedLabels

toNote :: ByteString -> State BState (Layout.Doc Text)
toNote label = do
  notes <- gets noteMap
  case lookupNote label notes of
    Nothing -> pure mempty
    Just bls ->
      hang 4 (toNoteRef label <> ":" <> space) <$> toLayout bls

fromUtf8 :: ByteString -> Text
fromUtf8 = decodeUtf8With lenientDecode

data EscapeContext = Normal

{-# INLINE escapeDjot #-}
escapeDjot :: EscapeContext -> ByteString -> Text
escapeDjot Normal bs
  | B8.any escapable bs = T.pack. go . utf8ToStr $ bs
  | otherwise = fromUtf8 bs
 where
  escapable c = c == '[' || c == ']' || c == '<' || c == '>' ||
                c == '$' || c == '!' || c == '{' || c == '}' || c == ':' ||
                c == '-' || c == '^' || c == '~' ||
                c == '*' || c == '_' || c == '\''|| c == '"' || c == '.' ||
                c == '|' || c == '`' || c == '\\'
  go [] = []
  go ('$':c:cs)
    | c == '`' = '\\' : '$' : c : go cs
    | otherwise = '$' : go (c : cs)
  go ('-':cs) =
    case cs of
      '-':_ -> '\\' : '-' : go cs
      _ -> '-' : go cs
  go ('.':cs) =
    case cs of
      '.':'.':_ -> '\\' : '.' : go cs
      _ -> '.' : go cs
  go (c:cs)
    | escapable c = '\\' : c : go cs
    | otherwise = c : go cs

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
        let ident' = case lookup "id" kvs of
                        Nothing -> []
                        Just ident ->
                          case lookup "_autogen" kvs of
                            Nothing -> [literal ("#" <> fromUtf8 ident)]
                            Just _ -> [] -- omit autogenerated identifers
            classes' = maybe []
                        (map (("." <>) . literal) . T.words . fromUtf8)
                        (lookup "class" kvs)
            kvs' = [ literal (fromUtf8 k) <> "=" <>
                       doubleQuotes (literal (escapeDjot Normal v))
                       | (k,v) <- kvs
                       , k /= "id" && k /= "class" && k /= "_implicit" && k /= "_autogen"]
        pure $ case ident' ++ classes' ++ kvs' of
                 [] -> mempty
                 as -> "{" <> hsep as <> "}"

instance ToLayout (Node Block) where
  toLayout (Node attr bl) =
    ($$) <$> toLayout (case bl of
                         Heading _ _ ->
                           -- don't print an id that was generated implicitly
                           let Attr as = attr
                           in   Attr [(k,v) | (k,v) <- as
                                            , not (k == "id" && "S_" `B8.isPrefixOf` v)]
                         _ -> attr)
         <*> (($$ blankline) <$> case bl of
               Para ils -> toLayout ils
               Heading lev ils -> do
                 contents <- toLayout ils
                 pure $ literal (T.replicate lev "#") <+> contents
               Section bls -> ($$ blankline) <$> toLayout bls
               ThematicBreak -> pure $ literal "* * * *"
               BulletList listSpacing items ->
                 (case listSpacing of
                    Tight -> vcat . map chomp
                    Loose -> vsep) <$>
                 mapM (fmap (hang 2 ("-" <> space)) . toLayout) items
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
               Div bls -> do
                 level <- gets divNestingLevel
                 modify' $ \st -> st{ divNestingLevel = level + 1 }
                 contents <- toLayout bls
                 modify' $ \st -> st{ divNestingLevel = level }
                 let colons = literal (T.replicate (level + 3) ":")
                 pure $ colons $$ contents $$ colons
               BlockQuote bls ->
                 if bls == mempty
                    then pure ">"
                    else prefixed "> " <$> toLayout bls
               CodeBlock lang bs -> do
                 let longesttickline =
                       case B8.lines bs of
                         [] -> 0
                         ls -> maximum $ map (B8.length . B8.takeWhile (=='`')) ls
                 let numticks = max 3 longesttickline
                 let ticks = literal $ T.replicate numticks "`"
                 let lang' = if lang == mempty
                                then mempty
                                else literal (fromUtf8 lang)
                 pure $ ticks <+> lang'
                      $$ literal (fromUtf8 bs)
                      $$ ticks
               Table mbCaption rows -> do
                 caption <- case mbCaption of
                               Nothing -> pure mempty
                               Just (Caption bls)
                                       -> hang 2 "^" <$> toLayout bls
                 body <- toTable rows
                 pure $ body $+$ caption
               RawBlock (Format "djot") bs ->
                 pure $ literal (fromUtf8 bs)
               RawBlock _ _ -> pure mempty)
         <* modify' (\st -> st{ afterSpace = True })

toTable :: [[Cell]] -> State BState (Layout.Doc Text)
toTable rows = do
  let getCellContents (Cell hd al ils) = ((hd, al),) <$> toLayout ils
  rowContents <- mapM (mapM getCellContents) rows
  let colwidths = map (maximum . map (offset . snd))
                      (transpose rowContents)
  let toCell width ((_,align), d) =
        (case align of
          AlignLeft -> lblock
          AlignRight -> rblock
          AlignCenter -> cblock
          AlignDefault -> lblock) width d
  let mkRow sep ds = sep $ "|" : intersperse "|" ds ++ ["|"]
  let toUnderline width ((_,al),_) = literal $
        case al of
           AlignLeft -> ":" <> T.replicate (width + 1) "-"
           AlignRight -> T.replicate (width + 1) "-" <> ":"
           AlignCenter -> ":" <> T.replicate width "-" <> ":"
           AlignDefault -> T.replicate width "-"
  let toRow cells =
         let isHeader = case cells of
                          ((HeadCell,_),_) : _ -> True
                          _ -> False
         in mkRow hsep (zipWith toCell colwidths cells)
            $$
            if isHeader
               then mkRow hcat (zipWith toUnderline colwidths cells)
               else mempty
  pure $ vcat $ map toRow rowContents

toDefinitionListItem :: (Inlines, Blocks) -> State BState (Layout.Doc Text)
toDefinitionListItem (term, def) = do
  term' <- toLayout term
  def' <- toLayout def
  pure $ hang 2 (":" <> space) $ term' $+$ def'

toTaskListItem :: (TaskStatus, Blocks) -> State BState (Layout.Doc Text)
toTaskListItem (status, bls) = do
  contents <- toLayout bls
  let marker = case status of
                  Incomplete -> "- [ ]" <> space
                  Complete -> "- [X]" <> space
  pure $ hang 2 marker contents

toOrderedListItem :: OrderedListAttributes -> Int -> Blocks
                  -> State BState (Layout.Doc Text)
toOrderedListItem listAttr num bs = do
  contents <- toLayout bs
  let marker = formatOrderedListMarker listAttr num
  pure $ hang (offset marker + 1) marker contents

formatOrderedListMarker :: OrderedListAttributes -> Int -> Layout.Doc Text
formatOrderedListMarker listAttr =
  addDelims (orderedListDelim listAttr) .
    formatNumber (orderedListStyle listAttr)

addDelims :: OrderedListDelim -> Layout.Doc Text -> Layout.Doc Text
addDelims RightPeriod d = d <> "."
addDelims RightParen d = d <> ")"
addDelims LeftRightParen d = "(" <> d <> ")"

formatNumber :: OrderedListStyle -> Int -> Layout.Doc Text
formatNumber Decimal n = literal (T.pack (show n))
formatNumber LetterUpper n = literal (T.singleton (chr (ord 'A' + n - 1)))
formatNumber LetterLower n = literal (T.singleton (chr (ord 'a' + n - 1)))
formatNumber RomanUpper n = literal $ toRomanNumeral n
formatNumber RomanLower n = literal $ T.toLower (toRomanNumeral n)

-- | Convert number < 4000 to uppercase roman numeral. (from pandoc)
toRomanNumeral :: Int -> T.Text
toRomanNumeral x
  | x >= 4000 || x < 0 = "?"
  | x >= 1000 = "M" <> toRomanNumeral (x - 1000)
  | x >= 900  = "CM" <> toRomanNumeral (x - 900)
  | x >= 500  = "D" <> toRomanNumeral (x - 500)
  | x >= 400  = "CD" <> toRomanNumeral (x - 400)
  | x >= 100  = "C" <> toRomanNumeral (x - 100)
  | x >= 90   = "XC" <> toRomanNumeral (x - 90)
  | x >= 50   = "L"  <> toRomanNumeral (x - 50)
  | x >= 40   = "XL" <> toRomanNumeral (x - 40)
  | x >= 10   = "X" <> toRomanNumeral (x - 10)
  | x == 9    = "IX"
  | x >= 5    = "V" <> toRomanNumeral (x - 5)
  | x == 4    = "IV"
  | x >= 1    = "I" <> toRomanNumeral (x - 1)
  | otherwise = ""



instance ToLayout (Node Inline) where
  toLayout (Node attr il) = (<>)
    <$> case il of
          Str bs -> do
            let chunks =
                  T.groupBy
                   (\c d -> (c /= ' ' && d /= ' ') || (c == ' ' && d == ' '))
                   (escapeDjot Normal bs)
            let toChunk ch
                  = case T.uncons ch of
                      Just (' ', rest) -> space <> literal rest
                      _ -> literal ch
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
          Quoted SingleQuotes ils -> surround '\'' ils
          Quoted DoubleQuotes ils -> surround '"' ils
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
          Link ils target -> do
            contents <- toLayout ils
            let suffix = toLinkSuffix target contents
            pure $ "[" <> contents <> "]" <> suffix
          Image ils target -> do
            contents <- toLayout ils
            let suffix = toLinkSuffix target contents
            pure $ "![" <> contents <> "]" <> suffix
          EmailLink email -> pure $ "<" <> literal (fromUtf8 email) <> ">"
          UrlLink url -> pure $ "<" <> literal (fromUtf8 url) <> ">"
          RawInline (Format "djot") bs -> pure $ literal (fromUtf8 bs)
          RawInline _ _ -> pure mempty
          FootnoteReference label -> do
            order <- gets noteOrder
            case M.lookup label order of
              Nothing -> modify' $ \st ->
                            st{ noteOrder =
                                  M.insert label (M.size order + 1) order }
              Just _ -> pure ()
            pure $ toNoteRef label
   <*> toLayout attr
    <* modify' (\st ->
                 st{ afterSpace =
                      case il of
                         Str bs | isWhite (B8.takeEnd 1 bs) -> True
                         SoftBreak -> True
                         HardBreak -> True
                         NonBreakingSpace -> True
                         _ -> False })

toLinkSuffix :: Target -> Layout.Doc Text -> Layout.Doc Text
toLinkSuffix (Direct url) _ = literal $ "(" <> fromUtf8 url <> ")"
toLinkSuffix (Reference label) d
  | render Nothing d == fromUtf8 label = literal "[]"
  | otherwise = literal $ "[" <> fromUtf8 label <> "]"

toVerbatimSpan :: ByteString -> Layout.Doc Text
toVerbatimSpan bs =
  ticks <> literal (fromUtf8 bs) <> ticks
 where
  ticks = literal $ T.replicate (maxticks + 1) "`"
  maxticks = fst $ B8.foldl' scanTicks (0,0) bs
  scanTicks (longest, theseticks) '`' =
     (max (theseticks + 1) longest, theseticks + 1)
  scanTicks (longest, _) _ = (longest, 0)

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
  modify' $ \st -> st{ nestings = IntMap.adjust (+ 1) (ord c) (nestings st)}
  contents <- toLayout ils
  modify' $ \st -> st{ nestings = IntMap.adjust (\x -> x - 1)
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
