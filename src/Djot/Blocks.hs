{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Djot.Blocks
( parseDoc
, toIdentifier
)
where

import Prelude hiding (div)
import Text.Read (readMaybe)
import Data.Char (ord, isAsciiLower, isAsciiUpper, isAscii, isAlphaNum, isDigit)
import Data.Foldable as F
import Djot.Parse
import Djot.AST
import Djot.Inlines (parseInlines, parseTableCells)
import Djot.Options (ParseOptions(..), SourcePosOption(..))
import Djot.Attributes (parseAttributes, AttrParserState, AttrParseResult(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import Control.Monad (replicateM_, void, mzero, unless, when, guard, foldM)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative
import Data.Typeable (Typeable)
import Text.Printf (printf)
-- import Debug.Trace

parseDoc :: ParseOptions -> ByteString -> Either String Doc
parseDoc opts bs = do
  case parse pDoc PState{ psParseOptions = opts
                        , psContainerStack =
                            NonEmpty.fromList
                             [emptyContainer{ containerSpec = docSpec }]
                        , psReferenceMap = mempty
                        , psAutoReferenceMap = mempty
                        , psNoteMap = mempty
                        , psAttributes = mempty
                        , psAttrParserState = Nothing
                        , psIds = mempty
                        , psAutoIds = mempty
                        } [Chunk{ chunkLine = 1, chunkColumn = 0, chunkBytes = bs }] of
    Just doc -> Right doc
    Nothing -> Left "Parse failure."

data BlockType =
  Normal | ListItem | CaptionBlock | Document
  deriving (Show, Eq)

data BlockSpec =
  BlockSpec
  { -- | Descriptive name
    blockName :: String
  , -- | Type of block
    blockType :: BlockType
    -- | Parser for start of this block type
  , blockStart :: P ()
    -- | Parser that must return True if this block is to continue
  , blockContinue :: Container -> P Bool
    -- | Just blockType if it can contain that type of block
  , blockContainsBlock :: Maybe BlockType
    -- | True if it can accept text lines
  , blockContainsLines :: Bool
    -- | Parser that runs when block is closed, possibly
    -- updating the container.
  , blockClose :: Container -> P Container
    -- | Parser that runs when the document is closed, creating the
    -- block AST element.
  , blockFinalize :: Container -> Blocks
  }

docSpec :: BlockSpec
docSpec =
  BlockSpec
  { blockName = "Doc"
  , blockType = Document
  , blockStart = mzero
  , blockContinue = \_ -> pure True
  , blockContainsBlock = Just Normal
  , blockContainsLines = False
  , blockClose = pure
  , blockFinalize = finalizeChildren
  }

listItemSpec :: BlockSpec
listItemSpec =
  BlockSpec
  { blockName = "ListItem"
  , blockType = ListItem
  , blockStart = do
      ind <- sourceColumn
      ltypes <- pListStart
      skipMany spaceOrTab
      tip :| _ <- psContainerStack <$> getState
      case blockContainsBlock (containerSpec tip) of
        Just ListItem -> pure ()
        _ -> addContainer listSpec NoData
      addContainer listItemSpec (ListItemData ind ltypes False)
  , blockContinue = \container -> do
          True <$ fails
            (do skipMany spaceOrTab
                curind <- sourceColumn
                let liIndent = case containerData container of
                                 ListItemData i _ _ -> i
                                 _ -> error "Missing ListItemData"
                tip :| _ <- psContainerStack <$> getState
                guard (curind <= liIndent)
                case blockName (containerSpec tip) of
                  "Para" -> void pListStart
                  _ -> pure ())
        <|> pure False
  , blockContainsBlock = Just Normal
  , blockContainsLines = False
  , blockClose = pure
  , blockFinalize = finalizeChildren
  }

pListStart :: P [ListType]
pListStart = pBulletListStart <|> pDefinitionListStart <|> pOrderedListStart

pBulletListStart :: P [ListType]
pBulletListStart = do
  bulletchar <- satisfyByte (\c -> c == '-' || c == '+' || c == '*')
  followedByWhitespace
  (do skipMany spaceOrTab
      asciiChar '['
      status <- (Complete <$ byteString "x]")
            <|> (Complete <$ byteString "X]")
            <|> (Incomplete <$ byteString " ]")
      followedByWhitespace
      pure [Task status])
   <|> pure [Bullet bulletchar]

pDefinitionListStart :: P [ListType]
pDefinitionListStart = do
  asciiChar ':'
  followedByWhitespace
  pure [Definition]

groupLists :: Seq Container -> Seq ([ListType], Seq Container)
groupLists = snd . foldl' go ([], mempty)
 where
   go :: ([ListType], Seq ([ListType], Seq Container))
      -> Container
      -> ([ListType], Seq ([ListType], Seq Container))
   go (curtypes, lists) cont =
     case Seq.viewr lists of
       Seq.EmptyR -> (getListTypes cont,
                       Seq.singleton (getListTypes cont, Seq.singleton cont))
       rest Seq.:> (_, cur) ->
         let lt = getListTypes cont
             matchedTypes = [ty | ty <- curtypes, any (ty `matches`) lt]
         in if null matchedTypes
               then (getListTypes cont, lists Seq.|> (getListTypes cont, Seq.singleton cont)) -- new list
               else (matchedTypes, rest Seq.|> (matchedTypes, cur Seq.|> cont))

   matches :: ListType -> ListType -> Bool
   matches (Bullet b1) (Bullet b2) = b1 == b2
   matches (Ordered o1) (Ordered o2) =
     orderedListStyle o1 == orderedListStyle o2 &&
     orderedListDelim o1 == orderedListDelim o2
   matches Definition Definition = True
   matches Task{} Task{} = True
   matches _ _ = False


getListTypes :: Container -> [ListType]
getListTypes cont = case containerData cont of
                      ListItemData _ tys _ -> tys
                      _ -> error "Missing ListItemData"

pOrderedListStart :: P [ListType]
pOrderedListStart = do
  openParen <- (True <$ asciiChar '(') <|> pure False
  lookahead $ do
    skipSome $ skipSatisfyByte (\c -> isAscii c && isAlphaNum c)
    skipSatisfyByte (\c -> c == '.' || c == ')')
  stylesAndStarts <- decimalStart <|> romanStart <|> letterStart
  delimType <-
    if openParen
       then LeftRightParen <$ asciiChar ')'
       else (RightParen <$ asciiChar ')') <|> (RightPeriod <$ asciiChar '.')
  followedByWhitespace
  pure $ map
    (\(style, start) -> Ordered
        OrderedListAttributes
        { orderedListStyle = style
        , orderedListDelim = delimType
        , orderedListStart = start }) stylesAndStarts
 where
  decimalStart = do
    digits <- some (satisfyByte isDigit)
    case readMaybe digits of
      Just n -> pure [(Decimal, n)]
      Nothing -> mzero
  letterStart = do
    c <- satisfyByte (\c -> isAsciiLower c || isAsciiUpper c)
    if isAsciiLower c
       then pure [(LetterLower, 1 + (ord c - ord 'a'))]
       else pure [(LetterUpper, 1 + (ord c - ord 'A'))]
  romanStart = do
    (n, lettercase) <- pRomanNumeral
    let sty = if lettercase == Uppercase then RomanUpper else RomanLower
    let altsty = if lettercase == Uppercase then LetterUpper else LetterLower
    pure $ (sty, n) :
      case n of
        1 -> [(altsty, 9)]
        5 -> [(altsty, 22)]
        10 -> [(altsty, 24)]
        50 -> [(altsty, 12)]
        100 -> [(altsty, 3)]
        500 -> [(altsty, 4)]
        1000 -> [(altsty, 13)]
        _ -> []

data Case = Uppercase | Lowercase
  deriving (Eq)

pRomanNumeral :: P (Int, Case)
pRomanNumeral = do
  let isUpperRomanChar c = c == 'I' || c == 'V' || c == 'X' ||
                           c == 'L' || c == 'C' || c == 'D' || c == 'M'
  let isLowerRomanChar c = c == 'i' || c == 'v' || c == 'x' ||
                           c == 'l' || c == 'c' || c == 'd' || c == 'm'
  let isRomanChar c = isUpperRomanChar c || isLowerRomanChar c
  lettercase <- lookahead $ do
    c <- satisfyByte isRomanChar
    let lettercase = if isUpperRomanChar c then Uppercase else Lowercase
    skipMany $ skipSatisfyByte $
      case lettercase of
        Uppercase -> isUpperRomanChar
        Lowercase -> isLowerRomanChar
    skipSatisfyByte (\d -> d == ')' || d == '.')
    pure lettercase
  let rchar uc lc = satisfyByte $ if lettercase == Uppercase
                                       then (== uc)
                                       else (== lc)
  let one         = rchar 'I' 'i'
  let five        = rchar 'V' 'v'
  let ten         = rchar 'X' 'x'
  let fifty       = rchar 'L' 'l'
  let hundred     = rchar 'C' 'c'
  let fivehundred = rchar 'D' 'd'
  let thousand    = rchar 'M' 'm'
  thousands <- (1000 *) . length <$> many thousand
  ninehundreds <- option 0 $ hundred >> thousand >> return 900
  fivehundreds <- option 0 $ 500 <$ fivehundred
  fourhundreds <- option 0 $ hundred >> fivehundred >> return 400
  hundreds <- (100 *) . length <$> many hundred
  nineties <- option 0 $ ten >> hundred >> return 90
  fifties <- option 0 (50 <$ fifty)
  forties <- option 0 $ ten >> fifty >> return 40
  tens <- (10 *) . length <$> many ten
  nines <- option 0 $ one >> ten >> return 9
  fives <- option 0 (5 <$ five)
  fours <- option 0 $ one >> five >> return 4
  ones <- length <$> many one
  let total = thousands + ninehundreds + fivehundreds + fourhundreds +
              hundreds + nineties + fifties + forties + tens + nines +
              fives + fours + ones
  if total == 0
     then mzero
     else return (total, lettercase)
 where
   option defval p = p <|> pure defval

listSpec :: BlockSpec
listSpec =
  BlockSpec
  { blockName = "List"
  , blockType = Normal
  , blockStart = mzero -- added in listItemSpec
  , blockContinue = \_ -> pure True
  , blockContainsBlock = Just ListItem
  , blockContainsLines = False
  , blockClose = pure
  , blockFinalize = foldMap itemsToList . groupLists . containerChildren
  }

itemsToList :: ([ListType], Seq Container) -> Blocks
itemsToList (ltypes, containers) =
  case containers of
    Seq.Empty -> mempty
    _ ->
       let spacing =
             case Seq.viewr containers of
               Seq.EmptyR -> Tight
               as Seq.:> _ | any itemEndsWithBlank as ||
                             any hasChildrenSeparatedWithBlank containers
                          -> Loose
               _ -> Tight
           items' = toList items
           taskListStatus = map getTaskStatus (toList containers)
       in case ltypes of
            Bullet{} : _-> bulletList spacing items'
            Ordered _ : _->
              orderedList (chooseOrderedAttr ltypes) spacing items'
            Definition : _ -> definitionList spacing $ map toDefinition items'
            Task _ : _ -> taskList spacing $ zip taskListStatus items'
            [] -> mempty
 where
   items = map finalize $ toList containers
   getTaskStatus cont = case getListTypes cont of
                           ([Task stat] :: [ListType]) -> stat
                           _ -> error "getTaskStatus: wrong shape"
   -- when ambiguous between roman and lettered list, choose roman if start number is 1,
   -- otherwise lettered
   chooseOrderedAttr os =
     case [at | Ordered at <- os, isRomanStartOne at] of
       (a:_) -> a
       _ -> case [at | Ordered at <- os, isLettered at] of
         (a:_)  -> a
         _ -> case [at | Ordered at <- os] of
           (a:_) -> a
           [] -> error "chooseOrderedAttr on empty list"
   isRomanStartOne at = (orderedListStyle at == RomanUpper ||
                         orderedListStyle at == RomanLower) &&
                         orderedListStart at == 1
   isLettered at = orderedListStyle at == LetterUpper ||
                   orderedListStyle at == LetterLower

-- | We determine whether a list item ends with a blank line by
-- comparing its end line with the end line of its last child.
itemEndsWithBlank :: Container -> Bool
itemEndsWithBlank li =
  case Seq.viewr (containerChildren li) of
    Seq.EmptyR -> False
    _ Seq.:> lastChild -> containerEndLine li > containerEndLine lastChild

-- | We don't count blanks before lists, because
-- otherwise it would be impossible to have nested tight lists.
hasChildrenSeparatedWithBlank :: Container -> Bool
hasChildrenSeparatedWithBlank cont =
  or $ Seq.zipWith check children (Seq.drop 1 children)
 where
   children = (if Definition `elem` liTypes then Seq.drop 1 else id) $
                  containerChildren cont
   check x y = (blockName (containerSpec y) /= "List") &&
               (containerStartLine y > containerEndLine x)
   liTypes = getListTypes cont

toDefinition :: Blocks -> (Inlines, Blocks)
toDefinition bs =
  case Seq.viewl bs' of
    Node _ _ (Para ils) Seq.:< _ -> (ils, Many (Seq.drop 1 bs'))
    _ -> (mempty, bs)
  where
   bs' = unMany bs

sectionSpec :: BlockSpec
sectionSpec =
  BlockSpec
  { blockName = "Section"
  , blockType = Normal
  , blockStart = mzero  -- these containers are added by headingSpec
  , blockContinue = \_ -> pure True  -- these are closed by headingSpec
  , blockContainsBlock = Just Normal
  , blockContainsLines = False
  , blockClose = \container -> do
      case containerChildren container of
        h Seq.:<| _
          | blockName (containerSpec h) == "Heading" -> do
             let lev = case containerData container of
                         SectionData n _ -> n
                         _ -> error "Missing SectionData"
             let ils = case containerData h of
                         HeadingData _ xs -> xs
                         _ -> error "Missing HeadingData"
             (secid, attr, label) <- do
               let bs = inlinesToByteString ils
               let Attr ats = containerAttr container
               case lookup "id" ats of
                 Just id' -> pure (id', mempty, normalizeLabel bs)
                 Nothing -> do -- generate id from title
                   let generateId (n :: Int) base = do
                         let candidate
                               | n == 0 = base
                               | otherwise = base <> "-" <> B8.pack (show n)
                         ids <- psIds <$> getState
                         if candidate `Set.member` ids
                            then generateId (n+1) base
                            else do
                              updateState $ \st ->
                                st{ psIds = Set.insert candidate (psIds st)
                                  , psAutoIds = Set.insert candidate
                                                   (psAutoIds st) }
                              pure candidate
                   ident <- generateId 0 (toIdentifier bs)
                   pure (ident, mempty, normalizeLabel bs)
             -- add implicit reference
             let dest = "#" <> secid
             updateState $ \st -> st{ psAutoReferenceMap = insertReference label
                                     (dest, Attr []) (psAutoReferenceMap st) }

             pure container{ containerData =
                               SectionData lev (Just secid)
                           , containerAttr = containerAttr container <> attr }
        _ -> pure container
  , blockFinalize = \container ->
      let blocks = finalizeChildren container
          secid = case containerData container of
                    SectionData _ ident -> ident
                    _ -> error "Missing SectionData"
      in  maybe id (\ident -> addAttr (Attr [("id", ident)])) secid
           $ section blocks
  }

blockQuoteSpec :: BlockSpec
blockQuoteSpec =
  BlockSpec
  { blockName = "BlockQuote"
  , blockType = Normal
  , blockStart = do
      asciiChar '>'
      followedByWhitespace
      skipMany spaceOrTab
      addContainer blockQuoteSpec NoData
  , blockContinue = \_ -> do
      skipMany spaceOrTab
      asciiChar '>'
      followedByWhitespace
      skipMany spaceOrTab
      pure True
  , blockContainsBlock = Just Normal
  , blockContainsLines = False
  , blockClose = pure
  , blockFinalize = blockQuote . finalizeChildren
  }

tableSpec :: BlockSpec
tableSpec =
  BlockSpec
  { blockName = "Table"
  , blockType = Normal
  , blockStart = do
      lookahead pRawTableRow
      addContainer tableSpec (TableData mempty)
  , blockContinue = \container ->
      -- TODO: this is inefficient; we parse the inline contents
      -- twice. Find a better way.
      (True <$ lookahead pRawTableRow)
      <|> (True <$ followedByBlankLine)
      <|> (True <$ lookahead
                      (skipMany spaceOrTab *> asciiChar '^' *> spaceOrTab))
      <|> (True <$ guard (not (null (containerChildren container))))
  , blockContainsBlock = Just CaptionBlock
  , blockContainsLines = True
  , blockClose = \container -> do
      let lns = containerText container
      rows <- reverse . snd <$> foldM parseTableRow ([], []) lns
      pure $ container{ containerData = TableData rows }
  , blockFinalize = \container ->
      let rows = case containerData container of
                   TableData rs -> rs
                   _ -> error "Missing TableData"
          mbCaption =
            case Seq.viewr (containerChildren container) of
              Seq.EmptyR -> Nothing
              _ Seq.:> x -> Just . Caption $ blockFinalize (containerSpec x) x
      in  table mbCaption rows
  }

parseTableRow :: ([Align], [[Cell]])
              -> Chunk
              -> P ([Align], [[Cell]])
parseTableRow (aligns, rows) chunk =
  case B8.uncons (B8.strip $ chunkBytes chunk) of
    Just ('|',_) -> do
      res <- pTableCells aligns chunk
      case res of
        Left aligns' -> pure (aligns',
                               case rows of
                                 r:rs -> zipWith toHeadCell aligns' r : rs
                                 [] -> [] )
        Right cells -> pure (aligns, cells : rows)
    Nothing -> pure (aligns, rows)
    Just (_,_) -> mzero
 where
   toHeadCell align' (Cell _ _ ils) = Cell HeadCell align' ils

pTableCells :: [Align] -> Chunk -> P (Either [Align] [Cell])
pTableCells aligns chunk =
  case parse pTableSeps () [chunk] of
    Just aligns' -> pure $ Left aligns'
    Nothing -> do
      opts <- psParseOptions <$> getState
      case parseTableCells opts chunk of
        Right cs ->
          pure $ Right $
            zipWith (Cell BodyCell) (aligns ++ repeat AlignDefault) cs
        Left _ -> mzero

pTableSeps :: Parser () [Align]
pTableSeps = do
  skipMany spaceOrTab
  asciiChar '|'
  many pTableSep <* skipMany ws <* eof
 where
   pTableSep = do
     skipMany spaceOrTab
     start <- (True <$ asciiChar ':') <|> pure False
     skipSome (asciiChar '-')
     end <- (True <$ asciiChar ':') <|> pure False
     skipMany spaceOrTab
     asciiChar '|'
     pure $ case (start, end) of
              (True, True) -> AlignCenter
              (True, False) -> AlignLeft
              (False, True) -> AlignRight
              (False, False) -> AlignDefault

pRawTableRow :: P ()
pRawTableRow = do
  lookahead $ asciiChar '|'
  curline <- sourceLine
  curcolumn <- sourceColumn
  bs <- restOfLine
  void $ parseTableRow ([],[]) Chunk{ chunkLine = curline
                                    , chunkColumn = curcolumn
                                    , chunkBytes = bs }

captionSpec :: BlockSpec
captionSpec =
  BlockSpec
  { blockName = "Caption"
  , blockType = CaptionBlock
  , blockStart = do
      ind <- sourceColumn
      asciiChar '^'
      void spaceOrTab
      addContainer captionSpec $ CaptionData ind
  , blockContinue = \container -> (do
      skipMany spaceOrTab
      curind <- sourceColumn
      let ind = case containerData container of
                  CaptionData i -> i
                  _ -> error "Missing CaptionData"
      guard (curind > ind) <|> followedByBlankLine
      pure True) <|> pure False
  , blockContainsBlock = Just Normal
  , blockContainsLines = False
  , blockClose = pure
  , blockFinalize = finalizeChildren
  }


thematicBreakSpec :: BlockSpec
thematicBreakSpec =
  BlockSpec
  { blockName = "ThematicBreak"
  , blockType = Normal
  , blockStart = do
      let breakChar = skipSatisfyByte (\c -> c == '-' || c == '*')
                        *> skipMany spaceOrTab
      breakChar *> breakChar *> breakChar *> skipMany breakChar
      lookahead endline
      addContainer thematicBreakSpec NoData
  , blockContinue = \_ -> pure False
  , blockContainsBlock = Nothing
  , blockContainsLines = True
  , blockClose = pure
  , blockFinalize = const thematicBreak
  }

headingSpec :: BlockSpec
headingSpec =
  BlockSpec
  { blockName = "Heading"
  , blockType = Normal
  , blockStart = do
      lev <- length <$> some (asciiChar '#')
      followedByWhitespace
      skipMany spaceOrTab
      closeContainingSections lev
      addContainer sectionSpec $ SectionData lev Nothing
      addContainer headingSpec $ HeadingData lev mempty
  , blockContinue = \container -> do
       do skipMany spaceOrTab
          let lev = case containerData container of
                      HeadingData n _ -> n
                      _ -> error "Missing HeadingData"
          (True <$ (do lev' <- length <$> some (asciiChar '#')
                       guard (lev' == lev)
                       skipMany spaceOrTab))
            <|> (False <$ do
                    lookahead (asciiChar '#' <|> endline <|> eof))
            <|> pure True
  , blockContainsBlock = Nothing
  , blockContainsLines = True
  , blockClose = \container -> do
      ils <- parseTextLines container
      let lev = case containerData container of
                  HeadingData n _ -> n
                  _ -> error "Missing HeadingData"
      pure $ container{ containerData = HeadingData lev ils }
  , blockFinalize = \container ->
      let (lev, title) =
            case containerData container of
              HeadingData l t -> (l, t)
              _ -> error "Missing HeadingData"
      in  heading lev title
  }

codeBlockSpec :: BlockSpec
codeBlockSpec =
  BlockSpec
  { blockName = "CodeBlock"
  , blockType = Normal
  , blockStart = do
      indent <- sourceColumn
      ticks <- byteStringOf $ asciiChar '`' *> asciiChar '`' *> skipSome (asciiChar '`')
      skipMany spaceOrTab
      lang <- (byteStringOf
                 (skipSome $ skipSatisfyByte (\c -> c /= '`' && not (isWs c)))
                  <* skipMany spaceOrTab)
             <|> pure ""
      lookahead endline
      addContainer codeBlockSpec (CodeBlockData ticks lang indent)
  , blockContinue = \container -> do
      let (ticks, indent) = case containerData container of
                              CodeBlockData t _ i -> (t, i)
                              _ -> error "Missing CodeBlockData"
      gobbleSpaceToIndent indent
      (do skipMany spaceOrTab
          byteString ticks
          skipMany (asciiChar '`')
          skipMany spaceOrTab
          lookahead endline
          pure False)
        <|> pure True
  , blockContainsBlock = Nothing
  , blockContainsLines = True
  , blockClose = pure
  , blockFinalize = \container ->
      let lang = case containerData container of
                   CodeBlockData _ l _ -> l
                   _ -> error "Missing CodeBlockData"
      -- drop first line which should be empty
          bs = foldMap chunkBytes (Seq.drop 1 $ containerText container)
      in  case B8.uncons lang of
            Just ('=', fmt) -> rawBlock (Format fmt) bs
            _ -> codeBlock lang bs
  }

divSpec :: BlockSpec
divSpec =
  BlockSpec
  { blockName = "Div"
  , blockType = Normal
  , blockStart = do
      colons <- byteStringOf $
        asciiChar ':' *> asciiChar ':' *> skipSome (asciiChar ':')
      skipMany spaceOrTab
      label <- byteStringOf $ skipMany $ skipSatisfyByte (not . isWs)
      skipMany spaceOrTab
      lookahead endline
      addContainer divSpec (DivData colons label)
  , blockContinue = \container -> (do
      tip <- getTip
      -- see jgm/djot.js#109
      guard $ blockName (containerSpec tip) /= "CodeBlock"
      skipMany spaceOrTab
      let colons = case containerData container of
                     DivData c _ -> c
                     _ -> error "Missing DivData"
      byteString colons
      skipMany (asciiChar ':')
      skipMany spaceOrTab
      lookahead endline
      pure False) <|> pure True
  , blockContainsBlock = Just Normal
  , blockContainsLines = False
  , blockClose = pure
  , blockFinalize = \container ->
      let label = case containerData container of
                     DivData _ l -> l
                     _ -> error "Missing DivData"
      -- drop first line which should be empty
          bls = finalizeChildren container
      in  (if B.null label
              then id
              else addAttr (Attr [("class", label)])) $ div bls
  }

attrSpec :: BlockSpec
attrSpec =
  BlockSpec
  { blockName = "Attributes"
  , blockType = Normal
  , blockStart = do
      ind <- sourceColumn
      lookahead $ asciiChar '{'
      addContainer attrSpec $ AttributeData ind
  , blockContinue = \container -> do
      let ind = case containerData container of
                  AttributeData i -> i
                  _ -> error "Missing AttributeData"
      skipMany spaceOrTab
      curind <- sourceColumn
      mbapstate <- psAttrParserState <$> getState
      if curind <= ind
         then pure False
         else do
           let lastLine = case Seq.viewr (containerText container) of
                             _ Seq.:> ll -> chunkBytes ll
                             _ -> mempty
           case parseAttributes mbapstate lastLine of
             Done _ -> pure False
             Partial apstate' -> do
               updateState $ \st -> st{ psAttrParserState = Just apstate' }
               pure True
             Failed _ -> pure True -- not yet: keep going!
  , blockContainsBlock = Nothing
  , blockContainsLines = True
  , blockClose = \container -> do
      let bs = foldMap chunkBytes $ containerText container
      case parseAttributes Nothing bs of
        Done (attr, off)
          | B8.all isWs (B8.drop off bs) -> do
             updateState $ \st -> st{ psAttributes = psAttributes st <> attr }
             pure container
          | otherwise -> do
             ils <- parseTextLines container
             pure $ container{ containerSpec = paraSpec
                             , containerInlines = ils }
        _ -> do  -- could not parse lines as attribute, treat as Para
          ils <- parseTextLines container
          pure $ container{ containerSpec = paraSpec
                          , containerInlines = ils }
  , blockFinalize = const mempty
  }

referenceDefinitionSpec :: BlockSpec
referenceDefinitionSpec =
  BlockSpec
  { blockName = "ReferenceDefinition"
  , blockType = Normal
  , blockStart = do
      asciiChar '['
      fails (asciiChar '^') -- footnote
      label <- byteStringOf
                (some (skipSatisfyByte (\c -> c /= ']' && c /= '\n')))
      asciiChar ']'
      asciiChar ':'
      skipMany spaceOrTab
      addContainer referenceDefinitionSpec
        (ReferenceData (normalizeLabel label))
  , blockContinue = \_ ->
      True <$ skipSome spaceOrTab `notFollowedBy` endline
  , blockContainsBlock = Nothing
  , blockContainsLines = True
  , blockClose = \container -> do
      let label = case containerData container of
                    ReferenceData l -> l
                    _ -> error "Missing ReferenceData"
      let attr = containerAttr container
      let dest = B.filter (> 32) . foldMap chunkBytes $ containerText container
      updateState $ \st ->
        st{ psReferenceMap = insertReference label (dest, attr)
                                 (psReferenceMap st) }
      pure container
  , blockFinalize = const mempty
  }

footnoteSpec :: BlockSpec
footnoteSpec =
  BlockSpec
  { blockName = "Footnote"
  , blockType = Normal
  , blockStart = do
      ind <- sourceColumn
      asciiChar '['
      asciiChar '^'
      label <- byteStringOf
                (some (skipSatisfyByte (\c -> c /= ']' && c /= '\n')))
      asciiChar ']'
      asciiChar ':'
      skipMany spaceOrTab
      addContainer footnoteSpec $ FootnoteData ind (normalizeLabel label)
  , blockContinue = \container -> (do
      skipMany spaceOrTab
      curind <- sourceColumn
      let ind = case containerData container of
                  FootnoteData i _ -> i
                  _ -> error "Missing FootnoteData"
      guard (curind > ind) <|> followedByBlankLine
      pure True) <|> pure False
  , blockContainsBlock = Just Normal
  , blockContainsLines = True
  , blockClose = \container -> do
      let label = case containerData container of
                     FootnoteData _ l -> l
                     _ -> error "Missing FootnoteData"
      let bls = finalizeChildren container
      updateState $ \st -> st{ psNoteMap = insertNote label bls (psNoteMap st) }
      pure container
  , blockFinalize = const mempty
  }


paraSpec :: BlockSpec
paraSpec =
  BlockSpec
  { blockName = "Para"
  , blockType = Normal
  , blockStart = fails followedByBlankLine *> addContainer paraSpec NoData
  , blockContinue = \_ -> do
      skipMany spaceOrTab
      (False <$ lookahead (endline <|> eof)) <|> pure True
  , blockContainsBlock = Nothing
  , blockContainsLines = True
  , blockClose = \container -> do
      ils <- parseTextLines container
      pure $ container{ containerInlines = ils }
  , blockFinalize = para . containerInlines
  }

parseTextLines :: Container -> P Inlines
parseTextLines cont = do
  opts <- psParseOptions <$> getState
  either error pure . parseInlines opts $ containerText cont

emptyContainer :: Container
emptyContainer =
  Container { containerSpec = docSpec
            , containerChildren = mempty
            , containerText = mempty
            , containerInlines = mempty
            , containerStartLine = 1
            , containerStartColumn = 0
            , containerEndLine = 1
            , containerEndColumn = 0
            , containerData = NoData
            , containerAttr = mempty
            , containerSourcePos = False
            }

data Container =
  Container
  { containerSpec      :: BlockSpec
  , containerChildren  :: Seq Container
  , containerText :: Seq Chunk
  , containerInlines :: Inlines
  , containerStartLine :: Int
  , containerStartColumn :: Int
  , containerEndLine :: Int
  , containerEndColumn :: Int
  , containerData :: ContainerData
  , containerAttr :: Attr
  , containerSourcePos :: Bool
  }

data ContainerData =
    NoData
  | ListItemData Int [ListType] Bool
  | SectionData Int (Maybe ByteString)
  | HeadingData Int Inlines
  | CodeBlockData ByteString ByteString Int
  | DivData ByteString ByteString
  | FootnoteData Int ByteString
  | TableData [[Cell]]
  | CaptionData Int
  | AttributeData Int
  | ReferenceData ByteString
  deriving (Show, Eq, Ord, Typeable)

data ListType =
    Bullet Char
  | Ordered OrderedListAttributes
  | Definition
  | Task TaskStatus
  deriving (Show, Ord, Eq)

data PState =
  PState
  { psParseOptions :: ParseOptions
  , psContainerStack :: NonEmpty Container
  , psReferenceMap :: ReferenceMap
  , psAutoReferenceMap :: ReferenceMap
  , psNoteMap :: NoteMap
  , psAttributes :: Attr
  , psAttrParserState :: Maybe AttrParserState
  , psIds :: Set ByteString
  , psAutoIds :: Set ByteString
  }

type P = Parser PState

pDoc :: P Doc
pDoc = do
  bls <- pBlocks <* eof
  st <- getState
  pure $ Doc{ docBlocks = bls
            , docFootnotes = psNoteMap st
            , docReferences = psReferenceMap st
            , docAutoReferences = psAutoReferenceMap st
            , docAutoIdentifiers = psAutoIds st }

pBlocks :: P Blocks
pBlocks = processLines >> finalizeDocument

-- | Return value is True if all continuations match.
checkContinuations :: NonEmpty Container -> P Bool
checkContinuations = go . reverse . NonEmpty.toList
 where
   go [] = return True
   go (c:cs) = do continue <- blockContinue (containerSpec c) c <|> pure False
                  if continue
                     then go cs
                     else False <$ -- close len (c:cs) containers
                          replicateM_ (length (c:cs)) closeCurrentContainer

{-# INLINE processLines #-}
processLines :: P ()
processLines = do
  -- check continuations for open containers and close any that don't match
  containers <- psContainerStack <$> getState
  allContainersMatch <- checkContinuations containers

  -- check for new container starts and open if needed
  newContainersAdded <- tryContainerStarts

  followedByBlankLine <|> do
    -- determine if we have a lazy line
    let isLazy = not (allContainersMatch || newContainersAdded) &&
                 blockName (containerSpec (NonEmpty.head containers)) == "Para"

    when isLazy $ -- restore original containers
       updateState (\st -> st{ psContainerStack = containers })

    tip <- getTip

    when (blockContainsBlock (containerSpec tip) == Just Normal) $ do
      -- add a paragraph container
      skipMany spaceOrTab
      blockStart paraSpec

  !curline <- sourceLine
  !curcolumn <- sourceColumn
  !restline <- restOfLine

  -- if current container is a line container, add remainder of line
  modifyContainers $
    \(c :| rest) ->
       if blockContainsLines (containerSpec c)
          then c{ containerText = containerText c Seq.|>
                    Chunk{ chunkLine = curline
                         , chunkColumn = curcolumn
                         , chunkBytes = restline } } :| rest
          else c :| rest

  eof <|> processLines

-- True if new container was started
tryContainerStarts :: P Bool
tryContainerStarts = do
  (c :| _) <- psContainerStack <$> getState
  case blockContainsBlock (containerSpec c) of
    Just bt -> (do
      nextc <- lookahead (satisfyByte isAscii)
      next <- if nextc == ' ' || nextc == '\t'
                 then skipMany spaceOrTab *> lookahead (satisfyByte isAscii)
                 else pure nextc
      case next of
        '>' -> blockStart blockQuoteSpec
        '#' -> blockStart headingSpec
        ':' -> blockStart divSpec <|> blockStart listItemSpec
        '*' -> blockStart thematicBreakSpec <|> blockStart listItemSpec
        '-' -> blockStart thematicBreakSpec <|> blockStart listItemSpec
        '`' -> blockStart codeBlockSpec
        '{' -> blockStart attrSpec
        '[' -> blockStart referenceDefinitionSpec <|> blockStart footnoteSpec
        '|' | bt == Normal -> blockStart tableSpec
        '^' | bt == CaptionBlock -> blockStart captionSpec
        _ -> blockStart listItemSpec
      True <$ tryContainerStarts) <|> pure False
    _ -> pure False

-- | Close and finalize containers, returning Blocks.
finalizeDocument :: P Blocks
finalizeDocument = do
  cs <- psContainerStack <$> getState
  case cs of
    _ :| [] -> closeCurrentContainer >> finalize <$> getTip
    _ -> closeCurrentContainer >> finalizeDocument

{-# INLINE closeCurrentContainer #-}
-- | Close container and add to parent container.
closeCurrentContainer :: P ()
closeCurrentContainer = do
  cs <- psContainerStack <$> getState
  cs' <- case cs of
           _ :| [] -> pure cs
           c :| rest -> do
             case containerAttr c of
               Attr as | Just ident <- lookup "id" as
                 -> updateState $ \st -> st{ psIds = Set.insert ident (psIds st) }
               _ -> pure ()
             c' <- blockClose (containerSpec c) c
             pure (c':|rest)
  curline <- sourceLine
  case cs' of
    c :| (d:rest) -> updateState $
        \st -> st{ psContainerStack =
                   d{ containerChildren = containerChildren d Seq.|>
                        c{ containerEndLine = curline
                         , containerEndColumn = 0 } } :| rest }
    c :| [] -> updateState $
        \st -> st{ psContainerStack =
                   c{ containerEndLine = curline
                    , containerEndColumn = 0 } :| [] }

{-# INLINE modifyContainers #-}
modifyContainers :: (NonEmpty Container -> NonEmpty Container) -> P ()
modifyContainers f =
  updateState $ \st -> st{ psContainerStack = f (psContainerStack st) }

{-# INLINE addContainer #-}
addContainer :: BlockSpec -> ContainerData -> P ()
addContainer bspec bdata = do
  curline <- sourceLine
  curcol <- sourceColumn
  attr <- psAttributes <$> getState
  opts <- psParseOptions <$> getState
  let newcontainer = emptyContainer { containerSpec = bspec
                                    , containerStartLine = curline
                                    , containerStartColumn = curcol
                                    , containerEndLine = curline
                                    , containerEndColumn = curcol
                                    , containerData = bdata
                                    , containerAttr = attr
                                    , containerSourcePos = sourcePositions opts /= NoSourcePos }
  unless (blockName bspec == "Attributes") $
    updateState $ \st -> st{ psAttributes = mempty }
  closeInappropriateContainers bspec
  modifyContainers (newcontainer NonEmpty.<|)

closeInappropriateContainers :: BlockSpec -> P ()
closeInappropriateContainers spec = do
  -- close containers until we get one that can accept this type of container
  cs <- psContainerStack <$> getState
  case cs of
    c :| _
      | blockContainsBlock (containerSpec c) == Just (blockType spec) ->
              pure ()
      | otherwise -> closeCurrentContainer *> closeInappropriateContainers spec


finalize :: Container -> Blocks
finalize cont =
  addAttr (if containerSourcePos cont
              then Attr [("data-pos", B8.pack $
                            printf "%d:%d-%d:%d"
                                (containerStartLine cont)
                                (containerStartColumn cont)
                                (containerEndLine cont)
                                (containerEndColumn cont))] <>
                     containerAttr cont
              else containerAttr cont)
     $ blockFinalize (containerSpec cont) cont

finalizeChildren :: Container -> Blocks
finalizeChildren = foldMap finalize . containerChildren

-- Gobble as much space as possible up to indent.
gobbleSpaceToIndent :: Int -> P ()
gobbleSpaceToIndent indent = do
  curindent <- sourceColumn
  when (curindent < indent) $
     optional_ (spaceOrTab *> gobbleSpaceToIndent indent)

{-# INLINE getTip #-}
-- Get tip of container stack.
getTip :: P Container
getTip = NonEmpty.head . psContainerStack <$> getState

closeContainingSections :: Int -> P ()
closeContainingSections lev = do
  tip <- getTip
  case containerData tip of
    SectionData lev' _ | lev' >= lev ->
      closeCurrentContainer >>
      closeContainingSections lev
    _ -> pure ()

-- TODO avoid detour through String
toIdentifier :: ByteString -> ByteString
toIdentifier bs =
  if null parts
     then "sec"
     else strToUtf8 $ intercalate "-" parts
 where
   isSym = (`elem` ("][~!@#$%^&*(){}`,.<>\\|=+/" :: [Char]))
   parts = words $ map (\c -> if isSym c then ' ' else c) $ utf8ToStr bs
