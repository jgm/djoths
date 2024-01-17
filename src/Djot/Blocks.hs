{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Djot.Blocks
( parseDoc
, ParseOptions(..)
)
where

import Prelude hiding (div)
import Data.Char (isSpace, ord, isAsciiLower, isAsciiUpper, toLower)
import Data.Foldable as F
import qualified FlatParse.Stateful as FP
import Djot.FlatParse
import Djot.AST
import Djot.Inlines (parseInlines, parseTableCells)
import Djot.Options (ParseOptions(..))
import Djot.Attributes (pAttributes)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import Data.STRef
import Control.Monad.ST
import Control.Monad (replicateM_, void, mzero, unless, when, guard, foldM)
import Data.Dynamic
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set

parseDoc :: ParseOptions -> ByteString -> Either String Doc
parseDoc opts bs = do
  let res = runST $ do
              st <- newSTRef $
                       PState{ psParseOptions = opts
                             , psContainerStack =
                                 NonEmpty.fromList
                                  [emptyContainer{ containerSpec = docSpec }]
                             , psCurrentLine = 0
                             , psCurrentLineStart = Pos 0
                             , psReferenceMap = mempty
                             , psNoteMap = mempty
                             , psAttributes = mempty
                             , psIds = mempty
                             }
              runParserST pDoc st 0 bs
  case res of
    OK doc _ unconsumedBs
      | B.null unconsumedBs -> pure doc
      | otherwise -> Left $ "Failed parsing at: " <>
          show (B.take 50 unconsumedBs)
    Fail -> Left "Fail"
    Err e -> Left e

data BlockType =
  Normal | ListItem | CaptionBlock | Document
  deriving (Show, Eq)

data BlockSpec s =
  BlockSpec
  { -- | Descriptive name
    blockName :: String
  , -- | Type of block
    blockType :: BlockType
    -- | Parser for start of this block type
  , blockStart :: P s ()
    -- | Parser that must return True if this block is to continue
  , blockContinue :: Container s -> P s Bool
    -- | Just blockType if it can contain that type of block
  , blockContainsBlock :: Maybe BlockType
    -- | True if it can accept text lines
  , blockContainsLines :: Bool
    -- | Parser that runs when block is closed, possibly
    -- updating the container.
  , blockClose :: Container s -> P s (Container s)
    -- | Parser that runs when the document is closed, creating the
    -- block AST element.
  , blockFinalize :: Container s -> Blocks
  }

-- TODO:
-- [ ] pipe table

specs :: [BlockSpec s]
specs = [blockQuoteSpec, headingSpec, divSpec, thematicBreakSpec,
         codeBlockSpec, listItemSpec, attrSpec,
         referenceDefinitionSpec, footnoteSpec,
         tableSpec, captionSpec]

docSpec :: BlockSpec s
docSpec =
  BlockSpec
  { blockName = "Doc"
  , blockType = Document
  , blockStart = failed
  , blockContinue = \_ -> pure True
  , blockContainsBlock = Just Normal
  , blockContainsLines = False
  , blockClose = pure
  , blockFinalize = finalizeChildren
  }

data ListItemData =
  ListItemData
  { liIndent :: Maybe Int
  , liTypes :: [ListType]
  , liHasBlankLines :: Bool }
  deriving (Show, Eq, Ord, Typeable)

listItemSpec :: BlockSpec s
listItemSpec =
  BlockSpec
  { blockName = "ListItem"
  , blockType = ListItem
  , blockStart = try $ do
      ind <- getIndent
      ltypes <- pListStart
      skipMany spaceOrTab
      tip :| _ <- getsP psContainerStack
      case blockContainsBlock (containerSpec tip) of
        Just ListItem -> pure ()
        _ -> addContainer listSpec ()
      addContainer listItemSpec (ListItemData ind ltypes False)
  , blockContinue = \container -> do
          True <$ fails
            (do skipMany spaceOrTab
                curind <- getIndent
                let liData = getContainerData container
                tip :| _ <- getsP psContainerStack
                case blockName (containerSpec tip) of
                  "Para" -> void pListStart
                  _ -> pure ()
                guard (curind <= liIndent liData))
        <|> pure False
  , blockContainsBlock = Just Normal
  , blockContainsLines = False
  , blockClose = pure
  , blockFinalize = finalizeChildren
  }

data ListType =
    Bullet Char
  | Ordered OrderedListAttributes
  | Definition
  | Task TaskStatus
  deriving (Show, Ord, Eq)

pListStart :: P s [ListType]
pListStart = pBulletListStart <|> pOrderedListStart <|> pDefinitionListStart

pBulletListStart :: P s [ListType]
pBulletListStart = do
  bulletchar <- satisfyAscii' (\c -> c == '-' || c == '+' || c == '*')
  (do skipMany spaceOrTab
      asciiChar' '['
      status <- (Complete <$ byteString "x]")
            <|> (Complete <$ byteString "X]")
            <|> (Incomplete <$ byteString " ]")
      skipMany spaceOrTab
      pure [Task status])
   <|> (do lookahead ws
           pure [Bullet bulletchar])

pDefinitionListStart :: P s [ListType]
pDefinitionListStart = do
  asciiChar' ':'
  lookahead ws
  pure [Definition]

groupLists :: Seq (Container a) -> Seq ([ListType], Seq (Container a))
groupLists = snd . foldl' go ([], mempty)
 where
   go :: ([ListType], Seq ([ListType], Seq (Container a)))
      -> Container a
      -> ([ListType], Seq ([ListType], Seq (Container a)))
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

   getListTypes :: Container a -> [ListType]
   getListTypes cont = maybe [] liTypes $ fromDynamic (containerData cont)

   matches :: ListType -> ListType -> Bool
   matches (Bullet b1) (Bullet b2) = b1 == b2
   matches (Ordered o1) (Ordered o2) =
     orderedListStyle o1 == orderedListStyle o2 &&
     orderedListDelim o1 == orderedListDelim o2
   matches Definition Definition = True
   matches Task{} Task{} = True
   matches _ _ = False

pOrderedListStart :: P s [ListType]
pOrderedListStart = try $ do
  openParen <- (True <$ asciiChar' '(') <|> pure False
  stylesAndStarts <- decimalStart
                 <|> romanLowerStart <|> romanUpperStart
                 <|> letterLowerStart <|> letterUpperStart
  delimType <-
    if openParen
       then LeftRightParen <$ asciiChar' ')'
       else (RightParen <$ asciiChar' ')') <|> (RightPeriod <$ asciiChar' '.')
  pure $ map
    (\(style, start) -> Ordered
        OrderedListAttributes
        { orderedListStyle = style
        , orderedListDelim = delimType
        , orderedListStart = start }) stylesAndStarts
 where
  decimalStart = do
    n <- anyAsciiDecimalInt
    pure [(Decimal, n)]
  letterLowerStart = do
    c <- satisfyAscii' isAsciiLower
    pure [(LetterLower, 1 + (ord c - ord 'a'))]
  letterUpperStart = do
    c <- satisfyAscii' isAsciiUpper
    pure [(LetterUpper, 1 + (ord c - ord 'A'))]
  romanLowerStart = do
    (n, raw) <- withByteString (pRomanNumeral Lowercase) (curry pure)
    pure $ (RomanLower, n) :
           case B8.unpack raw of
             [c] -> [(LetterLower, 1 + (ord c - ord 'a'))]
             _ -> []
  romanUpperStart = do
    (n, raw) <- withByteString (pRomanNumeral Uppercase) (curry pure)
    pure $ (RomanUpper, n) :
           case B8.unpack raw of
             [c] -> [(LetterUpper, 1 + (ord c - ord 'A'))]
             _ -> []

data Case = Uppercase | Lowercase
  deriving (Eq)

-- | Parses a roman numeral (uppercase or lowercase), returns number.
pRomanNumeral :: Case -> P s Int
pRomanNumeral lettercase = try $ do
  let rchar uc = satisfyAscii' $ if lettercase == Uppercase
                                    then (== uc)
                                    else (== toLower uc)
  let one         = rchar 'I'
  let five        = rchar 'V'
  let ten         = rchar 'X'
  let fifty       = rchar 'L'
  let hundred     = rchar 'C'
  let fivehundred = rchar 'D'
  let thousand    = rchar 'M'
  thousands <- (1000 *) . length <$> many thousand
  ninehundreds <- option 0 $ try $ hundred >> thousand >> return 900
  fivehundreds <- option 0 $ 500 <$ fivehundred
  fourhundreds <- option 0 $ try $ hundred >> fivehundred >> return 400
  hundreds <- (100 *) . length <$> many hundred
  nineties <- option 0 $ try $ ten >> hundred >> return 90
  fifties <- option 0 (50 <$ fifty)
  forties <- option 0 $ try $ ten >> fifty >> return 40
  tens <- (10 *) . length <$> many ten
  nines <- option 0 $ try $ one >> ten >> return 9
  fives <- option 0 (5 <$ five)
  fours <- option 0 $ try $ one >> five >> return 4
  ones <- length <$> many one
  let total = thousands + ninehundreds + fivehundreds + fourhundreds +
              hundreds + nineties + fifties + forties + tens + nines +
              fives + fours + ones
  if total == 0
     then mzero
     else return total
 where
   option defval p = p <|> pure defval

listSpec :: BlockSpec s
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

itemsToList :: ([ListType], Seq (Container s)) -> Blocks
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
   getTaskStatus cont = case liTypes <$> fromDynamic (containerData cont) of
                           Just ([Task stat] :: [ListType]) -> stat
                           _ -> error "getTaskStatus: container data has wrong shape"
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
itemEndsWithBlank :: Container s -> Bool
itemEndsWithBlank li =
  case Seq.viewr (containerChildren li) of
    Seq.EmptyR -> False
    _ Seq.:> lastChild -> containerEndLine li > containerEndLine lastChild

-- | We don't count blanks before lists, because
-- otherwise it would be impossible to have nested tight lists.
hasChildrenSeparatedWithBlank :: Container s -> Bool
hasChildrenSeparatedWithBlank cont =
  or $ Seq.zipWith check children (Seq.drop 1 children)
 where
   children = (if Definition `elem` liTypes lid then Seq.drop 1 else id) $
                  containerChildren cont
   check x y = (blockName (containerSpec y) /= "List") &&
               (containerStartLine y > containerEndLine x + 1)
   lid = getContainerData cont

toDefinition :: Blocks -> (Inlines, Blocks)
toDefinition bs =
  case Seq.viewl bs' of
    Node _ (Para ils) Seq.:< _ -> (ils, Blocks (Seq.drop 1 bs'))
    _ -> (mempty, bs)
  where
   bs' = unBlocks bs

sectionSpec :: BlockSpec s
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
             let SectionData lev _ = getContainerData container
             (secid, label) <- do
               let bs = fold (containerText h)
               let Attr ats = containerAttr container
               case lookup "id" ats of
                 Just id' -> pure (id', normalizeLabel bs)
                 Nothing -> do -- generate id from title
                   let generateId (n :: Int) base = do
                         let candidate
                               | n == 0 = base
                               | otherwise = base <> "-" <> B8.pack (show n)
                         ids <- getsP psIds
                         if candidate `Set.member` ids
                            then generateId (n+1) base
                            else do
                              modifyP $ \st ->
                                st{ psIds = Set.insert candidate (psIds st) }
                              pure candidate
                   ident <- generateId 0 (toIdentifier bs)
                   pure (ident, normalizeLabel bs)
             -- add implicit reference
             let dest = "#" <> secid
             modifyP $ \st -> st{ psReferenceMap =
                    case lookupReference label (psReferenceMap st) of
                        Nothing -> insertReference label (dest, mempty)
                                      (psReferenceMap st)
                        Just _ -> psReferenceMap st }
             pure container{ containerData =
                               toDyn $ SectionData lev (Just secid) }
        _ -> pure container
  , blockFinalize = \container ->
      let blocks = finalizeChildren container
          SectionData _ secid = getContainerData container
      in  maybe id (\ident -> addAttr (Attr [("id", ident)])) secid
           $ section blocks
  }

blockQuoteSpec :: BlockSpec s
blockQuoteSpec =
  BlockSpec
  { blockName = "BlockQuote"
  , blockType = Normal
  , blockStart = try $ do
      asciiChar' '>'
      lookahead ws
      skipMany spaceOrTab
      addContainer blockQuoteSpec ()
  , blockContinue = \_ -> (False <$ lookahead pBlankLine)
       <|> (True <$ try (asciiChar' '>' *> lookahead ws *> skipMany spaceOrTab))
  , blockContainsBlock = Just Normal
  , blockContainsLines = False
  , blockClose = pure
  , blockFinalize = blockQuote . finalizeChildren
  }

tableSpec :: BlockSpec s
tableSpec =
  BlockSpec
  { blockName = "Table"
  , blockType = Normal
  , blockStart = do
      lookahead pRawTableRow
      addContainer tableSpec (mempty :: [[Cell]])
  , blockContinue = \container ->
      -- TODO: this is inefficient; we parse the inline contents
      -- twice. Find a better way.
      (True <$ lookahead pRawTableRow)
      <|> (True <$ lookahead pBlankLine)
      <|> (True <$ lookahead
                      (try (skipMany spaceOrTab *> asciiChar' '^' *> spaceOrTab)))
      <|> (True <$ guard (not (null (containerChildren container))))
  , blockContainsBlock = Just CaptionBlock
  , blockContainsLines = True
  , blockClose = \container -> do
      let lns = containerText container
      rows <- reverse . snd <$> foldM parseTableRow ([], []) lns
      pure $ container{ containerData = toDyn rows }
  , blockFinalize = \container ->
      let rows = getContainerData container
          mbCaption =
            case Seq.viewr (containerChildren container) of
              Seq.EmptyR -> Nothing
              _ Seq.:> x -> Just . Caption $ blockFinalize (containerSpec x) x
      in  table mbCaption rows
  }

parseTableRow :: ([Align], [[Cell]])
              -> ByteString
              -> P s ([Align], [[Cell]])
parseTableRow (aligns, rows) bs =
  case B8.uncons (B8.strip bs) of
    Just ('|',rest) -> do
      res <- pTableCells aligns rest
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

pTableCells :: [Align] -> ByteString -> P s (Either [Align] [Cell])
pTableCells aligns bs =
  case runParser pTableSeps () 0 bs of
    OK aligns' _ _ -> pure $ Left aligns'
    Err _ -> mzero
    Fail -> do
      case parseTableCells bs of
        Right cs ->
          pure $ Right $
            zipWith (Cell BodyCell) (aligns ++ repeat AlignDefault) cs
        Left _ -> mzero

pTableSeps :: Parser () String [Align]
pTableSeps = many pTableSep <* eof
 where
   pTableSep = do
     skipMany spaceOrTab
     start <- (True <$ asciiChar' ':') <|> pure False
     skipSome (asciiChar' '-')
     end <- (True <$ asciiChar' ':') <|> pure False
     skipMany spaceOrTab
     asciiChar' '|'
     pure $ case (start, end) of
              (True, True) -> AlignCenter
              (True, False) -> AlignLeft
              (False, True) -> AlignRight
              (False, False) -> AlignDefault

pRawTableRow :: P s ()
pRawTableRow = try $ do
  lookahead $ asciiChar' '|'
  pLine >>= void . parseTableRow ([],[]) . B8.strip

captionSpec :: BlockSpec s
captionSpec =
  BlockSpec
  { blockName = "Caption"
  , blockType = CaptionBlock
  , blockStart = try $ do
      ind <- getIndent
      asciiChar' '^'
      void spaceOrTab
      addContainer captionSpec ind
  , blockContinue = \container -> try (do
      skipMany spaceOrTab
      curind <- getIndent
      let ind = getContainerData container
      guard (curind > ind) <|> lookahead pBlankLine
      pure True) <|> pure False
  , blockContainsBlock = Just Normal
  , blockContainsLines = False
  , blockClose = pure
  , blockFinalize = finalizeChildren
  }


thematicBreakSpec :: BlockSpec s
thematicBreakSpec =
  BlockSpec
  { blockName = "ThematicBreak"
  , blockType = Normal
  , blockStart = try $ do
      let breakChar = skipSatisfyAscii' (\c -> c == '-' || c == '*')
                        *> skipMany spaceOrTab
      breakChar *> breakChar *> breakChar *> skipMany breakChar
      lookahead endline
      addContainer thematicBreakSpec ()
  , blockContinue = \_ -> pure False
  , blockContainsBlock = Nothing
  , blockContainsLines = True
  , blockClose = pure
  , blockFinalize = const thematicBreak
  }

data SectionData =
  SectionData Int (Maybe ByteString)
  deriving (Show, Eq, Ord, Typeable)

data HeadingData =
  HeadingData Int Inlines
  deriving (Show, Eq, Ord, Typeable)

headingSpec :: BlockSpec s
headingSpec =
  BlockSpec
  { blockName = "Heading"
  , blockType = Normal
  , blockStart = try $ do
      lev <- length <$> some (asciiChar' '#')
      lookahead ws
      skipMany spaceOrTab
      closeContainingSections lev
      addContainer sectionSpec $ SectionData lev Nothing
      addContainer headingSpec $ HeadingData lev mempty
  , blockContinue = \container -> try $ do
       do skipMany spaceOrTab
          let HeadingData lev _ = getContainerData container
          (True <$ try (do lev' <- length <$> some (asciiChar' '#')
                           guard (lev' == lev)
                           skipMany spaceOrTab))
            <|> (False <$ do
                    lookahead (asciiChar' '#' <|> endline <|> eof))
            <|> (True <$ skipMany spaceOrTab)
  , blockContainsBlock = Nothing
  , blockContainsLines = True
  , blockClose = \container -> do
      ils <- parseTextLines container
      let HeadingData lev _ = getContainerData container
      pure $ container{ containerData = toDyn $ HeadingData lev ils }
  , blockFinalize = \container ->
      let HeadingData lev title = getContainerData container
      in  heading lev title
  }

data CodeBlockData =
  CodeBlockData ByteString ByteString (Maybe Int)
  deriving (Eq, Show, Ord, Typeable)

codeBlockSpec :: BlockSpec s
codeBlockSpec =
  BlockSpec
  { blockName = "CodeBlock"
  , blockType = Normal
  , blockStart = try $ do
      indent <- getIndent
      ticks <- byteStringOf $ asciiChar' '`' *> asciiChar' '`' *> skipSome (asciiChar' '`')
      skipMany spaceOrTab
      lang <- try (byteStringOf
                    (skipSome $ skipSatisfy' (\c -> c /= '`' && not (isSpace c)))
                    <* skipMany spaceOrTab)
             <|> pure ""
      lookahead endline
      addContainer codeBlockSpec (CodeBlockData ticks lang indent)
  , blockContinue = \container -> do
      let CodeBlockData ticks _ indent = getContainerData container
      try (do skipMany spaceOrTab
              byteString ticks
              skipMany (asciiChar' '`')
              skipMany spaceOrTab
              lookahead endline
              pure False)
        <|> (True <$ gobbleSpaceToIndent indent)
  , blockContainsBlock = Nothing
  , blockContainsLines = True
  , blockClose = pure
  , blockFinalize = \container ->
      let CodeBlockData _ lang _ = getContainerData container
      -- drop first line which should be empty
          bs = fold (Seq.drop 1 $ containerText container)
      in  case B8.uncons lang of
            Just ('=', fmt) -> rawBlock (Format fmt) bs
            _ -> codeBlock lang bs
  }

data DivData =
  DivData ByteString ByteString
  deriving (Show, Eq, Ord, Typeable)

divSpec :: BlockSpec s
divSpec =
  BlockSpec
  { blockName = "Div"
  , blockType = Normal
  , blockStart = try $ do
      colons <- byteStringOf $ asciiChar' ':' *> asciiChar' ':' *> skipSome (asciiChar' ':')
      skipMany spaceOrTab
      label <- byteStringOf $ skipMany $ skipSatisfy' (not . isSpace)
      skipMany spaceOrTab
      lookahead endline
      addContainer divSpec (DivData colons label)
  , blockContinue = \container -> try (do
      tip <- getTip
      -- see jgm/djot.js#109
      guard $ blockName (containerSpec tip) /= "CodeBlock"
      let DivData colons _ = getContainerData container
      skipMany spaceOrTab
      byteString colons
      skipMany (asciiChar' ':')
      skipMany spaceOrTab
      lookahead endline
      pure False) <|> pure True
  , blockContainsBlock = Just Normal
  , blockContainsLines = False
  , blockClose = pure
  , blockFinalize = \container ->
      let DivData _ label = getContainerData container
      -- drop first line which should be empty
          bls = finalizeChildren container
      in  (if B.null label
              then id
              else addAttr (Attr [("class", label)])) $ div bls
  }

attrSpec :: BlockSpec s
attrSpec =
  BlockSpec
  { blockName = "Attributes"
  , blockType = Normal
  , blockStart = try $ do
      lookahead (asciiChar' '{')
      -- isolate pAttributes to content before newline
      FP.Span s e <- lookahead $ spanOf
                       (skipMany $ skipSatisfy' (\c -> c /= '\r' && c /= '\n'))
      let linebytes = unPos s - unPos e
      attr <- isolate linebytes pAttributes
      modifyP $ \st -> st{ psAttributes = psAttributes st <> attr }
      addContainer attrSpec ()
  , blockContinue = \_ -> pure False
  , blockContainsBlock = Nothing
  , blockContainsLines = False
  , blockClose = pure
  , blockFinalize = const mempty
  }

referenceDefinitionSpec :: BlockSpec s
referenceDefinitionSpec =
  BlockSpec
  { blockName = "ReferenceDefinition"
  , blockType = Normal
  , blockStart = try $ do
      asciiChar' '['
      lookahead (skipSatisfy' (/= '^')) -- footnote
      label <- byteStringOf
                (some (skipSatisfy' (\c -> c /= ']' && c /= '\n')))
      asciiChar' ']'
      asciiChar' ':'
      skipMany spaceOrTab
      addContainer referenceDefinitionSpec (normalizeLabel label)
  , blockContinue = \container ->
      (True <$ try (skipSome spaceOrTab `notFollowedBy` endline))
      <|>
      -- if we're done, we update reference map:
      (do let label = getContainerData container
          let attr = containerAttr container
          let dest = B.filter (> 32) . fold $ containerText container
          modifyP $ \st ->
            st{ psReferenceMap = insertReference label (dest, attr)
                                     (psReferenceMap st) }
          pure False)
  , blockContainsBlock = Nothing
  , blockContainsLines = True
  , blockClose = pure
  , blockFinalize = const mempty
  }

data FootnoteData =
  FootnoteData (Maybe Int) ByteString
  deriving (Show, Ord, Eq, Typeable)

footnoteSpec :: BlockSpec s
footnoteSpec =
  BlockSpec
  { blockName = "Footnote"
  , blockType = Normal
  , blockStart = try $ do
      ind <- getIndent
      asciiChar' '['
      asciiChar' '^'
      label <- byteStringOf
                (some (skipSatisfy' (\c -> c /= ']' && c /= '\n')))
      asciiChar' ']'
      asciiChar' ':'
      skipMany spaceOrTab
      addContainer footnoteSpec $ FootnoteData ind (normalizeLabel label)
  , blockContinue = \container -> try (do
      skipMany spaceOrTab
      curind <- getIndent
      let FootnoteData ind _ = getContainerData container
      guard (curind > ind) <|> lookahead pBlankLine
      pure True) <|> pure False
  , blockContainsBlock = Just Normal
  , blockContainsLines = True
  , blockClose = \container -> do
      let FootnoteData _ label = getContainerData container
      let bls = finalizeChildren container
      modifyP $ \st -> st{ psNoteMap = insertNote label bls (psNoteMap st) }
      pure container
  , blockFinalize = const mempty
  }


paraSpec :: BlockSpec s
paraSpec =
  BlockSpec
  { blockName = "Para"
  , blockType = Normal
  , blockStart = fails pBlankLine *> addContainer paraSpec (mempty :: Inlines)
  , blockContinue = \_ -> (False <$ lookahead pBlankLine) <|> pure True
  , blockContainsBlock = Nothing
  , blockContainsLines = True
  , blockClose = \container -> do
      ils <- parseTextLines container
      pure $ container{ containerData = toDyn ils }
  , blockFinalize = para . getContainerData
  }

parseTextLines :: Container s -> P s Inlines
parseTextLines = either err pure . parseInlines . containerText

emptyContainer :: Container s
emptyContainer =
  Container { containerSpec = docSpec
            , containerChildren = mempty
            , containerText = mempty
            , containerStartLine = 1
            , containerEndLine = 0
            , containerData = toDyn ()
            , containerAttr = mempty
            }

data Container s =
  Container
  { containerSpec      :: BlockSpec s
  , containerChildren  :: Seq (Container s)
  , containerText :: Seq ByteString
  , containerStartLine :: Int
  , containerEndLine   :: Int
  , containerData :: Dynamic
  , containerAttr :: Attr
  }

data PState s =
  PState
  { psParseOptions :: ParseOptions
  , psContainerStack :: NonEmpty (Container s)
  , psCurrentLine :: Int
  , psCurrentLineStart :: Pos
  , psReferenceMap :: ReferenceMap
  , psNoteMap :: NoteMap
  , psAttributes :: Attr
  , psIds :: Set ByteString
  }

type P s = ParserST s (STRef s (PState s)) String

getsP :: (PState s -> a) -> P s a
getsP f = do
  ref <- ask
  liftST $ f <$> readSTRef ref

modifyP :: (PState s -> PState s) -> P s ()
modifyP f = do
  ref <- ask
  liftST (modifySTRef' ref f)

pDoc :: P s Doc
pDoc = do
  bls <- pBlocks
  notemap <- getsP psNoteMap
  refmap <- getsP psReferenceMap
  pure $ Doc{ docBlocks = bls
            , docFootnotes = notemap
            , docReferences = refmap }

pBlocks :: P s Blocks
pBlocks = processLines >> finalizeDocument

incrementCurrentLine :: P s ()
incrementCurrentLine = do
  pos <- getPos
  modifyP $ \st -> st{ psCurrentLine = psCurrentLine st + 1
                     , psCurrentLineStart = pos }

-- | Return value is True if all continuations match.
checkContinuations :: NonEmpty (Container s) -> P s Bool
checkContinuations = go . reverse . NonEmpty.toList
 where
   go [] = return True
   go (c:cs) = do continue <- blockContinue (containerSpec c) c <|> pure False
                  if continue
                     then go cs
                     else False <$ -- close len (c:cs) containers
                          replicateM_ (length (c:cs)) closeCurrentContainer

processLines :: P s ()
processLines = void $ many processLine

processLine :: P s ()
processLine = do
  incrementCurrentLine
  -- check continuations for open containers and close any that don't match
  containers <- getsP psContainerStack
  allContainersMatch <- checkContinuations containers

  -- check for new container starts and open if needed
  newContainersAdded <- tryContainerStarts

  isBlank <- (True <$ lookahead pBlankLine) <|> pure False

  -- determine if we have a lazy line
  let isLazy = not (allContainersMatch || newContainersAdded || isBlank) &&
               blockName (containerSpec (NonEmpty.head containers)) == "Para"

  when isLazy $ -- restore original containers
     modifyP (\st -> st{ psContainerStack = containers })

  tip <- getTip

  when (not isBlank &&
        blockContainsBlock (containerSpec tip) == Just Normal) $ do
    -- add a paragraph container
    skipMany spaceOrTab
    blockStart paraSpec

  restOfLine <- pLine
  -- if current container is a line container, add remainder of line
  modifyContainers $
    \(c :| rest) ->
       if blockContainsLines (containerSpec c)
          then c{ containerText = containerText c Seq.|> restOfLine } :| rest
          else c :| rest

-- | Return value is True if new containers were added.
tryContainerStarts :: P s Bool
tryContainerStarts = do
  (c :| _) <- getsP psContainerStack
  case blockContainsBlock (containerSpec c) of
    Just bt ->
      (do skipMany spaceOrTab
          msum [blockStart sp | sp <- specs
                              -- don't allow tables to contain anything but captions
                              , (bt /= CaptionBlock || blockType sp == CaptionBlock)
                              ]
          True <$ tryContainerStarts)
       <|> pure False
    _ -> pure False

-- | Close and finalize containers, returning Blocks.
finalizeDocument :: P s Blocks
finalizeDocument = do
  cs <- getsP psContainerStack
  case cs of
    c :| [] -> pure $ finalize c
    _ -> closeCurrentContainer >> finalizeDocument

-- | Close container and add to parent container.
closeCurrentContainer :: P s ()
closeCurrentContainer = do
  cs <- getsP psContainerStack
  cs' <- case cs of
           _ :| [] -> err "Attempted to close root document container"
           c :| rest -> do
             case containerAttr c of
               Attr as | Just ident <- lookup "id" as
                 -> modifyP $ \st -> st{ psIds = Set.insert ident (psIds st) }
               _ -> pure ()
             c' <- blockClose (containerSpec c) c
             pure (c':|rest)
  curline <- getsP psCurrentLine
  case cs' of
    c :| (d:rest) -> modifyP $
        \st -> st{ psContainerStack =
                   d{ containerChildren = containerChildren d Seq.|>
                        c{ containerEndLine = curline - 1 } } :| rest }
    _ :| [] -> err "Attempted to close root document container"

modifyContainers :: (NonEmpty (Container s)
                 -> NonEmpty (Container s)) -> P s ()
modifyContainers f =
  modifyP $ \st -> st{ psContainerStack = f (psContainerStack st) }

addContainer :: Typeable a => BlockSpec s -> a -> P s ()
addContainer bspec bdata = do
  curline <- getsP psCurrentLine
  attr <- getsP psAttributes
  let newcontainer = emptyContainer { containerSpec = bspec
                                    , containerStartLine = curline
                                    , containerData = toDyn bdata
                                    , containerAttr = attr }
  unless (blockName bspec == "Attributes") $
    modifyP $ \st -> st{ psAttributes = mempty }
  closeInappropriateContainers bspec
  modifyContainers (newcontainer NonEmpty.<|)

closeInappropriateContainers :: BlockSpec s -> P s ()
closeInappropriateContainers spec = do
  -- close containers until we get one that can accept this type of container
  cs <- getsP psContainerStack
  case cs of
    c :| _
      | blockContainsBlock (containerSpec c) == Just (blockType spec) ->
              pure ()
      | otherwise -> closeCurrentContainer *> closeInappropriateContainers spec


{-# INLINE pLine #-}
pLine :: P s ByteString
pLine = byteStringOf $
  (skipSome (skipSatisfy' (\c -> c /= '\n' && c /= '\r')) <* optional_ endline)
    <|> endline

{-# INLINE pBlankLine #-}
pBlankLine :: P s ()
pBlankLine = try $
  skipMany spaceOrTab *> (endline <|> eof)

{-# INLINE addAttr #-}
addAttr :: Attr -> Blocks -> Blocks
addAttr attr (Blocks nodes) =
  Blocks (fmap (\(Node attr' bs) -> Node (attr' <> attr) bs) nodes)

finalize :: Container s -> Blocks
finalize cont =
  addAttr (containerAttr cont) $ blockFinalize (containerSpec cont) cont

finalizeChildren :: Container s -> Blocks
finalizeChildren = foldMap finalize . containerChildren

-- Gobble as much space as possible up to indent.
gobbleSpaceToIndent :: Maybe Int -> P s ()
gobbleSpaceToIndent indent = do
  curindent <- getIndent
  case (curindent, indent) of
    (Just cur, Just ind) | cur < ind ->
         optional_ (spaceOrTab *> gobbleSpaceToIndent indent)
    _ -> pure ()

{-# INLINE getTip #-}
-- Get tip of container stack.
getTip :: P s (Container s)
getTip = NonEmpty.head <$> getsP psContainerStack

{-# INLINE getContainerData #-}
getContainerData :: Typeable a => Container s -> a
getContainerData cont =
   case fromDynamic (containerData cont) of
      Nothing -> error $ blockName (containerSpec cont) <> " missing data"
      Just dat -> dat

closeContainingSections :: Int -> P s ()
closeContainingSections lev = do
  tip <- getTip
  case fromDynamic (containerData tip) of
    Just (SectionData lev' _) | lev' >= lev ->
      closeCurrentContainer >>
      closeContainingSections lev
    _ -> pure ()

toIdentifier :: ByteString -> ByteString
toIdentifier bs =
  if null parts
     then "sec"
     else strToUtf8 $ intercalate "-" parts
 where
   isSym = (`elem` ("][~!@#$%^&*(){}`,.<>\\|=+/" :: [Char]))
   parts = words $ map (\c -> if isSym c then ' ' else c) $ utf8ToStr bs
