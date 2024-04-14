{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE BinaryLiterals #-}
module Djot.Inlines
  ( parseInlines
  , parseTableCells
  )
where

import Data.Char (isAscii, isLetter, isAlphaNum, isSymbol, isPunctuation)
import Control.Monad (guard, when, mzero)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Djot.Parse
import Djot.Options (ParseOptions(..), SourcePosOption(..))
import Djot.Attributes (pAttributes)
import Djot.AST
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import Data.Foldable as F
import Control.Applicative
import Data.Maybe (fromMaybe)

-- import Debug.Trace

{-# INLINE isSpecial #-}
isSpecial :: Char -> Bool
isSpecial c = c == '[' || c == ']' || c == '<' || c == '>' ||
              c == '$' || c == '!' || c == '{' || c == '}' || c == ':' ||
              c == '=' || c == '+' || c == '-' || c == '^' || c == '~' ||
              c == '*' || c == '_' || c == '\''|| c == '"' || c == '.' ||
              c == '|' || c == '`' || c == '\\'|| c == '\n' || c == '\r'

parseInlines :: ParseOptions -> Seq Chunk -> Either String Inlines
parseInlines opts chunks = do
  case parse (pInlines <* eof) ParserState{ mode = NormalMode
                                          , activeDelims = mempty
                                          , options = opts }
       (toList (stripEndChunks chunks)) of
    Just ils -> Right ils
    Nothing -> Left $ "parseInlines failed on input: "
                     <> show (foldMap chunkBytes chunks)

parseTableCells :: ParseOptions -> Chunk -> Either String [Inlines]
parseTableCells opts chunk = do
  case parse (asciiChar '|'
                 *> some (removeFinalWs <$> pInlines <* asciiChar '|')
                 <* skipMany ws
                 <* eof)
        ParserState{ mode = TableCellMode
                   , activeDelims = mempty
                   , options = opts }
       [chunk] of
    Just cells -> Right cells
    Nothing -> Left $ "parseTableCells failed on input: " <> show chunk

removeFinalWs :: Inlines -> Inlines
removeFinalWs (Many ils) = Many $
  case Seq.viewr ils of
    rest Seq.:> Node pos attr (Str bs)
      | B8.takeEnd 1 bs == " "
        -> case B8.dropWhileEnd (== ' ') bs of
             "" -> rest
             bs' -> rest Seq.|> Node pos attr (Str bs')
    _ -> ils

data InlineParseMode =
  NormalMode | TableCellMode
  deriving (Show, Ord, Eq)

data ParserState =
  ParserState
  { mode :: InlineParseMode
  , activeDelims :: Set Delim
  , options :: ParseOptions }
  deriving (Show)

data Delim = Delim Bool Char
  deriving (Show, Ord, Eq)

type P = Parser ParserState

pInlines :: P Inlines
pInlines = skipMany ws *> (mconcat <$> many pInline)

pInline :: P Inlines
pInline = do
  sline <- sourceLine
  scol <- sourceColumn
  res <- pInline'
  opts <- options <$> getState
  (case sourcePositions opts of
     AllSourcePos -> do
       eline <- sourceLine
       ecol <- sourceColumn
       pure $ addPos (Pos sline scol eline (ecol - 1)) <$> res
     _ -> pure res) >>= pOptionalAttributes

pOptionalAttributes :: Inlines -> P Inlines
pOptionalAttributes (Many ils) = pAddAttributes (Many ils) <|> pure (Many ils)

pAddAttributes :: Inlines -> P Inlines
pAddAttributes (Many ils) = do
  attr <- mconcat <$> some pAttributes
  pure $
    case attr of
      Attr [] -> Many ils
      _ -> case Seq.viewr ils of
             Seq.EmptyR -> mempty
             ils' Seq.:> Node pos attr' (Str bs)
               | B8.any isWs bs ->
               -- attach attribute to last word
               let (front, lastword) = B8.breakEnd isWs bs
               in if B.null lastword
                     then Many ils  -- ignore attr after whitespace
                     else
                       let (pos1, pos2) =
                               case pos of
                                 NoPos -> (NoPos, NoPos)
                                 Pos sl sc el ec ->
                                   let frontlen = B8.length
                                        (B8.filter (\c -> c < '\128' || c >= '\192') front)
                                   in (Pos sl sc sl (sc + frontlen),
                                       Pos sl (sc + frontlen + 1) el ec)
                       in Many (ils' Seq.|>
                                     Node pos1 attr' (Str front) Seq.|>
                                     Node pos2 attr (Str lastword))
             ils' Seq.:> Node pos attr' il ->
               Many (ils' Seq.|> Node pos (attr' <> attr) il)

pInline' :: P Inlines
pInline' = do
  (do c <- lookahead (satisfyByte isSpecial)
      fails pCloser
      (case c of
          '\\' -> pEscaped
          '[' -> pFootnoteReference
             <|> pLinkOrSpan
          '<' -> pAutolink
          '!' -> pImage
          '_' -> pEmph
          '*' -> pStrong
          '^' -> pSuperscript
          '~' -> pSubscript
          '{' -> pEmph
             <|> pStrong
             <|> pHighlight
             <|> pInsert
             <|> pDelete
             <|> pSuperscript
             <|> pSubscript
             <|> pDoubleQuote
             <|> pSingleQuote
             <|> (mempty <$ pAttributes)
          '`' -> pVerbatim
          ':' -> pSymbol
          '$' -> pMath
          '"' -> pDoubleQuote
          '\'' -> pSingleQuote
          '-' -> pHyphens
          '.' -> pEllipses
          '\n' -> pSoftBreak
          _ -> mzero)
        <|> pSpecial
       ) <|> pWords

pSpecial :: P Inlines
pSpecial = do
  st <- getState
  c <- satisfyByte (case mode st of
                       TableCellMode -> \d -> isSpecial d && d /= '|'
                       _ -> isSpecial)
  if c == '\r'
     then pure mempty
     else pure $ str $ B8.singleton c

pWords :: P Inlines
pWords = str <$> byteStringOf (skipSome (skipSatisfyByte (not . isSpecial)))

pEscaped :: P Inlines
pEscaped = do
  asciiChar '\\'
  c <- satisfyByte (\d ->
          isAscii d &&
            (isSymbol d || isPunctuation d || d == ' ' || d == '\t'))
         <|> ('\n' <$ endline)
         <|> pure '\\'
  case c of
    '\n' -> hardBreak <$ skipMany spaceOrTab
    _ | c == ' ' || c == '\t' -> pHardBreak
                             <|> if c == ' '
                                    then pure nonBreakingSpace
                                    else pure $ str "\\\t"
    _ -> pure $ str $ B8.singleton c

pHardBreak :: P Inlines
pHardBreak = do -- assumes we've parsed \ already
  skipMany spaceOrTab
  endline
  skipMany spaceOrTab
  pure hardBreak

pSoftBreak :: P Inlines
pSoftBreak = do
  endline
  skipMany spaceOrTab
  (mempty <$ eof) <|> pure softBreak

pSymbol :: P Inlines
pSymbol = do
  asciiChar ':'
  bs <- byteStringOf $ skipSome (skipSatisfyByte
                                    (\c -> c == '+' || c == '-' ||
                                           c == '_' ||
                                         (isAscii c && isAlphaNum c)))
  asciiChar ':'
  pure $ symbol bs

pMath :: P Inlines
pMath = do
  asciiChar '$'
  mathStyle <- (DisplayMath <$ asciiChar '$') <|> pure InlineMath
  verb <- pVerbatim
  case unMany verb of
         [Node pos attr (Verbatim bs)] ->
              pure $ Many $ Seq.singleton $ Node pos attr (Math mathStyle bs)
         _ -> pure $ (case mathStyle of
                        DisplayMath -> str "$$"
                        _ -> str "$") <> verb

{-# INLINE bracesRequired #-}
bracesRequired :: Char -> Bool
bracesRequired '=' = True
bracesRequired '+' = True
bracesRequired '-' = True
bracesRequired _ = False

pCloser :: P ()
pCloser = do
  delims <- activeDelims <$> getState
  if Set.null delims
     then mzero
     else do
       openerHadBrace <- asum $
         map (\(Delim hadBrace c) -> hadBrace <$ asciiChar c) (F.toList delims)
       mblastc <- peekBack
       let afterws = maybe True isWs mblastc
       when ( afterws || openerHadBrace ) $ asciiChar '}'

pEmph, pStrong, pSuperscript, pSubscript :: P Inlines
pEmph = pBetween '_' emph
pStrong = pBetween '*' strong
pSuperscript = pBetween '^' superscript
pSubscript = pBetween '~' subscript

pHighlight, pInsert, pDelete :: P Inlines
pHighlight = pBetween '=' highlight
pInsert = pBetween '+' insert
pDelete = pBetween '-' delete

pBetween :: Char -> (Inlines -> Inlines) -> P Inlines
pBetween c constructor = do
  let starter leftBrace = do
        case leftBrace of
          False
            | bracesRequired c -> mzero
            | otherwise -> asciiChar c `notFollowedBy`
                                (ws <|> asciiChar '}')
          True -> asciiChar c `notFollowedBy` asciiChar '}'
  let ender leftBrace = do
        mblastc <- peekBack
        let afterws = maybe True isWs mblastc
        asciiChar c
        if leftBrace
           then asciiChar '}'
           else guard (not afterws) `notFollowedBy` asciiChar '}'
  leftBrace <- (True <$ asciiChar '{') <|> pure False
  starterBs <- (if leftBrace then ("{" <>) else id) <$>
                 byteStringOf (starter leftBrace) `notFollowedBy` pAttributes
                 -- don't let *{.foo} start emphasis, for example
  oldActiveDelims <- activeDelims <$> getState
  updateState $ \st -> st{ activeDelims = Set.insert (Delim leftBrace c)
                                             (activeDelims st) }
  firstIl <- pInline <|> pBetween c constructor -- to allow stacked cases like '**hi**'
  restIls <- many pInline
  let ils = mconcat (firstIl:restIls)
  updateState $ \st -> st{ activeDelims = oldActiveDelims }
  (constructor ils <$ ender leftBrace) <|> pure (str starterBs <> ils)

pTicks :: P Int
pTicks = do
  sp <- getOffset
  skipSome (asciiChar '`')
  ep <- getOffset
  pure (ep - sp)

pVerbatim :: P Inlines
pVerbatim = do
  numticks <- pTicks
  let ender = pTicks >>= guard . (== numticks)
  let content = skipSome (skipSatisfyByte (\c -> c /= '`' && c /= '\\')) <|>
                 (asciiChar '\\' <* anyChar) <|>
                 (fails ender *> skipSome (asciiChar '`'))
  bs <- trimSpaces <$> byteStringOf (skipMany content) <* (ender <|> eof)
  (rawInline <$> pRawAttribute <*> pure bs) <|> pure (verbatim bs)

-- Trim a leading space if first non-space character is `,
-- and similarly for trailing space/last non-space.
trimSpaces :: ByteString -> ByteString
trimSpaces = trimSpaceFront . trimSpaceBack
 where
   trimSpaceFront bs =
        case B8.span (== ' ') bs of
          (a, b) | B8.take 1 b == "`"
                 , not (B8.null a)
            -> B8.drop 1 bs
          _ -> bs
   trimSpaceBack bs =
        case B8.spanEnd (== ' ') bs of
          (a, b) | B8.takeEnd 1 a == "`"
                 , not (B8.null b)
            -> B8.dropEnd 1 bs
          _ -> bs

pRawAttribute :: P Format
pRawAttribute = do
  byteString "{="
  fmt <- Format <$>
             byteStringOf (skipMany (skipSatisfyByte (\c -> c /= '}' &&
                                                 not (isWs c))))
  asciiChar '}'
  pure fmt

pFootnoteReference :: P Inlines
pFootnoteReference = do
  asciiChar '['
  asciiChar '^'
  label <- byteStringOf $ skipMany $
             skipSatisfyByte (\c -> c /= ']' && not (isWs c))
  asciiChar ']'
  pure $ footnoteReference label

-- returns Left with parsed content if no ] has been reached, otherwise Right
-- with inner contents.
pBracketed :: P (Either Inlines Inlines)
pBracketed = do
  let starter = asciiChar '['
  let ender = asciiChar ']'
  starterBs <- byteStringOf starter
  oldActiveDelims <- activeDelims <$> getState
  updateState $ \st -> st{ activeDelims = Set.insert (Delim False ']') (activeDelims st) }
  ils <- mconcat <$> many pInline
  updateState $ \st -> st{ activeDelims = oldActiveDelims }
  (Right ils <$ ender) <|> pure (Left (str starterBs <> ils))

pImage :: P Inlines
pImage = do
  asciiChar '!'
  (res, raw) <- withByteString pBracketed
  case res of
    Left ils -> pure (str "!" <> ils)
    Right ils ->
            ((str "!" <>) <$> pAddAttributes (span_ ils))
        <|> (image ils <$> (pDestination <|> pReference raw))
        <|> pure (str "![" <> ils <> str "]")

pAutolink :: P Inlines
pAutolink = do
  asciiChar '<'
  res <- byteStringOf $ skipSome $ skipSatisfyByte (\c -> c /= '>' && c /= '<')
  asciiChar '>'
  let url = B8.filter (\c -> c /= '\n' && c /= '\r') res
  case B8.find (\c -> c == '@' || c == ':' || c == '.') url of
    Just '@' -> pure $ emailLink url
    Just _ -> pure $ urlLink url
    Nothing -> mzero

pLinkOrSpan :: P Inlines
pLinkOrSpan = do
  (res, raw) <- withByteString pBracketed
  case res of
    Left ils -> pure ils
    Right ils ->
            (span_ ils <$ lookahead (asciiChar '{'))
        <|> (link ils <$> (pDestination <|> pReference raw))
        <|> pure (str "[" <> ils <> str "]")

-- We allow balanced pairs of parens inside.
pDestination :: P Target
pDestination = do
  asciiChar '('
  res <- byteStringOf $ pInBalancedParens 0
  asciiChar ')'
  pure $ Direct (snd (handleEscapesAndNewlines res))
 where
  handleEscapesAndNewlines = B8.foldl' go (False, mempty)
  go (esc, bs) '\n' = (esc, bs)
  go (esc, bs) '\r' = (esc, bs)
  go (True, bs) c = (False, bs `B8.snoc` c)
  go (False, bs) '\\' = (True, bs)
  go (False, bs) c = (False, bs `B8.snoc` c)

pInBalancedParens :: Int -> P ()
pInBalancedParens nestlevel =
  (guard (nestlevel == 0) <* lookahead (asciiChar ')')) <|>
    do lev <-   (nestlevel <$ (fails pCloser *>
                               -- but see https://github.com/jgm/djot/discussions/247
                               skipSatisfyByte
                                 (\c -> c /= '(' && c /= ')' && c /= '\\')))
            <|> (nestlevel <$ (asciiChar '\\' <* anyChar))
            <|> ((nestlevel + 1) <$ asciiChar '(')
            <|> ((nestlevel - 1) <$ asciiChar ')')
       pInBalancedParens lev

pReference :: ByteString -> P Target
pReference rawDescription = do
  asciiChar '['
  bs <- byteStringOf $ pAtMost 400 $ skipSatisfyByte
           (\c -> c /= '[' && c /= ']')
  asciiChar ']'
  let label = normalizeLabel $
              if B.null bs
                 then B.drop 1 $ B.dropEnd 1
                               $ B8.filter (/= '\n') rawDescription
                 else bs
  pure $ Reference label

pAtMost :: Int -> P () -> P ()
pAtMost n pa = optional_ (pa *> when (n > 0) (pAtMost ( n - 1 ) pa))

pOpenDoubleQuote :: P ()
pOpenDoubleQuote = do
  lbrace <- (True <$ asciiChar '{') <|> pure False
  asciiChar '"'
  rbrace <- (True <$ asciiChar '}') <|> pure False
  guard $ lbrace || not rbrace

pCloseDoubleQuote :: P ()
pCloseDoubleQuote = do
  mblastc <- peekBack
  let whitespaceBefore = maybe True isWs mblastc
  lbrace <- (True <$ asciiChar '{') <|> pure False
  asciiChar '"'
  rbrace <- (True <$ asciiChar '}') <|> pure False
  whitespaceAfter <- (True <$ lookahead (skipSatisfyByte isWs)) <|> pure False
  guard $ not lbrace && (rbrace || not whitespaceBefore || whitespaceAfter)

pDoubleQuote :: P Inlines
pDoubleQuote = (do
  pOpenDoubleQuote
  contents <- mconcat <$> many (fails pCloseDoubleQuote *> pInline)
  (doubleQuoted contents <$ pCloseDoubleQuote)
    <|> pure (openDoubleQuote <> contents))
 <|> (closeDoubleQuote <$ asciiChar '"')

openDoubleQuote, closeDoubleQuote :: Inlines
openDoubleQuote = str "\226\128\156" -- utf8 0x201C
closeDoubleQuote = str "\226\128\157" -- utf8 0x201D

pOpenSingleQuote :: P ()
pOpenSingleQuote = do
  lastc <- fromMaybe '\n' <$> peekBack
  let openContext = lastc == '\t' || lastc == '\r' ||
                    lastc == '\n' || lastc == ' ' ||
                    lastc == '"' || lastc == '\'' ||
                    lastc == '(' || lastc == '[' || lastc == '\0'
  lbrace <- (True <$ asciiChar '{') <|> pure False
  asciiChar '\''
  rbrace <- (True <$ asciiChar '}') <|> pure False
  guard $ lbrace || (openContext && not rbrace)

pCloseSingleQuote :: P ()
pCloseSingleQuote = do
  mblastc <- peekBack
  let whitespaceBefore = maybe True isWs mblastc
  lbrace <- (True <$ asciiChar '{') <|> pure False
  asciiChar '\''
  rbrace <- (True <$ asciiChar '}') <|> pure False
  letterAfter <- (True <$ lookahead (satisfy isLetter)) <|> pure False
  guard $ not lbrace && (rbrace || not (whitespaceBefore || letterAfter))

pSingleQuote :: P Inlines
pSingleQuote = (do
  pOpenSingleQuote
  contents <- mconcat <$> many (fails pCloseSingleQuote *> pInline)
  (singleQuoted contents <$ pCloseSingleQuote)
    <|> pure (closeSingleQuote <> contents))
 <|> (closeSingleQuote <$ asciiChar '\'')

closeSingleQuote :: Inlines
closeSingleQuote = str "\226\128\153" -- utf8 0x2019

pHyphens :: P Inlines
pHyphens = do
  numHyphens <- length <$> some hyphen
  pure $ str $ go numHyphens
    where
     emdash = "\226\128\148" -- utf8 0x2014
     endash = "\226\128\147" -- utf8 0x2013
     hyphen = asciiChar '-' `notFollowedBy` asciiChar '}'
     go 1 = "-"
     go n | n `mod` 3 == 0
          = mconcat (replicate (n `Prelude.div` 3) emdash)
          | n `mod` 2 == 0
          = mconcat (replicate (n `Prelude.div` 2) endash)
          | n `mod` 3 == 2
          = mconcat (replicate (n `Prelude.div` 3) emdash) <> endash
          | n `mod` 3 == 1
          = mconcat (replicate (n `Prelude.div` 3 - 1) emdash) <>
              endash <> endash
          | otherwise
          = emdash <> go (n - 3)

pEllipses :: P Inlines
pEllipses = str "\226\128\166" {- utf8 0x2026 -} <$ byteString "..."

stripEndChunks :: Seq Chunk -> Seq Chunk
stripEndChunks cs =
  case Seq.viewr cs of
    initial Seq.:> c ->
      initial Seq.|> c{ chunkBytes = B8.dropWhileEnd isWs (chunkBytes c) }
    _ -> cs
