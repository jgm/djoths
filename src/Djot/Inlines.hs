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
import Control.Monad (guard, when, unless, mzero)
import Data.Sequence (Seq)
import Data.Bits (testBit, setBit, clearBit, (.&.), (.|.))
import qualified Data.Sequence as Seq
import qualified Djot.FlatParse as FP
import Djot.Parse
import Djot.Attributes (pAttributes)
import Djot.AST
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import Data.Foldable as F
import Control.Applicative
import Data.Maybe (fromMaybe)

-- TODO:
-- [ ] withSourcePos combinator on pInline

--

{-# INLINE isSpecial #-}
isSpecial :: Char -> Bool
isSpecial c = c == '[' || c == ']' || c == '<' || c == '>' ||
              c == '$' || c == '!' || c == '{' || c == '}' || c == ':' ||
              c == '=' || c == '+' || c == '-' || c == '^' || c == '~' ||
              c == '*' || c == '_' || c == '\''|| c == '"' || c == '.' ||
              c == '|' || c == '`' || c == '\\'|| c == '\n' || c == '\r'

parseInlines :: Seq ByteString -> Either String Inlines
parseInlines lns = do
  let inp = B8.dropWhileEnd isWs $ fold lns
  case parse pInlines ParserState{ mode = NormalMode
                                 , activeDelims = 0 } inp of
    Just (off, ils)
      | off >= B8.length inp -> Right ils
      | otherwise -> Left $ "parseInlines failed to consume input: " <>
                              show (B8.drop off inp)
    Nothing -> Left $ "parseInlines failed on input: " <> show inp

parseTableCells :: ByteString -> Either String [Inlines]
parseTableCells inp' = do
  let inp = B8.dropWhileEnd isWs inp'
  case parse (many (removeFinalWs <$> pInlines <* asciiChar '|'))
        ParserState{ mode = TableCellMode
                   , activeDelims = 0 } inp of
    Just (off, cells)
      | off >= B8.length inp -> Right cells
      | otherwise -> Left $ "parseTableCells failed to consume input: " <>
                              show (B8.drop off inp)
    Nothing -> Left $ "parseTableCells failed on input: " <> show inp

removeFinalWs :: Inlines -> Inlines
removeFinalWs (Many ils) = Many $
  case Seq.viewr ils of
    rest Seq.:> Node attr (Str bs)
      | B8.takeEnd 1 bs == " "
        -> case B8.dropWhileEnd (== ' ') bs of
             "" -> rest
             bs' -> rest Seq.|> Node attr (Str bs')
    _ -> ils

data InlineParseMode =
  NormalMode | TableCellMode
  deriving (Show, Ord, Eq)

data ParserState =
  ParserState
  { mode :: InlineParseMode
  , activeDelims :: Int }
  deriving (Show, Ord, Eq)

type P = Parser ParserState

-- for mutable state, can do something like:
-- type P s = ParserST s (STRef s PState) String

pInlines :: P Inlines
pInlines = skipMany ws *> (consolidate . mconcat <$> many pInline)

{-# INLINE consolidate #-}
-- This is need so that we don't have Str "*", Str "x"
-- so that only the "x" gets following attributes.
-- e.g.  *x{.foo}
consolidate :: Inlines -> Inlines
consolidate (Many ils') = Many (foldl' go mempty ils')
 where
   go ils il@(Node attr (Str s)) =
     case Seq.viewr ils of
              start Seq.:> Node (Attr []) (Str s')
                | (sa, sb) <- B8.spanEnd (not . isWs) s'
                , not (B8.null sb)
                -> if B8.null sa
                      then start Seq.|> Node attr (Str (sb <> s))
                      else start Seq.|> Node (Attr []) (Str sa)
                                 Seq.|> Node attr (Str (sb <> s))
              _ -> ils Seq.|> il
   go ils il = ils Seq.|> il

pInline :: P Inlines
pInline = pInline' >>= pOptionalAttributes

pOptionalAttributes :: Inlines -> P Inlines
pOptionalAttributes (Many ils) =
 (lookahead (satisfyAscii (== '{')) *> do
  attr <- mconcat <$> some pAttributes
  case attr of
    Attr [] -> pure (Many ils)
    _ -> case Seq.viewr ils of
           Seq.EmptyR -> pure mempty
           ils' Seq.:> Node attr' (Str bs) ->
             -- attach attribute to last word
             let (front, lastword) = B8.breakEnd isWs bs
             in if B.null lastword
                   then pure (Many ils)
                   else pure $ Many (ils' Seq.|>
                                   Node attr' (Str front) Seq.|>
                                   Node attr (Str lastword))
           ils' Seq.:> Node attr' il ->
             pure $ Many (ils' Seq.|> Node (attr' <> attr) il))
  <|> pure (Many ils)

pInline' :: P Inlines
pInline' = do
  (do c <- lookahead (satisfyAscii isSpecial)
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
  c <- satisfyAscii (case mode st of
                       TableCellMode -> \d -> isSpecial d && d /= '|'
                       _ -> isSpecial)
  if c == '\r'
     then pure mempty
     else pure $ str $ FP.strToUtf8 [c]

pWords :: P Inlines
pWords = str <$> someAsciiWhile (not . isSpecial)

pEscaped :: P Inlines
pEscaped = do
  asciiChar '\\'
  c <- satisfyAscii (\d ->
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
    _ -> pure $ str $ FP.strToUtf8 [c]

pHardBreak :: P Inlines
pHardBreak = do -- assumes we've parsed \ already
  skipMany spaceOrTab
  endline
  skipMany spaceOrTab
  pure hardBreak

pSoftBreak :: P Inlines
pSoftBreak = do
  asciiChar '\n'
  skipMany ws
  (mempty <$ eof) <|> pure softBreak

pSymbol :: P Inlines
pSymbol = do
  asciiChar ':'
  bs <- byteStringOf $ skipSome (skipSatisfyAscii
                                    (\c -> c == '+' || c == '-' ||
                                         (isAscii c && isAlphaNum c)))
  asciiChar ':'
  pure $ symbol bs

pMath :: P Inlines
pMath = do
  asciiChar '$'
  mathStyle <- (DisplayMath <$ asciiChar '$') <|> pure InlineMath
  verb <- pVerbatim
  case unMany verb of
         [Node attr (Verbatim bs)] ->
              pure $ Many $ Seq.singleton $ Node attr (Math mathStyle bs)
         _ -> pure $ (case mathStyle of
                        DisplayMath -> str "$$"
                        _ -> str "$") <> verb

-- We use bits in state to track when we've had an opening for
-- an inline container:
-- bit  container require braces
-- ---  --------  --------------
-- 10   *         f
-- 11   _         f
-- 12   ^         f
-- 13   ~         f
-- 14   =         t
-- 15   +         t
-- 16   -         t
-- 17  reserved
-- 18  set if parsing inside '['
-- 19  set if opener had '{'

{-# INLINE bracesRequired #-}
bracesRequired :: Char -> Bool
bracesRequired '=' = True
bracesRequired '+' = True
bracesRequired '-' = True
bracesRequired _ = False

pCloser :: P ()
pCloser = do
  i <- activeDelims <$> getState
  if i .&. 0b000000000000000000000000000000000000000000011111111110000000000 == 0
     then failed
     else do
       skipSatisfyAscii (\d ->
                  (testBit i 10 && d == '*')
               || (testBit i 11 && d == '_')
               || (testBit i 12 && d == '^')
               || (testBit i 13 && d == '~')
               || (testBit i 14 && d == '=')
               || (testBit i 15 && d == '+')
               || (testBit i 16 && d == '-')
               || (testBit i 18 && d == ']'))
       let afterws = not $ testBit i 9
       let openerHadBrace = testBit i 19
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
            | bracesRequired c -> failed
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
                 byteStringOf (starter leftBrace)
  let bit = case c of
        '*' -> 10
        '_' -> 11
        '^' -> 12
        '~' -> 13
        '=' -> 14
        '+' -> 15
        '-' -> 16
        _   -> 17
  priorState <- activeDelims <$> getState
  updateState $ \st -> st{ activeDelims = setBit (activeDelims st) bit }
  updateState $ \st -> st{ activeDelims =
                            if leftBrace
                               then setBit (activeDelims st) 19
                               else clearBit (activeDelims st) 19 }
  firstIl <- pInline <|> pBetween c constructor -- to allow stacked cases like '**hi**'
  restIls <- many pInline
  let ils = mconcat (firstIl:restIls)
  updateState $ \st ->
    st{ activeDelims =
         activeDelims st .&. 0b111111111111111111111111111111111111111111100000000001111111111
                   .|. (priorState .&.
                       0b000000000000000000000000000000000000000000011111111110000000000) }
  (constructor ils <$ ender leftBrace) <|> pure (str starterBs <> ils)

pTicks :: P Int
pTicks = do
  sp <- getOffset
  skipSomeAsciiWhile (=='`')
  ep <- getOffset
  pure (ep - sp)

pVerbatim :: P Inlines
pVerbatim = do
  numticks <- pTicks
  let ender = pTicks >>= guard . (== numticks)
  let content = skipSomeAsciiWhile (\c -> c /= '`' && c /= '\\') <|>
                 (asciiChar '\\' <* anyChar) <|>
                 (fails ender *> skipSomeAsciiWhile (== '`'))
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
             byteStringOf (skipMany (skipSatisfyAscii (\c -> c /= '}' &&
                                                 not (isWs c))))
  asciiChar '}'
  pure fmt

pFootnoteReference :: P Inlines
pFootnoteReference = do
  asciiChar '['
  asciiChar '^'
  label <- byteStringOf $ skipMany $
             skipSatisfyAscii (\c -> c /= ']' && not (isWs c))
  asciiChar ']'
  pure $ footnoteReference label

-- returns Left with parsed content if no ] has been reached, otherwise Right
-- with inner contents.
pBracketed :: P (Either Inlines Inlines)
pBracketed = do
  let starter = asciiChar '['
  let ender = asciiChar ']'
  starterBs <- byteStringOf starter
  priorState <- activeDelims <$> getState
  updateState $ \st -> st{ activeDelims = setBit (activeDelims st) 18 }
  ils <- mconcat <$> many pInline
  updateState $ \st ->
    let i = activeDelims st
     in st{ activeDelims =
            i .&. 0b111111111111111111111111111111111111111111100000000001111111111
                   .|. (priorState .&.
                       0b000000000000000000000000000000000000000000011111111110000000000) }
  (Right ils <$ ender) <|> pure (Left (str starterBs <> ils))

pImage :: P Inlines
pImage = do
  asciiChar '!'
  (res, raw) <- withByteString pBracketed
  case res of
    Left ils -> pure (str "!" <> ils)
    Right ils ->
            ((str "!" <> span_ ils) <$ lookahead (asciiChar '{'))
        <|> (image ils <$> (pDestination <|> pReference raw))
        <|> pure (str "![" <> ils <> str "]")

pAutolink :: P Inlines
pAutolink = do
  asciiChar '<'
  res <- byteStringOf $ skipSome $ skipSatisfyAscii (\c -> c /= '>' && c /= '<')
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
                               skipSatisfyAscii
                                 (\c -> c /= '(' && c /= ')' && c /= '\\')))
            <|> (nestlevel <$ (asciiChar '\\' <* anyChar))
            <|> ((nestlevel + 1) <$ asciiChar '(')
            <|> ((nestlevel - 1) <$ asciiChar ')')
       pInBalancedParens lev

pReference :: ByteString -> P Target
pReference rawDescription = do
  asciiChar '['
  bs <- byteStringOf $ pAtMost 400 $ skipSatisfyAscii
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
  whitespaceAfter <- (True <$ lookahead (skipSatisfyAscii isWs)) <|> pure False
  guard $ not lbrace && (rbrace || not whitespaceBefore || whitespaceAfter)

pDoubleQuote :: P Inlines
pDoubleQuote = (do
  pOpenDoubleQuote
  contents <- mconcat <$> many (fails pCloseDoubleQuote *> pInline)
  (doubleQuoted contents <$ pCloseDoubleQuote)
    <|> pure (openDoubleQuote <> contents))
 <|> (closeDoubleQuote <$ asciiChar '"')

openDoubleQuote, closeDoubleQuote :: Inlines
openDoubleQuote = str (FP.strToUtf8 "\x201C")
closeDoubleQuote = str (FP.strToUtf8 "\x201D")

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
closeSingleQuote = str (FP.strToUtf8 "\x2019")

pHyphens :: P Inlines
pHyphens = do
  numHyphens <- length <$> some hyphen
  pure $ str $ go numHyphens
    where
     emdash = FP.strToUtf8 "\x2014"
     endash = FP.strToUtf8 "\x2013"
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
pEllipses = str (FP.strToUtf8 "\x2026") <$ byteString "..."
