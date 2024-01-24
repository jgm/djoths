{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Djot.AST
( Inline(..),
  Many(..),
  Inlines,
  MathStyle(..),
  Format(..),
  Node(Node),
  addAttr,
  Block(..),
  Blocks,
  Doc(..),
  NoteMap(..),
  insertNote,
  lookupNote,
  ReferenceMap(..),
  insertReference,
  lookupReference,
  normalizeLabel,
  Attr(..),
  Target(..),
  TaskStatus(..),
  Align(..),
  Cell(..),
  CellType(..),
  Caption(..),
  ListSpacing(..),
  OrderedListAttributes(..),
  OrderedListDelim(..),
  OrderedListStyle(..),
  QuoteType(..),
  delete,
  displayMath,
  insert,
  emailLink,
  emph,
  footnoteReference,
  hardBreak,
  highlight,
  image,
  inlineMath,
  link,
  nonBreakingSpace,
  rawInline,
  softBreak,
  span_,
  str,
  strong,
  subscript,
  superscript,
  singleQuoted,
  doubleQuoted,
  symbol,
  verbatim,
  urlLink,
  para,
  section,
  heading,
  blockQuote,
  codeBlock,
  div,
  bulletList,
  orderedList,
  definitionList,
  taskList,
  thematicBreak,
  table,
  rawBlock,
  inlinesToByteString
  )
where

import Prelude hiding (div)
import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M
import Data.Data (Data, Typeable)
import qualified Data.ByteString.Char8 as B8
import GHC.Generics (Generic)

newtype Attr = Attr [(ByteString, ByteString)]
  deriving (Show, Eq, Ord, Typeable, Generic, Data)

instance Semigroup Attr where
  Attr as <> Attr bs =
    Attr $ foldr integrate bs as

instance Monoid Attr where
  mappend = (<>)
  mempty = Attr mempty

integrate :: (ByteString, ByteString)
          -> [(ByteString, ByteString)] -> [(ByteString, ByteString)]
integrate (k,v) kvs =
  case lookup k kvs of
    Nothing -> (k,v) : kvs
    Just v'
      | k == "class" ->
        (k, v <> " " <> v') : filter (\(k',_) -> k' /= "class") kvs
      | otherwise -> kvs

data Node a = Node Attr a
  deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Typeable, Generic)

{-# INLINE addAttr #-}
addAttr :: Functor f => Attr -> f (Node a) -> f (Node a)
addAttr attr nodes =
  fmap (\(Node attr' bs) -> Node (attr' <> attr) bs) nodes

newtype Format = Format { unFormat :: ByteString }
  deriving (Show, Eq, Ord, Typeable, Generic)

data MathStyle = DisplayMath | InlineMath
  deriving (Show, Ord, Eq, Typeable, Generic)

data Target =
    Direct ByteString
  | Reference ByteString
  deriving (Show, Ord, Eq, Typeable, Generic)

data QuoteType = SingleQuotes | DoubleQuotes
  deriving (Show, Ord, Eq, Typeable, Generic)

data Inline =
      Str ByteString
    | Emph Inlines
    | Strong Inlines
    | Highlight Inlines
    | Insert Inlines
    | Delete Inlines
    | Superscript Inlines
    | Subscript Inlines
    | Verbatim ByteString
    | Symbol ByteString
    | Math MathStyle ByteString
    | Link Inlines Target
    | Image Inlines Target
    | Span Inlines
    | FootnoteReference ByteString
    | UrlLink ByteString
    | EmailLink ByteString
    | RawInline Format ByteString
    | NonBreakingSpace
    | Quoted QuoteType Inlines
    | SoftBreak
    | HardBreak
    deriving (Show, Ord, Eq, Typeable, Generic)

newtype Many a = Many { unMany :: Seq a }
  deriving (Show, Ord, Eq, Functor, Traversable, Foldable, Typeable, Generic)

type Inlines = Many (Node Inline)

instance Semigroup Inlines where
  Many as <> Many bs =
    case (Seq.viewr as, Seq.viewl bs) of
      (as' Seq.:> Node attr (Str s), Node attr' (Str t) Seq.:< bs')
        | attr == attr'
          -> Many (as' <> (Node attr (Str (s <> t)) Seq.<| bs'))
      _ -> Many (as <> bs)

instance Monoid Inlines where
  mappend = (<>)
  mempty = Many mempty

data ListSpacing = Tight | Loose
  deriving (Show, Ord, Eq, Typeable, Generic)

data OrderedListStyle =
  Decimal | LetterUpper | LetterLower | RomanUpper | RomanLower
  deriving (Show, Ord, Eq, Typeable, Generic)

data OrderedListDelim =
  RightPeriod | RightParen | LeftRightParen
  deriving (Show, Ord, Eq, Typeable, Generic)

data OrderedListAttributes =
  OrderedListAttributes
  { orderedListStyle :: OrderedListStyle
  , orderedListDelim :: OrderedListDelim
  , orderedListStart :: Int }
  deriving (Show, Ord, Eq, Typeable, Generic)

data TaskStatus = Complete | Incomplete
  deriving (Show, Ord, Eq, Typeable, Generic)

newtype Caption = Caption Blocks
  deriving (Show, Ord, Eq, Typeable, Generic)

data Align = AlignLeft | AlignRight | AlignCenter | AlignDefault
  deriving (Show, Ord, Eq, Typeable, Generic)

data CellType = HeadCell | BodyCell
  deriving (Show, Ord, Eq, Typeable, Generic)

data Cell = Cell CellType Align Inlines
  deriving (Show, Ord, Eq, Typeable, Generic)

data Block =
    Para Inlines
  | Section Blocks
  | Heading Int Inlines
  | BlockQuote Blocks
  | CodeBlock ByteString ByteString
  | Div Blocks
  | OrderedList OrderedListAttributes ListSpacing [Blocks]
  | BulletList ListSpacing [Blocks]
  | TaskList ListSpacing [(TaskStatus, Blocks)]
  | DefinitionList ListSpacing [(Inlines, Blocks)]
  | ThematicBreak
  | Table (Maybe Caption) [[Cell]]
  | RawBlock Format ByteString
  deriving (Show, Ord, Eq, Typeable, Generic)

type Blocks = Many (Node Block)

instance Semigroup Blocks where
  Many as <> Many bs = Many (as <> bs)

instance Monoid Blocks where
  mappend = (<>)
  mempty = Many mempty

data Doc =
  Doc{ docBlocks :: Blocks
     , docFootnotes :: NoteMap
     , docReferences :: ReferenceMap
     } deriving (Show, Ord, Eq, Typeable, Generic)

instance Semigroup Doc where
  Doc bs ns rs <> Doc bs' ns' rs' = Doc (bs <> bs') (ns <> ns') (rs <> rs')

instance Monoid Doc where
  mappend = (<>)
  mempty = Doc mempty mempty mempty

-- | A map from labels to contents.
newtype NoteMap = NoteMap { unNoteMap :: M.Map ByteString Blocks }
  deriving (Show, Ord, Eq, Semigroup, Monoid, Typeable, Generic)

insertNote :: ByteString -> Blocks -> NoteMap -> NoteMap
insertNote label ref (NoteMap m) =
  NoteMap (M.insert (normalizeLabel label) ref m)

lookupNote :: ByteString -> NoteMap -> Maybe Blocks
lookupNote label (NoteMap m) =
  M.lookup (normalizeLabel label) m

newtype ReferenceMap =
  ReferenceMap { unReferenceMap :: M.Map ByteString (ByteString, Attr) }
  deriving (Show, Ord, Eq, Semigroup, Monoid, Typeable, Generic)

normalizeLabel :: ByteString -> ByteString
normalizeLabel = B8.unwords . B8.words

insertReference :: ByteString -> (ByteString, Attr) -> ReferenceMap
                -> ReferenceMap
insertReference label ref (ReferenceMap rm) =
  ReferenceMap (M.insert (normalizeLabel label) ref rm)

lookupReference :: ByteString -> ReferenceMap -> Maybe (ByteString, Attr)
lookupReference label (ReferenceMap rm) =
  M.lookup (normalizeLabel label) rm

{-# INLINE inline #-}
inline :: Inline -> Inlines
inline = Many . Seq.singleton . Node mempty

str, verbatim, symbol :: ByteString -> Inlines
str = inline . Str
verbatim = inline . Verbatim
symbol = inline . Symbol

emph, strong, superscript, subscript :: Inlines -> Inlines
emph = inline . Emph
strong = inline . Strong
superscript = inline . Superscript
subscript = inline . Subscript

highlight, insert, delete :: Inlines -> Inlines
highlight = inline . Highlight
insert = inline . Insert
delete = inline . Delete

link, image :: Inlines -> Target -> Inlines
link ils url = inline $ Link ils url
image ils url = inline $ Image ils url

span_ :: Inlines -> Inlines
span_ = inline . Span

softBreak, hardBreak, nonBreakingSpace :: Inlines
softBreak = inline SoftBreak
hardBreak = inline HardBreak
nonBreakingSpace = inline NonBreakingSpace

inlineMath, displayMath :: ByteString -> Inlines
inlineMath = inline . Math InlineMath
displayMath = inline . Math DisplayMath

singleQuoted, doubleQuoted :: Inlines -> Inlines
singleQuoted = inline . Quoted SingleQuotes
doubleQuoted = inline . Quoted DoubleQuotes

footnoteReference :: ByteString -> Inlines
footnoteReference = inline . FootnoteReference

urlLink, emailLink :: ByteString -> Inlines
urlLink = inline . UrlLink
emailLink = inline . EmailLink

rawInline :: Format -> ByteString -> Inlines
rawInline f = inline . RawInline f


--

block :: Block -> Blocks
block = Many . Seq.singleton . Node mempty

para :: Inlines -> Blocks
para = block . Para

section :: Blocks -> Blocks
section = block . Section

heading :: Int -> Inlines -> Blocks
heading lev = block . Heading lev

blockQuote :: Blocks -> Blocks
blockQuote = block . BlockQuote

codeBlock :: ByteString -> ByteString -> Blocks
codeBlock lang bs = block $ CodeBlock lang bs

bulletList :: ListSpacing -> [Blocks] -> Blocks
bulletList tightness = block . BulletList tightness

orderedList :: OrderedListAttributes -> ListSpacing -> [Blocks] -> Blocks
orderedList attr tightness = block . OrderedList attr tightness

definitionList :: ListSpacing -> [(Inlines, Blocks)] -> Blocks
definitionList tightness = block . DefinitionList tightness

taskList :: ListSpacing -> [(TaskStatus, Blocks)] -> Blocks
taskList tightness = block . TaskList tightness

div :: Blocks -> Blocks
div = block . Div

thematicBreak :: Blocks
thematicBreak = block ThematicBreak

table :: Maybe Caption -> [[Cell]] -> Blocks
table mbCaption = block . Table mbCaption

rawBlock :: Format -> ByteString -> Blocks
rawBlock f = block . RawBlock f

inlinesToByteString :: Inlines -> ByteString
inlinesToByteString = foldMap go . unMany
 where
  go (Node _attr x) =
      case x of
        Str bs -> bs
        Emph ils -> inlinesToByteString ils
        Strong ils -> inlinesToByteString ils
        Highlight ils -> inlinesToByteString ils
        Insert ils -> inlinesToByteString ils
        Delete ils -> inlinesToByteString ils
        Superscript ils -> inlinesToByteString ils
        Subscript ils -> inlinesToByteString ils
        Quoted SingleQuotes ils ->
          "\x2018" <> inlinesToByteString ils <> "\x2019"
        Quoted DoubleQuotes ils ->
          "\x201C" <> inlinesToByteString ils <> "\x201D"
        Verbatim bs -> bs
        Math DisplayMath bs -> "$$" <> bs <> "$$"
        Math InlineMath bs -> "$" <> bs <> "$"
        Symbol bs -> ":" <> bs <> ":"
        Link ils _url -> inlinesToByteString ils
        Image ils _url -> inlinesToByteString ils
        Span ils -> inlinesToByteString ils
        UrlLink url -> url
        EmailLink email -> email
        RawInline _ _ -> mempty
        FootnoteReference bs -> "[" <> bs <> "]"
        SoftBreak -> "\n"
        HardBreak -> "\n"
        NonBreakingSpace -> "\160"
