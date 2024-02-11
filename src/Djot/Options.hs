{-# LANGUAGE StrictData #-}
module Djot.Options
( ParseOptions(..)
, RenderOptions(..)
, SourcePosOption(..) )
where

newtype ParseOptions =
  ParseOptions
  { sourcePositions :: SourcePosOption -- ^ Add attributes for source lines
  }
  deriving (Show)

newtype RenderOptions =
  RenderOptions
  { preserveSoftBreaks :: Bool -- ^ Preserve soft breaks as in the source
  }
  deriving (Show)

-- | Adding source positions for blocks adds almost no overhead to parsing.
-- Adding source positions for inlines has a small penalty.  For many purposes
-- it is enough to have source lines for blocks, so we offer the option.
data SourcePosOption =
  NoSourcePos | BlockSourcePos | AllSourcePos
  deriving (Show, Eq, Ord)
