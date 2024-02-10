{-# LANGUAGE StrictData #-}
module Djot.Options
( ParseOptions(..)
, RenderOptions(..)
, SourcePosOption(..) )
where

data ParseOptions =
  ParseOptions
  { sourcePositions :: SourcePosOption -- ^ Add attributes for source lines
  }
  deriving (Show)

data RenderOptions =
  RenderOptions
  { preserveSoftBreaks :: Bool -- ^ Preserve soft breaks as in the source
  }
  deriving (Show)

data SourcePosOption =
  NoSourcePos | BlockSourcePos | AllSourcePos
  deriving (Show, Eq, Ord)
