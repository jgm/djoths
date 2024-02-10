{-# LANGUAGE StrictData #-}
module Djot.Options
( ParseOptions(..)
, RenderOptions(..) )
where

data ParseOptions =
  ParseOptions
  { sourcePositions :: Bool -- ^ Add attributes for source lines
  }
  deriving (Show)

data RenderOptions =
  RenderOptions
  { preserveSoftBreaks :: Bool -- ^ Preserve soft breaks as in the source
  }
  deriving (Show)
