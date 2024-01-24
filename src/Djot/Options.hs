{-# LANGUAGE StrictData #-}
module Djot.Options
( ParseOptions(..)
, RenderOptions(..) )
where

data ParseOptions =
  ParseOptions
--  { sourcePositions :: Bool }
  deriving (Show)

data RenderOptions =
  RenderOptions
  { preserveSoftBreaks :: Bool }
  deriving (Show)
