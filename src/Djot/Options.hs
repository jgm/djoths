{-# LANGUAGE StrictData #-}
module Djot.Options
( ParseOptions(..)
, RenderOptions(..)
)
where

data ParseOptions =
  ParseOptions
  { optSourcePositions :: Bool }
  deriving (Show)

data RenderOptions =
  RenderOptions
  { optPreserveSoftBreaks :: Bool }
  deriving (Show)
