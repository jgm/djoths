module Djot.Options
( ParseOptions(..) )
where

data ParseOptions =
  ParseOptions
  { optSourcePositions :: Bool }
  deriving (Show)
