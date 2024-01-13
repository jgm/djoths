{-# LANGUAGE Strict #-}
module Main where

import qualified Data.ByteString as B
import Data.ByteString.Builder (hPutBuilder)
import Djot.Options ( ParseOptions(..) )
import Djot.Blocks ( parseDoc )
import Djot.Html ( renderDoc )
import System.Environment (getArgs)
import System.IO (stderr, stdout, hPutStrLn)
import System.Exit ( ExitCode(ExitFailure, ExitSuccess), exitWith )

main :: IO ()
main = do
  fs <- getArgs
  bs <- case fs of
          [] -> B.getContents
          _  -> mconcat <$> mapM B.readFile fs
  let opts = ParseOptions{ optSourcePositions = False }
  case parseDoc opts bs of
    Right doc -> do
      hPutBuilder stdout $ renderDoc doc
      exitWith ExitSuccess
    Left e -> do
      hPutStrLn stderr e
      exitWith $ ExitFailure 1
