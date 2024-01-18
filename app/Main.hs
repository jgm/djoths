{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
module Main where

import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.Builder (hPutBuilder)
import Djot ( ParseOptions(..), parseDoc, renderHtml, renderDjot )
import System.Environment (getArgs)
import System.IO (stderr, stdout, hPutStrLn)
import System.Exit ( ExitCode(ExitFailure, ExitSuccess), exitWith )
import Text.DocLayout (render)
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

data OutputFormat = Html | Djot

data Opts =
      Opts{ format :: OutputFormat
          , files :: [FilePath] }

parseOpts :: [String] -> IO Opts
parseOpts = foldM go Opts{ format = Html, files = [] }
 where
   go opts "--djot" = pure $ opts{ format = Djot }
   go opts "--html" = pure $ opts{ format = Html }
   go _opts ('-':xs) = do
     hPutStrLn stderr $ "Unknown option " <> ('-':xs)
     exitWith $ ExitFailure 1
   go opts f = pure $ opts{ files = files opts ++ [f] }

main :: IO ()
main = do
  opts <- getArgs >>= parseOpts
  bs <- case files opts of
          [] -> B.getContents
          fs  -> mconcat <$> mapM B.readFile fs
  let popts = ParseOptions{ optSourcePositions = False }
  case parseDoc popts bs of
    Right doc -> do
      case format opts of
        Html -> hPutBuilder stdout $ renderHtml doc
        Djot -> TIO.putStr $ render Nothing (renderDjot doc)
      exitWith ExitSuccess
    Left e -> do
      hPutStrLn stderr e
      exitWith $ ExitFailure 1
