{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
module Main where

import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.Builder (hPutBuilder)
import Djot ( ParseOptions(..), RenderOptions(..),
              parseDoc, renderHtml, renderDjot )
import System.Environment (getArgs)
import System.IO (stderr, stdout, hPutStrLn)
import System.Exit ( ExitCode(ExitFailure, ExitSuccess), exitWith )
import Text.DocLayout (render)
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

data OutputFormat = Html | Djot

data WrapOpts = Preserve | Wrap Int
  deriving (Eq)

data Opts =
      Opts{ format :: OutputFormat
          , wrap :: WrapOpts
          , files :: [FilePath] }

parseOpts :: [String] -> IO Opts
parseOpts = foldM go Opts{ format = Html, wrap = Preserve, files = [] }
 where
   go opts "-d" = pure $ opts{ format = Djot }
   go opts ('-':'w':ds) =
     if null ds
        then pure $ opts{ wrap = Wrap 0 }
        else case readMaybe ds of
               Just (n :: Int) -> pure $ opts{ wrap = Wrap n }
               Nothing -> do
                 hPutStrLn stderr "Can't parse argument of -w as number"
                 exitWith $ ExitFailure 1
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
  let ropts = RenderOptions{ optPreserveSoftBreaks = wrap opts == Preserve }
  case parseDoc popts bs of
    Right doc -> do
      case format opts of
        Html -> hPutBuilder stdout $ renderHtml ropts doc
        Djot -> TIO.putStr $ render (case wrap opts of
                                       Preserve -> Nothing
                                       Wrap n -> Just n)
                                    (renderDjot ropts doc)
      exitWith ExitSuccess
    Left e -> do
      hPutStrLn stderr e
      exitWith $ ExitFailure 1
