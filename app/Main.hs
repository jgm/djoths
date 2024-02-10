{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
module Main where

import qualified Data.ByteString as B
import Data.ByteString.Builder (hPutBuilder)
import Djot ( ParseOptions(..), RenderOptions(..), SourcePosOption(..),
              parseDoc, renderHtml, renderDjot )
import System.Environment (getArgs)
import System.IO (stderr, stdout, hPutStrLn)
import System.Exit ( ExitCode(ExitFailure), exitWith, exitSuccess )
import Text.DocLayout (render)
import Text.Read (readMaybe)
import qualified Data.Text.IO as TIO

data OutputFormat = Html | Djot | Ast
  deriving (Eq, Show)

data WrapOption = Auto | Preserve | NoWrap
  deriving (Eq, Show)

data Opts =
      Opts{ format :: OutputFormat
          , files :: [FilePath]
          , wrap :: WrapOption
          , columns :: Int
          , sourcePos :: SourcePosOption }

parseOpts :: [String] -> IO Opts
parseOpts = go Opts{ format = Html, files = [], wrap = Preserve, columns = 72,
                     sourcePos = NoSourcePos }
 where
   go opts [] = pure opts
   go opts ("--wrap" : as) =
     case as of
       "auto" : as' -> go opts{ wrap = Auto } as'
       "preserve" : as' -> go opts{ wrap = Preserve } as'
       "none" : as' -> go opts{ wrap = NoWrap } as'
       _ -> err "--wrap must be followed by auto, preserve, or none"
   go opts ("--columns" : as) =
     case as of
       (a:as') | Just n <- readMaybe a
         -> go opts{ columns = n } as'
       _ -> err "--columns must be followed by a number"
   go opts ("--to" : as) =
     case as of
       "djot" : as' -> go opts{ format = Djot } as'
       "html" : as' -> go opts{ format = Html } as'
       "ast" : as' -> go opts{ format = Ast } as'
       _ -> err "--to must be followed by djot, html, or ast"
   go opts ("--sourcepos" : as) =
     case as of
       ("none":as') -> go opts{ sourcePos = NoSourcePos } as'
       ("block":as') -> go opts{ sourcePos = BlockSourcePos } as'
       ("all":as') -> go opts{ sourcePos = AllSourcePos } as'
       _ -> err "--sourcepos takes an argument (none|block|all)"
   go _opts ("--help" : _) = do
     putStrLn "djoths [options] [files]"
     putStrLn "  --to djot|html*|ast"
     putStrLn "  --wrap auto|preserve*|none"
     putStrLn "  --columns NUMBER"
     putStrLn "  --sourcepos none*|block|all"
     putStrLn "  --help"
     exitSuccess
   go opts (xs@('-':_) : as) =
     case break (== '=') xs of  -- support e.g. '--columns=33'
       (ys, '=':zs) -> go opts (ys : zs : as)
       _ -> err $ "Unknown option " <> ('-':xs)
   go opts (f : as) = go opts{ files = files opts ++ [f] } as

err :: String -> IO a
err msg = do
  hPutStrLn stderr msg
  exitWith $ ExitFailure 1

main :: IO ()
main = do
  opts <- getArgs >>= parseOpts
  bs <- case files opts of
          [] -> B.getContents
          fs  -> mconcat <$> mapM B.readFile fs
  let popts = ParseOptions { sourcePositions = sourcePos opts }
  let ropts = RenderOptions { preserveSoftBreaks = wrap opts == Preserve }
  case parseDoc popts bs of
    Right doc -> do
      case format opts of
        Html -> hPutBuilder stdout $ renderHtml ropts doc
        Djot -> TIO.putStr $ render (case wrap opts of
                                       NoWrap -> Nothing
                                       Preserve -> Nothing
                                       Auto -> Just (columns opts))
                           $ renderDjot ropts doc
        Ast -> print doc
      exitSuccess
    Left e -> do
      hPutStrLn stderr e
      exitWith $ ExitFailure 1
