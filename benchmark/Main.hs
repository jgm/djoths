{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
import Test.Tasty.Bench
import Data.Functor.Identity  -- base >= 4.8
import qualified Data.ByteString as B
import Djot ( ParseOptions(..), parseDoc, renderDoc )
import Data.ByteString.Builder ( toLazyByteString )
import System.Directory
import System.FilePath (takeExtension, (</>))

main :: IO ()
main = do
  fns <- filter ((== ".dj") . takeExtension) <$> listDirectory "benchmark"
  files <- mapM (\fn -> (fn,) <$> B.readFile ("benchmark" </> fn)) fns
  defaultMain $
   map (\(fn, bs) ->
     bench fn $ whnf (parseDoc ParseOptions{ optSourcePositions = False }) bs)
     files
