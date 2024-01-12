{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- import Djot
import Test.Tasty.Bench
import Data.Functor.Identity  -- base >= 4.8
import qualified Data.ByteString as B
import Djot.Options ( ParseOptions(..) )
import Djot.Blocks ( parseDoc )
import Djot.Html ( renderDoc )
import Data.ByteString.Builder ( toLazyByteString )

main :: IO ()
main = do
  sample <- B.readFile "benchmark/m.dj"
  putStrLn $ "m.dj size = " <> show (B.length sample)
  defaultMain
   [ bench "parse m.dj" $ whnf (parseDoc ParseOptions{ optSourcePositions = False }) sample
    , bench "parse and render m.dj" $ whnf
       (either mempty (toLazyByteString . renderDoc) .
          parseDoc ParseOptions{ optSourcePositions = False }) sample
   ]
