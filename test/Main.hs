{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Builder ( toLazyByteString )
import Djot ( ParseOptions(..), parseDoc, renderHtml, renderDjot )
import Djot.AST
import System.FilePath ((</>), takeExtension)
import System.Directory (getDirectoryContents)
import Text.DocLayout (render)

main :: IO ()
main = do
  specTests <- filter ((== ".test") . takeExtension) <$>
                  getDirectoryContents "test"
  tests <- mapM (getSpecTestTree . ("test" </>)) specTests
  defaultMain $ testGroup "Tests" tests

getSpecTestTree :: FilePath
                -> IO TestTree
getSpecTestTree fp = do
  tests <- getSpecTests fp
  let parser = parseDoc ParseOptions{ optSourcePositions = False } .
                 BL.toStrict
  return $ testGroup fp
             [ testGroup "djot -> html"
                 (map (toSpecTest parser) tests)
             , testGroup "native -> djot -> native"
                 (map (toRoundTripTest parser) tests)
             ]

toSpecTest :: (BL.ByteString -> Either String Doc)
           -> SpecTest -> TestTree
toSpecTest parser st =
  testCase name (actual @?= expected)
    where name = "lines " ++ show (start_line st) ++ "-" ++ show (end_line st)
          expected = fromUtf8 $ html st
          actual = either mempty (fromUtf8 . toLazyByteString . renderHtml)
                     . parser $ djot st

toRoundTripTest :: (BL.ByteString -> Either String Doc)
                -> SpecTest -> TestTree
toRoundTripTest parser st =
  testCase name ((actual == expected) @? rtlog)
    where name = "lines " ++ show (start_line st) ++ "-" ++ show (end_line st)
          native = either (\_ -> Doc mempty mempty mempty) id $ parser (djot st)
          expected = native
          renderedDjot = encodeUtf8 . TL.fromStrict $ render (Just 62) $ renderDjot native
          actual = either (\_ -> Doc mempty mempty mempty) id $ parser renderedDjot
          lbsToStr = TL.unpack . fromUtf8
          rtlog = lbsToStr (djot st) <>
                  "↓\n" <>
                  show native <> "\n" <>
                  "↓\n" <>
                  lbsToStr renderedDjot <>
                  "↓\n" <>
                  show actual <> "\n"

data SpecTest = SpecTest
     { djot       :: BL.ByteString
     , end_line   :: Int
     , start_line :: Int
     , html       :: BL.ByteString }
  deriving (Show)

getSpecTests :: FilePath -> IO [SpecTest]
getSpecTests fp = do
  speclines <- zip [1..] . BL.lines <$> BL.readFile fp
  pure $ parseSpecTests speclines

--- state machine parser for spec test cases

data ParseState =
     Scanning
   | ParsingDjot (SpecTest, BL.ByteString)
   | ParsingHtml (SpecTest, BL.ByteString)
   deriving (Show)

parseSpecTests :: [(Int, BL.ByteString)] -> [SpecTest]
parseSpecTests = go Scanning
 where
   go _ [] = []
   go Scanning ((ln, bs) : xs)
     | BL.length bs > 0 && BL.all (== '`') bs =
          go (ParsingDjot (SpecTest { djot = mempty
                                    , end_line = ln
                                    , start_line = ln
                                    , html = mempty }, bs)) xs
     | otherwise = go Scanning xs
   go (ParsingDjot (st,fence)) ((_,bs) : xs)
     | bs == "." =
          go (ParsingHtml (st, fence)) xs
     | otherwise =
          go (ParsingDjot (st{ djot = djot st <> bs <> "\n" }, fence)) xs
   go (ParsingHtml (st,fence)) ((ln,bs) : xs)
     | bs == fence =
          st{ end_line = ln } : go Scanning xs
     | otherwise =
          go (ParsingHtml (st{ html = html st <> bs <> "\n" }, fence)) xs

fromUtf8 :: BL.ByteString -> TL.Text
fromUtf8 = decodeUtf8With lenientDecode
