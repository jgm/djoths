{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Builder ( toLazyByteString )
import Djot ( ParseOptions(..), RenderOptions(..), SourcePosOption(..),
              parseDoc, renderHtml, renderDjot )
import Djot.Parse ( parse, satisfy, strToUtf8, utf8ToStr, Chunk(..) )
import Djot.AST
import System.FilePath ((</>), takeExtension, takeFileName)
import System.Directory (getDirectoryContents)
import Text.DocLayout (render)

main :: IO ()
main = do
  specTests <- filter ((== ".test") . takeExtension) <$>
                  getDirectoryContents "test"
  tests <- mapM (\fp -> (fp,) <$> getSpecTests ("test" </> fp)) specTests
  let parser = parseDoc ParseOptions{ sourcePositions = NoSourcePos } . BL.toStrict
  defaultMain $ testGroup "Tests" $
    [ testGroup "djot -> html"
        (map (\(fp, ts) ->
                testGroup fp
                 (map (toSpecTest parser) ts)) tests)
    , testGroup "native -> djot -> native"
       [testGroup fp (map (toRoundTripTest parser) ts)
          | (fp, ts) <- tests
          , takeFileName fp /= "raw.test"]
    , testGroup "Djot.Parse" parserTests
    , testGroup "sourcepos" sourcePosTests
    , testGroup "Fuzz"
       [testProperty "parses all inputs"
         (\s -> case parseDoc ParseOptions{ sourcePositions = NoSourcePos }
                 (strToUtf8 s) of
                    Left _ -> False
                    Right _ -> True)
       ]
    ]

parserTests :: [TestTree]
parserTests =
  [ testCase "satisfy multibyte"
      (parse (satisfy (=='ǎ') *> satisfy (=='老')) ()
         (toChunks $ strToUtf8 "ǎ老bc") @?= Just '老')
  , testProperty "UTF8 conversion round-trips"
      (\s -> utf8ToStr (strToUtf8 s) == s)
  ]

sourcePosTests :: [TestTree]
sourcePosTests =
  let convert = either mempty (fromUtf8 . toLazyByteString .
                      renderHtml RenderOptions{ preserveSoftBreaks = True })
                 . parseDoc ParseOptions{ sourcePositions = AllSourcePos }
  in [ testCase "period at end" $
        convert "the `goo` option.\n" @?=
        "<p data-pos=\"1:1-2:0\"><span data-pos=\"1:1-1:4\">the </span><code data-pos=\"1:5-1:9\">goo</code><span data-pos=\"1:10-1:17\"> option.</span></p>\n"
     , testCase "attr after *" $
        convert "*{.foo}\n" @?=
        "<p data-pos=\"1:1-2:0\"><span data-pos=\"1:1-1:1\" class=\"foo\">*</span></p>\n"
     , testCase "no newline at end" $
        convert "foo" @?=
        "<p data-pos=\"1:1-1:3\"><span data-pos=\"1:1-1:3\">foo</span></p>\n"
     , testCase "list" $
        convert "1. > hello\nthere\n\n2.  ok" @?=
        "<ol data-pos=\"1:4-4:0\">\n<li>\n<blockquote data-pos=\"1:4-3:0\">\n<p data-pos=\"1:6-3:0\"><span data-pos=\"1:6-1:10\">hello</span>\n<span data-pos=\"2:1-2:5\">there</span></p>\n</blockquote>\n</li>\n<li>\n<p data-pos=\"4:5-4:0\"><span data-pos=\"4:5-4:6\">ok</span></p>\n</li>\n</ol>\n"
     ]

toChunks :: B.ByteString -> [Chunk]
toChunks bs = [Chunk{ chunkBytes = bs, chunkLine = 1, chunkColumn = 0 }]

toSpecTest :: (BL.ByteString -> Either String Doc)
           -> SpecTest -> TestTree
toSpecTest parser st =
  testCase name (actual @?= expected)
    where name = "lines " ++ show (start_line st) ++ "-" ++ show (end_line st)
          expected = fromUtf8 $ html st
          ropts = RenderOptions{ preserveSoftBreaks = True }
          actual = either mempty (fromUtf8 . toLazyByteString . renderHtml ropts)
                     . parser $ djot st

toRoundTripTest :: (BL.ByteString -> Either String Doc)
                -> SpecTest -> TestTree
toRoundTripTest parser st =
  testCase name ((actual == expected) @? rtlog)
    where name = "lines " ++ show (start_line st) ++ "-" ++ show (end_line st)
          native = either (\_ -> mempty) id $ parser (djot st)
          expected = native
          ropts = RenderOptions{ preserveSoftBreaks = True }
          renderedDjot = encodeUtf8 . TL.fromStrict $ render (Just 62) $
                           renderDjot ropts native
          actual = either (\_ -> mempty) id $ parser renderedDjot
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
     , source     :: FilePath
     , end_line   :: Int
     , start_line :: Int
     , html       :: BL.ByteString }
  deriving (Show)

getSpecTests :: FilePath -> IO [SpecTest]
getSpecTests fp = do
  speclines <- zip [1..] . BL.lines <$> BL.readFile fp
  pure $ parseSpecTests fp speclines

--- state machine parser for spec test cases

data ParseState =
     Scanning
   | ParsingDjot (SpecTest, BL.ByteString)
   | ParsingHtml (SpecTest, BL.ByteString)
   deriving (Show)

parseSpecTests :: FilePath -> [(Int, BL.ByteString)] -> [SpecTest]
parseSpecTests fp = go Scanning
 where
   go _ [] = []
   go Scanning ((ln, bs) : xs)
     | BL.length bs > 0 && BL.all (== '`') bs =
          go (ParsingDjot (SpecTest { djot = mempty
                                    , source = fp
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
