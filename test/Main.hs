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
    , testGroup "Djot.AST" astTests
    , testGroup "djot writer" writerTests
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

astTests :: [TestTree]
astTests =
  [ testCase "NoPos is an identity for <>" $ do
      Pos 1 1 2 5 <> NoPos @?= Pos 1 1 2 5
      NoPos <> Pos 1 1 2 5 @?= Pos 1 1 2 5
  , testCase "<> on Pos spans both arguments" $
      Pos 1 1 1 4 <> Pos 2 1 2 7 @?= Pos 1 1 2 7
  , testCase "inlinesToByteString emits valid UTF-8" $ do
      inlinesToByteString (singleQuoted (str "a")) @?=
        strToUtf8 "\x2018\&a\x2019"
      inlinesToByteString (doubleQuoted (str "a")) @?=
        strToUtf8 "\x201C\&a\x201D"
      inlinesToByteString nonBreakingSpace @?= strToUtf8 "\xA0"
  , testCase "image alt text with smart quotes is valid UTF-8" $
      convertNoPos "![a \"b\" c](url)\n" @?=
        "<p><img alt=\"a \x201C\&b\x201D c\" src=\"url\"></p>\n"
  , testCase "auto identifier with smart quotes is valid UTF-8" $
      convertNoPos "# Say \"hi\"\n" @?=
        "<section id=\"Say-\x201Chi\x201D\">\n<h1>Say \x201Chi\x201D</h1>\n</section>\n"
  , testCase "autolink with dot before @ is an email link" $
      convertNoPos "<user.name@example.com>\n" @?=
        "<p><a href=\"mailto:user.name@example.com\">user.name@example.com</a></p>\n"
  , testCase "autolink with @ only after : is a url link" $
      convertNoPos "<x:@example.com>\n" @?=
        "<p><a href=\"x:@example.com\">x:@example.com</a></p>\n"
  , testCase "CR-only line ending produces a soft break" $
      convertNoPos "a\rb\n" @?= "<p>a\nb</p>\n"
  , testCase "CRLF line ending produces a soft break" $
      convertNoPos "a\r\nb\n" @?= "<p>a\nb</p>\n"
  , testCase "whitespace runs in quoted attribute values collapse (as in djot.js)" $
      convertNoPos "{k=\"a  b\"}\npara\n" @?=
        "<p k=\"a b\">para</p>\n"
  , testCase "newline in quoted attribute value becomes a space" $
      convertNoPos "{k=\"a\n b\"}\npara\n" @?=
        "<p k=\"a b\">para</p>\n"
  ]

convertNoPos :: BL.ByteString -> TL.Text
convertNoPos = either mempty (fromUtf8 . toLazyByteString .
                    renderHtml RenderOptions{ preserveSoftBreaks = True })
               . parseDoc ParseOptions{ sourcePositions = NoSourcePos }
               . BL.toStrict

writerTests :: [TestTree]
writerTests =
  [ testCase "letter list style wraps around after 26" $
      render Nothing (renderDjot RenderOptions{ preserveSoftBreaks = True }
         mempty{ docBlocks = Djot.AST.orderedList
                   OrderedListAttributes{ orderedListStyle = LetterUpper
                                        , orderedListDelim = RightPeriod
                                        , orderedListStart = 27 }
                   Tight
                   [para (str "one"), para (str "two")] })
        @?= "A. one\nB. two\n"
  ]

sourcePosTests :: [TestTree]
sourcePosTests =
  let convert = either mempty (fromUtf8 . toLazyByteString .
                      renderHtml RenderOptions{ preserveSoftBreaks = True })
                 . parseDoc ParseOptions{ sourcePositions = AllSourcePos }
  in [ testCase "period at end" $
        convert "the `goo` option.\n" @?=
        "<p data-pos=\"1:1-1:17\"><span data-pos=\"1:1-1:4\">the </span><code data-pos=\"1:5-1:9\">goo</code><span data-pos=\"1:10-1:17\"> option.</span></p>\n"
     , testCase "attr after *" $
        convert "*{.foo}\n" @?=
        "<p data-pos=\"1:1-1:7\"><span data-pos=\"1:1-1:1\" class=\"foo\">*</span></p>\n"
     , testCase "attr on last word of merged strs" $
        convert "x y.z{.c}\n" @?=
        "<p data-pos=\"1:1-1:9\"><span data-pos=\"1:1-1:2\">x </span><span data-pos=\"1:3-1:5\" class=\"c\">y.z</span></p>\n"
     , testCase "tab advances to next tab stop (1-based)" $
        convert "a\tb *c*\n" @?=
        "<p data-pos=\"1:1-1:9\"><span data-pos=\"1:1-1:6\">a\tb </span><strong data-pos=\"1:7-1:9\"><span data-pos=\"1:8-1:8\">c</span></strong></p>\n"
     , testCase "no newline at end" $
        convert "foo" @?=
        "<p data-pos=\"1:1-1:3\"><span data-pos=\"1:1-1:3\">foo</span></p>\n"
     , testCase "list" $
        convert "1. > hello\nthere\n\n2.  ok" @?=
        "<ol data-pos=\"1:1-4:6\">\n<li>\n<blockquote data-pos=\"1:4-2:5\">\n<p data-pos=\"1:6-2:5\"><span data-pos=\"1:6-1:10\">hello</span>\n<span data-pos=\"2:1-2:5\">there</span></p>\n</blockquote>\n</li>\n<li>\n<p data-pos=\"4:5-4:6\"><span data-pos=\"4:5-4:6\">ok</span></p>\n</li>\n</ol>\n"
     , testCase "code block" $
        convert "``` ruby\nhi\n```\n" @?=
        "<pre data-pos=\"1:1-3:3\"><code class=\"language-ruby\">hi\n</code></pre>\n"
     , testCase "nested " $
        convert "*_hi_*" @?=
        "<p data-pos=\"1:1-1:6\"><strong data-pos=\"1:1-1:6\"><em data-pos=\"1:2-1:5\"><span data-pos=\"1:3-1:4\">hi</span></em></strong></p>\n"
     , testCase "hr " $
        convert "----\n" @?=
        "<hr data-pos=\"1:1-1:4\">\n"
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
          renderedDjot = encodeUtf8 . TL.fromStrict $ render (Just 78) $
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
