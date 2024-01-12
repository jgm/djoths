{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- import           Djot
import Control.Monad (when)
import Data.Functor.Identity
import Data.List (groupBy)
import System.IO (hSetEncoding, utf8, openFile,
 IOMode(..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Builder ( toLazyByteString )
import Djot.Options ( ParseOptions(..) )
import Djot.Blocks ( parseDoc )
import Djot.Html ( renderDoc )
import Djot.AST ( Doc )
import System.FilePath ((</>))

main :: IO ()
main = do
  tests <- mapM (getSpecTestTree . ("test" </>))
            [ "block_quote.test"
            , "code_blocks.test"
            , "definition_lists.test"
            , "emphasis.test"
            , "escapes.test"
            , "fenced_divs.test"
            -- , "filters.test"
            , "footnotes.test"
            , "headings.test"
            , "insert_delete_mark.test"
            , "links_and_images.test"
            , "lists.test"
            , "math.test"
            , "para.test"
            , "raw.test"
            , "regression.test"
            , "smart.test"
            , "spans.test"
            , "super_subscript.test"
            , "symb.test"
            , "tables.test"
            , "task_lists.test"
            , "thematic_breaks.test"
            , "verbatim.test" ]
  defaultMain $ testGroup "Tests" tests

getSpecTestTree :: FilePath
                -> IO TestTree
getSpecTestTree fp = do
  tests <- getSpecTests fp
  let parser = parseDoc ParseOptions{ optSourcePositions = False } .
                 BL.toStrict
  return $ testGroup fp $ map (toSpecTest parser) tests

toSpecTest :: (BL.ByteString -> Either String Doc)
           -> SpecTest -> TestTree
toSpecTest parser st =
  testCase name (actual @?= expected)
    where name = "lines " ++ show (start_line st) ++ "-" ++ show (end_line st)
          expected = fromUtf8 $ html st
          actual = either mempty (fromUtf8 . toLazyByteString . renderDoc)
                     . parser $ djot st

fromRight :: b -> Either a b ->  b
fromRight fallback (Left _) = fallback
fromRight _ (Right x)       = x


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
