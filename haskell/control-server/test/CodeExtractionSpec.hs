{-# LANGUAGE OverloadedStrings #-}

-- | Golden tests for code extraction boundary detection.
--
-- Tests that readCodeAtRange correctly stops at:
-- 1. Section dividers (═══ or ---)
-- 2. Haddock comments for new definitions (-- |)
-- 3. Two consecutive blank lines
-- 4. Next top-level definition (starts with lowercase at column 0)
-- 5. Maximum 30 lines (hard limit)
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Tidepool.Control.CodeExtraction (readCodeAtRange)
import Tidepool.Effect.LSP (Range(..), Position(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Code Extraction"
  [ testCase "CASE 1: Stops at section divider (═══)" case1_sectionDivider
  , testCase "CASE 2: Stops at haddock comment (-- |)" case2_haddockComment
  , testCase "CASE 3: Stops at double blank lines" case3_doubleBlank
  , testCase "CASE 4: Stops at next top-level definition" case4_nextTopLevel
  , testCase "CASE 5: Includes where clause and guards" case5_whereClause
  , testCase "CASE 6: Stops at long dash divider (---)" case6_longDashes
  ]

fixturePath :: FilePath
fixturePath = "test/fixtures/CodeBoundaries.hs"

-- | CASE 1: Function followed by section divider
-- simpleFunction haddock starts at line 13, 0-indexed = 12
case1_sectionDivider :: Assertion
case1_sectionDivider = do
  code <- readCodeAtRange fixturePath (Range (Position 12 0) (Position 12 0))
  let expected = T.unlines
        [ "-- | Simple function that should stop at section divider."
        , "simpleFunction :: Int -> Int"
        , "simpleFunction x ="
        , "  let y = x + 1"
        , "      z = y * 2"
        , "  in z + 3"
        , ""
        ]
  assertCodeEquals "simpleFunction" expected code

-- | CASE 2: Function followed by haddock comment
-- functionBeforeHaddock starts at line 24, 0-indexed = 23
case2_haddockComment :: Assertion
case2_haddockComment = do
  code <- readCodeAtRange fixturePath (Range (Position 23 0) (Position 23 0))
  let expected = T.unlines
        [ "functionBeforeHaddock :: String -> String"
        , "functionBeforeHaddock s ="
        , "  case s of"
        , "    \"\" -> \"empty\""
        , "    _ -> \"not empty\""
        , ""
        ]
  assertCodeEquals "functionBeforeHaddock" expected code

-- | CASE 3: Function followed by double blank lines
-- functionWithDoubleBlank starts at line 38, 0-indexed = 37
case3_doubleBlank :: Assertion
case3_doubleBlank = do
  code <- readCodeAtRange fixturePath (Range (Position 37 0) (Position 37 0))
  let expected = T.unlines
        [ "functionWithDoubleBlank :: Bool -> Int"
        , "functionWithDoubleBlank b ="
        , "  if b then 1 else 0"
        , ""
        ]
  assertCodeEquals "functionWithDoubleBlank" expected code

-- | CASE 4: Function followed by next top-level definition
-- multiLineFunction starts at line 50, 0-indexed = 49
case4_nextTopLevel :: Assertion
case4_nextTopLevel = do
  code <- readCodeAtRange fixturePath (Range (Position 49 0) (Position 49 0))
  let expected = T.unlines
        [ "multiLineFunction :: a -> b -> (a, b)"
        , "multiLineFunction a b ="
        , "  let pair = (a, b)"
        , "  in pair"
        ]
  assertCodeEquals "multiLineFunction" expected code

-- | CASE 5: Function with where clause and guards
-- functionWithWhere starts at line 61, 0-indexed = 60
case5_whereClause :: Assertion
case5_whereClause = do
  code <- readCodeAtRange fixturePath (Range (Position 60 0) (Position 60 0))
  let expected = T.unlines
        [ "functionWithWhere :: Int -> Int -> Int"
        , "functionWithWhere x y"
        , "  | x > y = bigger"
        , "  | x < y = smaller"
        , "  | otherwise = equal"
        , "  where"
        , "    bigger = x - y"
        , "    smaller = y - x"
        , "    equal = 0"
        , ""
        ]
  assertCodeEquals "functionWithWhere" expected code

-- | CASE 6: Function followed by long dash divider
-- functionBeforeDashes starts at line 75, 0-indexed = 74
case6_longDashes :: Assertion
case6_longDashes = do
  code <- readCodeAtRange fixturePath (Range (Position 74 0) (Position 74 0))
  let expected = T.unlines
        [ "functionBeforeDashes :: Int"
        , "functionBeforeDashes = 1"
        , ""
        ]
  assertCodeEquals "functionBeforeDashes" expected code

-- | Helper to compare extracted code with expected, showing diff on failure
assertCodeEquals :: String -> Text -> Text -> Assertion
assertCodeEquals name expected actual =
  let expectedLines = T.lines expected
      actualLines = T.lines actual
      -- Trim trailing empty lines for comparison
      trimTrailing = reverse . dropWhile T.null . reverse
      expectedTrimmed = trimTrailing expectedLines
      actualTrimmed = trimTrailing actualLines
  in if expectedTrimmed == actualTrimmed
     then pure ()
     else assertFailure $ unlines
       [ "Mismatch in " ++ name
       , "Expected (" ++ show (length expectedTrimmed) ++ " lines):"
       , T.unpack $ T.unlines expectedTrimmed
       , "Actual (" ++ show (length actualTrimmed) ++ " lines):"
       , T.unpack $ T.unlines actualTrimmed
       ]
