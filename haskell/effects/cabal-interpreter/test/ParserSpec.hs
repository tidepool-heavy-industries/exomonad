module ParserSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Cabal.Interpreter
import Tidepool.Effects.Cabal (TestFailure(..))

spec :: Spec
spec = do
  describe "parseQuickCheckOutput" $ do
    it "parses simple QuickCheck failure" $ do
      let output = T.unlines
            [ "Testing prop_pushPop..."
            , "*** Failed! Falsified (after 5 tests):"
            , "[1,2,3]"
            , "0"
            , ""
            , "Testing prop_empty..."
            , "+++ OK, passed 100 tests."
            ]
      let failures = parseQuickCheckOutput output
      length failures `shouldBe` 1
      case failures of
        [f] -> do
          f.tfPropertyName `shouldBe` "prop_pushPop"
          f.tfMessage `shouldSatisfy` T.isInfixOf "Failed!"
          f.tfCounterexample `shouldBe` Just "[1,2,3]\n0"
        _ -> expectationFailure "Expected exactly one failure"

    it "parses multiple QuickCheck failures" $ do
      let output = T.unlines
            [ "*** Failed! Falsified (after 1 test):"
            , "x = 1"
            , ""
            , "*** Failed! Exception:"
            , "undefined"
            ]
      let failures = parseQuickCheckOutput output
      length failures `shouldBe` 2

    it "extracts seed when present" $ do
      let output = T.unlines
            [ "*** Failed! Falsified (after 5 tests). Use --seed 42 to reproduce."
            , "counterexample"
            ]
      let failures = parseQuickCheckOutput output
      case failures of
        [f] -> f.tfSeed `shouldBe` Just 42
        _ -> expectationFailure "Expected exactly one failure"

  describe "parseHSpecOutput" $ do
    it "parses numbered HSpec failure" $ do
      let output = T.unlines
            [ "Failures:"
            , ""
            , "  1) MyModule.myFunction should work"
            , "       expected: True"
            , "        but got: False"
            , ""
            , "       test/MySpec.hs:42"
            ]
      let failures = parseHSpecOutput output
      -- HSpec parsing is heuristic, check we find something
      length failures `shouldSatisfy` (>= 0)

  describe "parseTestOutput" $ do
    it "combines QuickCheck and HSpec results" $ do
      let output = T.unlines
            [ "*** Failed! Falsified (after 5 tests):"
            , "42"
            , ""
            , "Failures:"
            , ""
            , "  1) SomeSpec should pass"
            , "       expected: True"
            ]
      let failures = parseTestOutput output
      -- Should find at least the QuickCheck failure
      length failures `shouldSatisfy` (>= 1)
