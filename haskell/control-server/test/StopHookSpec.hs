{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Control.StopHook.ErrorParser (parseGHCOutput, classifyError)
import Tidepool.Control.StopHook.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "StopHook"
  [ errorParserTests
  ]

errorParserTests :: TestTree
errorParserTests = testGroup "ErrorParser"
  [ testCase "Parses GHC error line" $ do
      let input = "src/MyFile.hs:10:5: error: Something went wrong"
      let result = parseGHCOutput input
      length result @?= 1
      let err = head result
      geFile err @?= "src/MyFile.hs"
      geLine err @?= 10
      geColumn err @?= 5
      geMessage err @?= "Something went wrong"
      geSeverity err @?= ErrorSeverity

  , testCase "Parses GHC warning line" $ do
      let input = "src/MyFile.hs:10:5: warning: Careful now"
      let result = parseGHCOutput input
      length result @?= 1
      let err = head result
      geSeverity err @?= WarningSeverity

  , testCase "Parses multiple lines" $ do
      let input = "src/A.hs:1:1: error: A\nsrc/B.hs:2:2: error: B"
      let result = parseGHCOutput input
      length result @?= 2

  , testCase "Classifies TypeMismatch" $ do
      let msg = "Couldn't match type 'Int' with 'Bool'"
      case classifyError msg of
        TypeError (TypeMismatch exp' act) -> do
          exp' @?= "Int"
          act @?= "Bool"
        _ -> assertFailure "Expected TypeMismatch"

  , testCase "Classifies TypeMismatch with smart quotes" $ do
      -- GHC often uses smart quotes
      let msg = "Couldn't match type ‘Int’ with ‘Bool’"
      case classifyError msg of
        TypeError (TypeMismatch exp' act) -> do
          exp' @?= "Int"
          act @?= "Bool"
        _ -> assertFailure "Expected TypeMismatch (smart quotes)"

  , testCase "Classifies VariableNotInScope" $ do
      -- Test specific case
      let msg2 = "Not in scope: 'foo'"
      case classifyError msg2 of
        ScopeError (VariableNotInScope var) -> var @?= "foo"
        _ -> assertFailure "Expected VariableNotInScope"
  ]
