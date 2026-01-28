{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T

import ExoMonad.Control.StopHook.ErrorParser (parseGHCOutput)
import ExoMonad.Control.StopHook.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "GHC Error Parser"
  [ testCase "Parses multi-line type error" $ do
      let input = T.unlines
            [ "[1 of 1] Compiling FailingTest      ( failing-test.hs, failing-test.o )"
            , "failing-test.hs:7:9: error: [GHC-83865]"
            , "    • Couldn't match expected type ‘Text’ with actual type ‘Int’"
            , "    • In the expression: x"
            , "      In an equation for ‘foo’: foo x = x"
            , "  |"
            , "7 | foo x = x"
            , "  |         ^"
            ]
      let errors = parseGHCOutput input
      length errors @?= 1
      let err = head errors
      geFile err @?= "failing-test.hs"
      geLine err @?= 7
      geColumn err @?= 9
      geSeverity err @?= ErrorSeverity
      -- Message should contain the detail
      assertBool "Message should contain detail" $
        "Couldn't match expected type" `T.isInfixOf` geMessage err
      
      -- Check error type classification
      case geErrorType err of
        TypeError (TypeMismatch exp act) -> do
          exp @?= "Text"
          act @?= "Int"
        _ -> assertFailure $ "Expected TypeMismatch, got: " ++ show (geErrorType err)

  , testCase "Parses variable not in scope" $ do
      let input = T.unlines
            [ "failing-test.hs:11:7: error: [GHC-88464]"
            , "    Variable not in scope: undefined_var :: Int"
            , "   |"
            , "11 | bar = undefined_var"
            , "   |       ^^^^^^^^^^^^^"
            ]
      let errors = parseGHCOutput input
      length errors @?= 1
      let err = head errors
      geFile err @?= "failing-test.hs"
      geLine err @?= 11
      
      case geErrorType err of
        ScopeError (VariableNotInScope var) -> var @?= "undefined_var"
        _ -> assertFailure $ "Expected VariableNotInScope, got: " ++ show (geErrorType err)
        
  , testCase "Parses multiple errors" $ do
      let input = T.unlines
            [ "failing-test.hs:7:9: error: [GHC-83865]"
            , "    Type mismatch..."
            , ""
            , "failing-test.hs:11:7: error: [GHC-88464]"
            , "    Variable not in scope..."
            ]
      let errors = parseGHCOutput input
      length errors @?= 2
  ]
