{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (Value (..), eitherDecode)
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V
import ExoMonad.Control.TLTools.Types (Component (..), IssueCategory (..), Priority (..), Severity (..))
import ExoMonad.Schema (HasJSONSchema (..), schemaToValue)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Enum Schema Tests"
    [ testGroup
        "Schema includes enum values"
        [ testCase "Component schema has correct enum values" $ do
            let schema = schemaToValue (jsonSchema @Component)
            case schema of
              Object obj -> case KM.lookup "enum" obj of
                Just (Array arr) -> do
                  let values = [t | String t <- V.toList arr]
                  values @?= ["controlserver", "dsl", "tui", "docker", "hooks", "mcp", "zellij", "other"]
                Just other -> assertFailure $ "enum field is not an array: " ++ show other
                Nothing -> assertFailure "Schema missing 'enum' field"
              _ -> assertFailure "Schema is not an object",
          testCase "Priority schema has correct enum values" $ do
            let schema = schemaToValue (jsonSchema @Priority)
            case schema of
              Object obj -> case KM.lookup "enum" obj of
                Just (Array arr) -> do
                  let values = [t | String t <- V.toList arr]
                  values @?= ["p0", "p1", "p2", "p3"]
                Just other -> assertFailure $ "enum field is not an array: " ++ show other
                Nothing -> assertFailure "Schema missing 'enum' field"
              _ -> assertFailure "Schema is not an object",
          testCase "Severity schema has correct enum values" $ do
            let schema = schemaToValue (jsonSchema @Severity)
            case schema of
              Object obj -> case KM.lookup "enum" obj of
                Just (Array arr) -> do
                  let values = [t | String t <- V.toList arr]
                  values @?= ["critical", "high", "medium", "low"]
                Just other -> assertFailure $ "enum field is not an array: " ++ show other
                Nothing -> assertFailure "Schema missing 'enum' field"
              _ -> assertFailure "Schema is not an object",
          testCase "IssueCategory schema has correct enum values" $ do
            let schema = schemaToValue (jsonSchema @IssueCategory)
            case schema of
              Object obj -> case KM.lookup "enum" obj of
                Just (Array arr) -> do
                  let values = [t | String t <- V.toList arr]
                  values @?= ["bug", "feature", "refactor", "docs", "experiment"]
                Just other -> assertFailure $ "enum field is not an array: " ++ show other
                Nothing -> assertFailure "Schema missing 'enum' field"
              _ -> assertFailure "Schema is not an object"
        ],
      testGroup
        "Parse errors include valid values"
        [ testCase "Component parse error lists valid values" $ do
            let result = eitherDecode "\"invalid\"" :: Either String Component
            case result of
              Left err -> do
                assertBool "Error should mention 'Valid values'" ("Valid values:" `isInfixOf` err)
                assertBool "Error should list controlserver" ("controlserver" `isInfixOf` err)
                assertBool "Error should list other" ("other" `isInfixOf` err)
              Right _ -> assertFailure "Should have failed to parse 'invalid'",
          testCase "Priority parse error lists valid values" $ do
            let result = eitherDecode "\"invalid\"" :: Either String Priority
            case result of
              Left err -> do
                assertBool "Error should mention 'Valid values'" ("Valid values:" `isInfixOf` err)
                assertBool "Error should list p0" ("p0" `isInfixOf` err)
                assertBool "Error should list p3" ("p3" `isInfixOf` err)
              Right _ -> assertFailure "Should have failed to parse 'invalid'"
        ]
    ]

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

    tails [] = [[]]
    tails xs@(_ : xs') = xs : tails xs'
