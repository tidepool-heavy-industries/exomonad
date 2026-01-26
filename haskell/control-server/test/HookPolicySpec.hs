{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson (object, (.=), eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL
import ExoMonad.Control.Hook.Policy

main :: IO ()
main = defaultMain spec

spec :: TestTree
spec = testGroup "Hook Policy Tests"
  [ testCase "Default policy allows everything" $ do
      let policy = defaultPolicy
      evaluatePolicy policy "AnyTool" @?= PolicyAllow (Just "Allowed by default policy") Nothing

  , testCase "Rule matches specific tool" $ do
      let policy = HookPolicy
            { rules = [ HookRule "Write" (PolicyDeny "Writing is forbidden") ]
            , defaultDecision = PolicyAllow Nothing Nothing
            }
      evaluatePolicy policy "Write" @?= PolicyDeny "Writing is forbidden"
      evaluatePolicy policy "Read" @?= PolicyAllow Nothing Nothing

  , testCase "First match wins" $ do
      let policy = HookPolicy
            { rules = [ HookRule "Write" (PolicyAllow (Just "Specific allow") Nothing)
                      , HookRule "Write" (PolicyDeny "Specific deny")
                      ]
            , defaultDecision = PolicyDeny "Default deny"
            }
      evaluatePolicy policy "Write" @?= PolicyAllow (Just "Specific allow") Nothing

  , testCase "Wildcard match" $ do
      let policy = HookPolicy
            { rules = [ HookRule "*" (PolicyDeny "Everything is forbidden") ]
            , defaultDecision = PolicyAllow Nothing Nothing
            }
      evaluatePolicy policy "AnyTool" @?= PolicyDeny "Everything is forbidden"

  , testCase "Modify input via policy" $ do
      let modifiedInput = object ["force" .= True]
          policy = HookPolicy
            { rules = [ HookRule "Delete" (PolicyAllow (Just "Enforcing force") (Just modifiedInput)) ]
            , defaultDecision = PolicyAllow Nothing Nothing
            }
      evaluatePolicy policy "Delete" @?= PolicyAllow (Just "Enforcing force") (Just modifiedInput)

  , testCase "JSON deserialization matches CLAUDE.md format" $ do
      let json = BL.unlines
            [ "{"
            , "  \"rules\": ["
            , "    {"
            , "      \"toolPattern\": \"Write\","
            , "      \"decision\": {"
            , "        \"tag\": \"PolicyDeny\","
            , "        \"contents\": \"Writing is disabled\""
            , "      }"
            , "    }"
            , "  ],"
            , "  \"defaultDecision\": {"
            , "    \"tag\": \"PolicyAllow\","
            , "    \"contents\": [\"Allowed by default\", null]"
            , "  }"
            , "}"
            ]
      let expected = HookPolicy
            { rules = [ HookRule "Write" (PolicyDeny "Writing is disabled") ]
            , defaultDecision = PolicyAllow (Just "Allowed by default") Nothing
            }
      
      eitherDecode json @?= Right expected
  ]
