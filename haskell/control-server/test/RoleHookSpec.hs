{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (spec, main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)
import Data.Aeson (toJSON, Value(..))
import Control.Monad.Freer (run)
import qualified Data.Text as T

import ExoMonad.Control.Role.Hook.Dispatch (dispatchHook, HookDispatchResult(..))
import ExoMonad.Control.Role.Hook.Definitions (CommonHooks(..), TLHooks(..))
import ExoMonad.Graph.Generic (AsHandler, HookHandler(..))
import ExoMonad.Control.Role.Types
  ( SessionStartInput(..)
  , SessionStartResponse(..)
  )

main :: IO ()
main = defaultMain spec

-- Mock Handlers
mockHandlers :: TLHooks (AsHandler '[])
mockHandlers = TLHooks
  { common = CommonHooks
      { sessionStart = HookHandler $ \_ -> pure $ SessionStartResponse True (Just "Mock Context")
      , preToolUse   = HookHandler $ \_ -> pure undefined
      , postToolUse  = HookHandler $ \_ -> pure ()
      , stop         = HookHandler $ \_ -> pure undefined
      , sessionEnd   = HookHandler $ \_ -> pure ()
      , notification = HookHandler $ \_ -> pure ()
      , subagentStop = HookHandler $ \_ -> pure ()
      }
  }

spec :: TestTree
spec = testGroup "Role Hook Dispatch"
  [ testCase "Dispatches session_start correctly" $ do
      let input = SessionStartInput "test-session" "/tmp" []
          inputJson = toJSON input
      
      let result = dispatchHook mockHandlers "session_start" inputJson
      
      case result of
        HookFound action -> do
          let val = run action
          -- We expect JSON output of SessionStartResponse
          let expected = toJSON $ SessionStartResponse True (Just "Mock Context")
          assertEqual "Handler output matches" expected val
        _ -> assertBool "Expected HookFound" False

  , testCase "Returns HookNotFound for unknown event" $ do
      let result = dispatchHook mockHandlers "unknown_event" Null
      case result of
        HookNotFound -> pure ()
        _ -> assertBool "Expected HookNotFound" False

  , testCase "Returns HookParseError for invalid input" $ do
      let result = dispatchHook mockHandlers "session_start" (String "invalid")
      case result of
        HookParseError _ -> pure ()
        _ -> assertBool "Expected HookParseError" False
  ]
