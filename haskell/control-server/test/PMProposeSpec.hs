{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Freer (Eff, run, reinterpret)
import Control.Monad.Freer.State (get, put, modify, runState)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Tidepool.Effects.BD (BD(..), CreateBeadInput(..), BeadType(..))
import Tidepool.Control.PMTools (labelNeedsTLReview)
import Tidepool.Control.PMPropose
import Tidepool.Graph.Goto (Goto, GotoChoice(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PMPropose"
  [ testCase "creates bead with correct details" test_createsBead
  , testCase "includes needs-tl-review label" test_includesLabel
  , testCase "formats description correctly" test_formatsDescription
  ]

-- | Mock State to capture created beads
data MockState = MockState
  { createdBeads :: [CreateBeadInput]
  }

initialState :: MockState
initialState = MockState []

-- | Mock BD interpreter
runMockBD :: Eff '[BD] a -> (a, MockState)
runMockBD eff = run $ runState initialState $ reinterpret (\case
  CreateBead input -> do
    modify $ \s -> s { createdBeads = input : createdBeads s }
    pure "bd-new"
  -- Minimal implementations for other methods needed for compilation/runtime if hit
  GetLabels _ -> pure []
  AddLabel _ _ -> pure ()
  RemoveLabel _ _ -> pure ()
  ListByStatus _ -> pure []
  ListByType _ -> pure []
  GetBead _ -> pure Nothing
  GetDeps _ -> pure []
  GetBlocking _ -> pure []
  GetChildren _ -> pure []
  UpdateBead _ _ -> pure ()
  CloseBead _ -> pure ()
  ReopenBead _ -> pure ()
  AddDep _ _ _ -> pure ()
  RemoveDep _ _ -> pure ()
  Sync -> pure ()
  ) eff

test_createsBead :: Assertion
test_createsBead = do
  let args = PMProposeArgs
        { ppaTitle = "My Feature"
        , ppaIntent = "Implement X"
        , ppaSuggestedPriority = Just 1
        , ppaSuggestedLabels = Just ["frontend"]
        , ppaContext = Nothing
        , ppaScopeHint = Nothing
        }
  
  -- We need to run the logic wrapped in the effect stack.
  -- pmProposeLogic returns a GotoChoice, we just ignore the return value for this test
  -- as we are inspecting the side effects (CreateBead call).
  let (res, state) = runMockBD $ pmProposeLogic args
  
  case createdBeads state of
    [input] -> do
      assertEqual "Title matches" "My Feature" input.cbiTitle
      assertEqual "Priority matches" 1 input.cbiPriority
      assertEqual "Type defaults to Task" TypeTask input.cbiType
    [] -> assertFailure "No bead created"
    _ -> assertFailure "Multiple beads created"

test_includesLabel :: Assertion
test_includesLabel = do
  let args = PMProposeArgs
        { ppaTitle = "My Feature"
        , ppaIntent = "Implement X"
        , ppaSuggestedPriority = Nothing
        , ppaSuggestedLabels = Just ["frontend"]
        , ppaContext = Nothing
        , ppaScopeHint = Nothing
        }
  
  let (_, state) = runMockBD $ pmProposeLogic args
  
  case createdBeads state of
    [input] -> do
      assertBool "Includes needs-tl-review" (labelNeedsTLReview `elem` input.cbiLabels)
      assertBool "Includes suggested label" ("frontend" `elem` input.cbiLabels)
    _ -> assertFailure "Expected one bead created"

test_formatsDescription :: Assertion
test_formatsDescription = do
  let args = PMProposeArgs
        { ppaTitle = "My Feature"
        , ppaIntent = "Do the thing"
        , ppaSuggestedPriority = Nothing
        , ppaSuggestedLabels = Nothing
        , ppaContext = Just "Because user said so"
        , ppaScopeHint = Just "Small"
        }
  
  let (_, state) = runMockBD $ pmProposeLogic args
  
  case createdBeads state of
    [input] -> do
      let desc = input.cbiDescription
      case desc of
        Just d -> do
          assertBool "Contains intent" ("Do the thing" `T.isInfixOf` d)
          assertBool "Contains context header" ("## Context" `T.isInfixOf` d)
          assertBool "Contains context body" ("Because user said so" `T.isInfixOf` d)
          assertBool "Contains scope header" ("## Scope Hint" `T.isInfixOf` d)
          assertBool "Contains scope body" ("Small" `T.isInfixOf` d)
        Nothing -> assertFailure "Description should not be empty"
    _ -> assertFailure "Expected one bead created"
