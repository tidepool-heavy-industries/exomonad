{-# LANGUAGE DeriveAnyClass #-}

-- | Integration test for ClaudeCode decision tools.
--
-- This test spawns a REAL mantle session with decision tools and verifies
-- that Claude Code calls one of the tools and the result parses correctly.
--
-- = Prerequisites
--
-- * Docker running
-- * @~/.claude/@ has valid OAuth (from normal @claude@ CLI usage)
-- * @mantle@ binary on PATH
--
-- = Running
--
-- @
-- cabal test session-executor --test-option="--match=/Decision Tools E2E/"
-- @
module Tidepool.Session.DecisionToolsIntegrationSpec (spec) where

import Data.Aeson (toJSON)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Test.Hspec

import Tidepool.Effect.Session (ToolCall(..), SessionOutput(..))
import Tidepool.Graph.Types (ModelChoice(..))
import Tidepool.Session.Executor (startSessionIO, defaultSessionConfig)
import Tidepool.StructuredOutput ()  -- Bring StructuredOutput instances into scope
import Tidepool.StructuredOutput.DecisionTools
  ( ToDecisionTools(..)
  , toDecisionTools
  , parseToolCall
  )
import qualified Tidepool.StructuredOutput.DecisionTools as DT


-- ════════════════════════════════════════════════════════════════════════════
-- TEST SUM TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | Sum type for testing decision tools.
--
-- Claude Code will be given MCP tools:
-- * @decision::approve@ with params @{ approveNotes }@
-- * @decision::reject@ with params @{ rejectReason }@
--
-- Field names have constructor prefix for automatic stripping.
data ReviewDecision
  = Approve { approveNotes :: Text }
  | Reject { rejectReason :: Text }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToDecisionTools)


-- ════════════════════════════════════════════════════════════════════════════
-- HELPER
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert Session.ToolCall to DecisionTools.ToolCall.
--
-- They have the same structure, just in different modules.
convertToolCall :: ToolCall -> DT.ToolCall
convertToolCall (ToolCall name input) = DT.ToolCall
  { DT.tcName = name
  , DT.tcInput = input
  }


-- ════════════════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════════════════

spec :: Spec
spec = describe "Decision Tools E2E" $ do

  it "Claude calls decision tool and result parses to sum type" $ do
    let config = defaultSessionConfig "."
        tools = toDecisionTools @ReviewDecision
        toolsJson = toJSON tools
        prompt = T.unlines
          [ "This is a test. You have decision tools available."
          , "Please call decision::approve with notes saying 'test passed'"
          , "or decision::reject with a reason."
          , ""
          , "Call one of the decision:: tools now."
          ]

    -- Run real mantle session with decision tools
    result <- startSessionIO config "test/decision-e2e" prompt Sonnet Nothing (Just toolsJson)

    -- Extract tool calls using pattern matching (NoFieldSelectors in use)
    let SessionOutput { soToolCalls = mToolCalls } = result

    -- Should have tool calls
    mToolCalls `shouldSatisfy` isJust

    -- Should have exactly one tool call
    let toolCalls = fromJust mToolCalls
    length toolCalls `shouldBe` 1

    -- Parse the tool call back to our sum type
    let tc = head toolCalls
    case parseToolCall @ReviewDecision (convertToolCall tc) of
      Right (Approve n) -> do
        putStrLn $ "Got Approve with notes: " <> T.unpack n
        n `shouldSatisfy` (not . T.null)
      Right (Reject r) -> do
        putStrLn $ "Got Reject with reason: " <> T.unpack r
        r `shouldSatisfy` (not . T.null)
      Left err ->
        expectationFailure $ "Failed to parse tool call: " <> err
