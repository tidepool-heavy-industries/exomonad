{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Tests for ChatHistory compression feature.
--
-- These tests verify:
-- 1. Token estimation functions work correctly
-- 2. Compression triggers when threshold exceeded
-- 3. Recent messages are preserved after compression
-- 4. All content block types are formatted correctly
--
-- All tests run locally without API calls by mocking the LLM effect.
module ChatHistoryCompressionSpec (spec) where

import Data.Aeson (Value(..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Test.Hspec

import Tidepool.Anthropic.Types
  ( ContentBlock(..), Message(..), Role(..)
  , ThinkingContent(..), RedactedThinking(..)
  , ToolUse(..)
  )
import qualified Tidepool.Anthropic.Types as AT
import Tidepool.Effect.Types
  ( LLM(..), Log(..), Random(..), RequestInput(..), ChatHistory
  , TurnOutcome(..), TurnResult(..), LLMConfig(..)
  , ToolResult(..)
  , estimateTokens, estimateBlockChars
  , getHistory, appendMessages
  )
import Tidepool.Effect.Runners
  ( ChatHistoryConfig(..), CompressionConfig(..)
  , runChatHistoryWithCompression
  , defaultCompressionPrompt, defaultCompressionSchema
  )


-- ════════════════════════════════════════════════════════════════════════════
-- MOCK EFFECT INTERPRETERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Mock LLM interpreter that returns a fixed summary.
-- Used for compression tests where we need deterministic output.
runMockLLM :: Text -> Eff (LLM : es) a -> Eff es a
runMockLLM summaryText = interpret $ \_ -> \case
  RunTurnOp _sysPrompt _userContent _schema _tools ->
    pure $ TurnCompleted TurnResult
      { trOutput = object ["summary" .= summaryText]
      , trToolsInvoked = []
      , trNarrative = summaryText
      , trThinking = ""
      }

-- | Mock LLM that returns TurnBroken
runMockLLMBroken :: Text -> Eff (LLM : es) a -> Eff es a
runMockLLMBroken reason = interpret $ \_ -> \case
  RunTurnOp _sysPrompt _userContent _schema _tools ->
    pure $ TurnBroken reason

-- | Mock LLM that returns output without a summary key (tests fallback to narrative)
runMockLLMNoSummary :: Text -> Eff (LLM : es) a -> Eff es a
runMockLLMNoSummary narrativeText = interpret $ \_ -> \case
  RunTurnOp _sysPrompt _userContent _schema _tools ->
    pure $ TurnCompleted TurnResult
      { trOutput = object ["irrelevant_key" .= ("ignored" :: Text)]
      , trToolsInvoked = []
      , trNarrative = narrativeText
      , trThinking = ""
      }

-- | Mock LLM that returns output with multiple candidate keys
-- Tests extractSummary priority
runMockLLMMultipleKeys :: Eff (LLM : es) a -> Eff es a
runMockLLMMultipleKeys = interpret $ \_ -> \case
  RunTurnOp _sysPrompt _userContent _schema _tools ->
    pure $ TurnCompleted TurnResult
      { trOutput = object
          [ "summary" .= ("from_summary" :: Text)
          , "content" .= ("from_content" :: Text)
          ]
      , trToolsInvoked = []
      , trNarrative = "from_narrative"
      , trThinking = ""
      }

-- | Mock Log interpreter that discards all logs.
runLogDiscard :: Eff (Log : es) a -> Eff es a
runLogDiscard = interpret $ \_ -> \case
  LogMsg _level _msg -> pure ()

-- | Mock Random interpreter with deterministic output.
runRandomFixed :: Eff (Random : es) a -> Eff es a
runRandomFixed = interpret $ \_ -> \case
  RandomInt lo _hi -> pure lo  -- Always return lower bound
  RandomDouble -> pure 0.5     -- Always return 0.5

-- | Mock RequestInput interpreter (stub - not used in compression).
runRequestInputStub :: Eff (RequestInput : es) a -> Eff es a
runRequestInputStub = interpret $ \_ -> \case
  RequestChoice _prompt choices -> case choices of
    ((_, a):_) -> pure a
    [] -> error "RequestChoice with no choices"
  RequestText _prompt -> pure ""
  RequestTextWithPhoto _prompt -> pure ("", [])
  RequestDice _prompt diceWithHints -> case diceWithHints of
    ((val, _, _):_) -> pure val
    [] -> pure 0
  RequestCustom _tag _payload -> pure Null


-- ════════════════════════════════════════════════════════════════════════════
-- TEST CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Create a test LLM config (values don't matter for mocked tests).
testLLMConfig :: LLMConfig
testLLMConfig = LLMConfig
  { llmApiKey = "test-key"
  , llmModel = "test-model"
  , llmMaxTokens = 1000
  , llmThinkingBudget = Nothing
  }

-- | Create a compression config with the given threshold and recent count.
mkTestConfig :: Int -> Int -> ChatHistoryConfig es
mkTestConfig threshold recentCount = ChatHistoryConfig
  { chcTokenThreshold = threshold
  , chcRecentToKeep = recentCount
  , chcCompression = CompressionConfig
      { ccLLMConfig = testLLMConfig
      , ccPrompt = defaultCompressionPrompt
      , ccSchema = defaultCompressionSchema
      , ccTools = []
      , ccDispatcher = \_ _ -> pure $ Right (ToolSuccess Null)
      }
  }


-- ════════════════════════════════════════════════════════════════════════════
-- TEST RUNNER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a ChatHistory action with compression enabled.
-- Uses mocked effects to avoid any real API calls.
--
-- The effect stack is:
--   ChatHistory : LLM : Log : Random : RequestInput : IOE : '[]
--
-- This satisfies the NotMember ChatHistory constraint because the base
-- stack (LLM : Log : Random : RequestInput : IOE : '[]) doesn't contain ChatHistory.
runCompressionTest
  :: Int  -- ^ Token threshold
  -> Int  -- ^ Recent messages to keep
  -> Text -- ^ Mock summary text for compression
  -> Eff '[ChatHistory, LLM, Log, Random, RequestInput, IOE] a
  -> IO a
runCompressionTest threshold recentCount mockSummary action =
  runEff
    $ runRequestInputStub
    $ runRandomFixed
    $ runLogDiscard
    $ runMockLLM mockSummary
    $ runChatHistoryWithCompression (mkTestConfig threshold recentCount)
    $ action

-- | Run with a custom LLM mock
runWithMockLLM
  :: (forall es. Eff (LLM : es) a -> Eff es a)
  -> Int -> Int
  -> Eff '[ChatHistory, LLM, Log, Random, RequestInput, IOE] a
  -> IO a
runWithMockLLM mockLLM threshold recentCount action =
  runEff
    $ runRequestInputStub
    $ runRandomFixed
    $ runLogDiscard
    $ mockLLM
    $ runChatHistoryWithCompression (mkTestConfig threshold recentCount)
    $ action


-- ════════════════════════════════════════════════════════════════════════════
-- HELPER FUNCTIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Create a simple user message with text content.
userMsg :: Text -> Message
userMsg t = Message User [TextBlock t]

-- | Create a simple assistant message with text content.
assistantMsg :: Text -> Message
assistantMsg t = Message Assistant [TextBlock t]


-- ════════════════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  describe "Token estimation (pure functions)" $ do
    describe "estimateTokens" $ do
      it "returns 0 for empty message list" $ do
        estimateTokens [] `shouldBe` 0

      it "estimates ~1 token per 4 characters" $ do
        -- 12 characters = 3 tokens
        let msg = userMsg "hello world!"  -- 12 chars
        estimateTokens [msg] `shouldBe` 3

      it "estimates correctly for single character" $ do
        -- 1 character = 0 tokens (integer division)
        let msg = userMsg "a"
        estimateTokens [msg] `shouldBe` 0

      it "estimates correctly for exactly 4 characters" $ do
        -- 4 characters = 1 token
        let msg = userMsg "abcd"
        estimateTokens [msg] `shouldBe` 1

      it "sums across multiple messages" $ do
        -- 8 + 8 = 16 chars = 4 tokens
        let msgs = [userMsg "aaaabbbb", assistantMsg "ccccdddd"]
        estimateTokens msgs `shouldBe` 4

      it "handles large messages" $ do
        -- 1000 characters = 250 tokens
        let largeText = T.replicate 1000 "x"
        let msg = userMsg largeText
        estimateTokens [msg] `shouldBe` 250

    describe "estimateBlockChars" $ do
      it "counts TextBlock characters" $ do
        estimateBlockChars (TextBlock "hello") `shouldBe` 5

      it "returns 1000 for ImageBlock" $ do
        -- Images get a fixed placeholder estimate
        let imgSrc = error "ImageSource not evaluated"
        estimateBlockChars (ImageBlock imgSrc) `shouldBe` 1000

      it "counts ToolUseBlock name + input" $ do
        let toolUse = ToolUse
              { toolUseId = "id"
              , toolName = "test_tool"  -- 9 chars
              , toolInput = object ["key" .= ("val" :: Text)]  -- {"key":"val"} = 13 chars
              }
        -- 9 + 13 = 22
        estimateBlockChars (ToolUseBlock toolUse) `shouldBe` 22

      it "counts ToolResultBlock content" $ do
        let toolResult = AT.ToolResult
              { AT.toolResultId = "id"
              , AT.toolResultContent = "result text"  -- 11 chars
              , AT.toolResultIsError = False
              }
        estimateBlockChars (ToolResultBlock toolResult) `shouldBe` 11

      it "counts ThinkingBlock text" $ do
        let thinking = ThinkingContent
              { thinkingText = "thinking..."  -- 11 chars
              , thinkingSignature = "sig"
              }
        estimateBlockChars (ThinkingBlock thinking) `shouldBe` 11

      it "returns 100 for RedactedThinkingBlock" $ do
        let redacted = RedactedThinking "data"
        estimateBlockChars (RedactedThinkingBlock redacted) `shouldBe` 100

      it "counts JsonBlock via encoded length" $ do
        -- {"x":1} = 7 characters
        let json = object ["x" .= (1 :: Int)]
        estimateBlockChars (JsonBlock json) `shouldBe` 7

  describe "Compression triggering" $ do
    it "does NOT compress when under threshold" $ do
      result <- runCompressionTest 10000 2 "Mock summary" $ do
        appendMessages [userMsg "short"]
        getHistory
      length result `shouldBe` 1

    it "compresses when over threshold" $ do
      -- 100 char message = 25 tokens, threshold = 10
      let longMsg = userMsg (T.replicate 100 "x")
      result <- runCompressionTest 10 2 "Compressed history" $ do
        appendMessages [longMsg, longMsg, longMsg, longMsg]
        getHistory
      -- After compression: 1 summary + 2 recent = 3 messages
      length result `shouldBe` 3

    it "compresses with exact boundary: threshold+1 triggers compression" $ do
      -- 44 chars = 11 tokens, threshold = 10
      let msg = userMsg (T.replicate 44 "x")
      result <- runCompressionTest 10 1 "Summary" $ do
        appendMessages [msg, msg]
        getHistory
      -- With recentToKeep=1, should have 1 summary + 1 recent
      length result `shouldBe` 2

    it "does NOT compress at exact threshold" $ do
      -- 40 chars = 10 tokens = threshold, no compression
      let msg = userMsg (T.replicate 40 "x")
      result <- runCompressionTest 10 1 "Summary" $ do
        appendMessages [msg]
        getHistory
      length result `shouldBe` 1

  describe "Message preservation" $ do
    it "preserves recent N messages verbatim after compression" $ do
      -- Use longer messages to ensure compression triggers
      -- Each 40-char message = 10 tokens, 4 messages = 40 tokens
      let msg1 = userMsg (T.replicate 40 "a")
      let msg2 = assistantMsg (T.replicate 40 "b")
      let msg3 = userMsg (T.replicate 40 "c")
      let msg4 = assistantMsg (T.replicate 40 "d")
      -- Trigger compression with threshold < 40 tokens
      result <- runCompressionTest 20 2 "Compressed" $ do
        appendMessages [msg1, msg2, msg3, msg4]
        getHistory
      -- After compression: 1 summary + 2 recent = 3 total
      -- Last 2 should be preserved exactly
      case drop 1 result of
        [r1, r2] -> do
          r1 `shouldBe` msg3
          r2 `shouldBe` msg4
        other -> expectationFailure $
          "Expected 2 preserved messages, got " ++ show (length other)

    it "does not compress if fewer messages than recentToKeep" $ do
      result <- runCompressionTest 1 10 "Summary" $ do
        appendMessages [userMsg "only one"]
        getHistory
      -- Even though threshold is low (1), recentToKeep=10 means nothing to compress
      length result `shouldBe` 1

    it "keeps all messages when recentToKeep >= message count" $ do
      let msgs = [userMsg "a", userMsg "b", userMsg "c"]
      result <- runCompressionTest 1 5 "Summary" $ do
        appendMessages msgs
        getHistory
      length result `shouldBe` 3

  describe "Summary message format" $ do
    it "creates an Assistant message with compressed history prefix" $ do
      let msgs = [userMsg (T.replicate 100 "x"), userMsg "keep"]
      result <- runCompressionTest 5 1 "The summary text" $ do
        appendMessages msgs
        getHistory
      case result of
        (summary:_) -> do
          summary.role `shouldBe` Assistant
          case summary.content of
            [TextBlock t] -> do
              T.isInfixOf "[Compressed history]" t `shouldBe` True
              T.isInfixOf "The summary text" t `shouldBe` True
            other -> expectationFailure $
              "Expected single TextBlock, got " ++ show (length other) ++ " blocks"
        [] -> expectationFailure "Expected at least one message"

  describe "Multiple compressions" $ do
    it "can compress multiple times as history grows" $ do
      result <- runCompressionTest 5 1 "Compressed" $ do
        -- First batch triggers compression
        appendMessages [userMsg (T.replicate 100 "x")]
        appendMessages [userMsg (T.replicate 100 "y")]
        -- Second batch triggers compression again
        appendMessages [userMsg (T.replicate 100 "z")]
        getHistory
      -- Multiple compressions should result in a small history
      length result `shouldSatisfy` (<= 3)

  describe "getHistory and appendMessages" $ do
    it "getHistory returns empty initially" $ do
      result <- runCompressionTest 10000 2 "Summary" getHistory
      result `shouldBe` []

    it "appendMessages adds messages to history" $ do
      let msg = userMsg "test"
      result <- runCompressionTest 10000 2 "Summary" $ do
        appendMessages [msg]
        getHistory
      result `shouldBe` [msg]

    it "appendMessages preserves message order" $ do
      let msg1 = userMsg "first"
      let msg2 = assistantMsg "second"
      let msg3 = userMsg "third"
      result <- runCompressionTest 10000 5 "Summary" $ do
        appendMessages [msg1, msg2, msg3]
        getHistory
      result `shouldBe` [msg1, msg2, msg3]

  describe "LLM failure handling" $ do
    it "handles TurnBroken by creating placeholder message" $ do
      let longMsg = userMsg (T.replicate 100 "x")
      result <- runWithMockLLM (runMockLLMBroken "tool interrupted") 5 1 $ do
        appendMessages [longMsg, longMsg]
        getHistory
      case result of
        (summary:_) -> do
          case summary.content of
            [TextBlock t] -> do
              T.isInfixOf "[Compressed history - interrupted:" t `shouldBe` True
              T.isInfixOf "tool interrupted" t `shouldBe` True
            _ -> expectationFailure "Expected single TextBlock"
        [] -> expectationFailure "Expected at least one message"

    it "falls back to narrative when output has no summary key" $ do
      let longMsg = userMsg (T.replicate 100 "x")
      result <- runWithMockLLM (runMockLLMNoSummary "narrative fallback text") 5 1 $ do
        appendMessages [longMsg, longMsg]
        getHistory
      case result of
        (summary:_) -> do
          case summary.content of
            [TextBlock t] -> do
              T.isInfixOf "[Compressed history]" t `shouldBe` True
              T.isInfixOf "narrative fallback text" t `shouldBe` True
            _ -> expectationFailure "Expected single TextBlock"
        [] -> expectationFailure "Expected at least one message"

  describe "extractSummary priority" $ do
    it "prefers 'summary' key over 'content' key" $ do
      -- foldr with early return finds leftmost match first
      let longMsg = userMsg (T.replicate 100 "x")
      result <- runWithMockLLM runMockLLMMultipleKeys 5 1 $ do
        appendMessages [longMsg, longMsg]
        getHistory
      case result of
        (summary:_) -> do
          case summary.content of
            [TextBlock t] ->
              T.isInfixOf "from_summary" t `shouldBe` True
            _ -> expectationFailure "Expected single TextBlock"
        [] -> expectationFailure "Expected at least one message"

  describe "Edge cases" $ do
    it "recentToKeep=0 compresses all messages, keeps only summary" $ do
      let msg1 = userMsg (T.replicate 40 "a")
      let msg2 = userMsg (T.replicate 40 "b")
      result <- runCompressionTest 10 0 "All compressed" $ do
        appendMessages [msg1, msg2]
        getHistory
      -- With recentToKeep=0, toKeep=[], only summary remains
      length result `shouldBe` 1
      case result of
        [summary] -> do
          summary.role `shouldBe` Assistant
          case summary.content of
            [TextBlock t] -> T.isInfixOf "All compressed" t `shouldBe` True
            _ -> expectationFailure "Expected single TextBlock"
        _ -> expectationFailure "Expected exactly one message"

    it "very low threshold triggers compression on single message" $ do
      -- 40 chars = 10 tokens, threshold = 5
      let msg = userMsg (T.replicate 40 "x")
      result <- runCompressionTest 5 0 "Single msg compressed" $ do
        appendMessages [msg]
        getHistory
      -- Single message compressed with recentToKeep=0
      length result `shouldBe` 1
      case result of
        [summary] -> summary.role `shouldBe` Assistant
        _ -> expectationFailure "Expected exactly one message"
