{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Mock LLM interpreters for testing graph logic without real LLM calls.
--
-- This module provides several mock interpreters for the LLM effect:
--
-- * 'runMockLLM' - Returns a fixed output for all LLM calls
-- * 'runMockLLMSequence' - Returns outputs from a list in order
-- * 'runMockLLMMatched' - Returns outputs based on schema matching
-- * 'runMockLLMCapture' - Captures requests for test assertions
--
-- @
-- spec = do
--   it "handles LLM response" $ do
--     let result = run $ runMockLLM (object ["status" .= "ok"]) $ do
--           llmCall \@MyOutput "system" "user" schema
--     result.status \`shouldBe\` "ok"
--
--   it "verifies prompts sent" $ do
--     let (requests, _) = run $ runMockLLMCapture (object []) $ runGraph handlers input
--     case requests of
--       (req:_) -> req.lrSystemPrompt \`shouldContain\` "classify"
--       []      -> expectationFailure "expected at least one request"
-- @
module Test.ExoMonad.MockLLM
  ( -- * Simple Mocks
    runMockLLM,
    runMockLLMSequence,

    -- * Schema-Matched Mock
    runMockLLMMatched,

    -- * Capturing Mock
    runMockLLMCapture,
    LLMRequest (..),

    -- * Schema Matching Helpers
    schemaHasField,
    schemaHasId,
  )
where

import Data.Aeson (Value (..))
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import ExoMonad.Effect.Types
  ( ContentBlock,
    LLM (..),
    TurnResult (..),
  )
import Polysemy (Member, Sem, interpret, raise, reinterpret)
import Polysemy.State (State, evalState, get, put)
import Polysemy.Writer (Writer, runWriter, tell)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Captured LLM request for test assertions.
--
-- Contains all parameters passed to the LLM effect, allowing tests to verify
-- that the correct prompts, schemas, and tools were used.
data LLMRequest = LLMRequest
  { lrSystemPrompt :: !Text,
    lrUserContent :: !(NonEmpty ContentBlock),
    lrSchema :: !Value,
    lrTools :: ![Value]
  }
  deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
-- SIMPLE MOCKS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run LLM with a fixed output for all calls.
--
-- This is the simplest mock - all LLM calls return the same value.
--
-- @
-- let result = run $ runMockLLM (object ["ok" .= True]) computation
-- @
runMockLLM :: Value -> Sem (LLM ': effs) a -> Sem effs a
runMockLLM fixedOutput = interpret $ \case
  RunTurnOp _sys _content _schema _tools ->
    pure $
      TurnResult
        { trOutput = fixedOutput,
          trToolsInvoked = [],
          trNarrative = "",
          trThinking = ""
        }

-- | Run LLM returning outputs from a sequence in order.
--
-- Each LLM call consumes the next value from the list. Throws an error
-- if there are more LLM calls than values in the list.
--
-- @
-- let fixtures = [object ["n" .= 1], object ["n" .= 2]]
-- let result = run $ runMockLLMSequence fixtures $ do
--       r1 <- llmCall "sys" "first" schema
--       r2 <- llmCall "sys" "second" schema
--       pure (r1, r2)
-- @
runMockLLMSequence :: [Value] -> Sem (LLM ': effs) a -> Sem effs a
runMockLLMSequence outputs = evalState outputs . reinterpret handler
  where
    handler :: (Member (State [Value]) r) => LLM m x -> Sem r x
    handler (RunTurnOp _ _ _ _) = do
      outs <- get
      case outs of
        [] -> error "runMockLLMSequence: ran out of fixture outputs"
        (o : os) -> do
          put os
          pure $
            TurnResult
              { trOutput = o,
                trToolsInvoked = [],
                trNarrative = "",
                trThinking = ""
              }

-- ════════════════════════════════════════════════════════════════════════════
-- SCHEMA-MATCHED MOCK
-- ════════════════════════════════════════════════════════════════════════════

-- | Run LLM with schema-matched outputs.
--
-- Given a list of (predicate, output) pairs, returns the output for the first
-- matching schema. Falls back to the default output if no predicate matches.
--
-- Use 'schemaHasField' or 'schemaHasId' as predicates, or write custom ones.
--
-- @
-- let matchers =
--       [ (schemaHasId "Intent", object ["intent" .= "refund"])
--       , (schemaHasField "response", object ["response" .= "done"])
--       ]
-- let result = run $ runMockLLMMatched matchers defaultVal computation
-- @
runMockLLMMatched ::
  -- | (predicate, output) pairs
  [(Value -> Bool, Value)] ->
  -- | Default output if no match
  Value ->
  Sem (LLM ': effs) a ->
  Sem effs a
runMockLLMMatched matchers defaultOutput = interpret $ \case
  RunTurnOp _sys _content schema _tools ->
    pure $
      TurnResult
        { trOutput = findMatch schema matchers,
          trToolsInvoked = [],
          trNarrative = "",
          trThinking = ""
        }
  where
    findMatch :: Value -> [(Value -> Bool, Value)] -> Value
    findMatch _ [] = defaultOutput
    findMatch s ((predicate, out) : rest)
      | predicate s = out
      | otherwise = findMatch s rest

-- ════════════════════════════════════════════════════════════════════════════
-- CAPTURING MOCK
-- ════════════════════════════════════════════════════════════════════════════

-- | Run LLM, capturing all requests and returning them alongside the result.
--
-- This is useful for verifying that the correct prompts and schemas were
-- passed to the LLM during a computation.
--
-- @
-- let (requests, result) = run $ runMockLLMCapture (object []) $ runGraph handlers input
-- length requests \`shouldBe\` 2
-- case requests of
--   (req:_) -> req.lrSystemPrompt \`shouldContain\` "classify"
--   []      -> expectationFailure "expected at least one request"
-- @
runMockLLMCapture ::
  Value ->
  Sem (LLM ': effs) a ->
  Sem effs ([LLMRequest], a)
runMockLLMCapture fixedOutput = runWriter . reinterpret handler
  where
    handler :: (Member (Writer [LLMRequest]) r) => LLM m x -> Sem r x
    handler (RunTurnOp sysPmt content schema tools) = do
      let req = LLMRequest sysPmt content schema tools
      tell [req]
      pure $
        TurnResult
          { trOutput = fixedOutput,
            trToolsInvoked = [],
            trNarrative = "",
            trThinking = ""
          }

-- ════════════════════════════════════════════════════════════════════════════
-- SCHEMA MATCHING HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if a JSON schema has a specific property field.
--
-- @
-- schemaHasField "intent" mySchema  -- True if schema has "properties.intent"
-- @
schemaHasField :: Text -> Value -> Bool
schemaHasField field (Object obj) =
  case KM.lookup "properties" obj of
    Just (Object props) -> KM.member (fromText field) props
    _ -> False
schemaHasField _ _ = False

-- | Check if a JSON schema has a specific @$id@ or @title@.
--
-- @
-- schemaHasId "Intent" mySchema  -- True if schema.$id == "Intent" or schema.title == "Intent"
-- @
schemaHasId :: Text -> Value -> Bool
schemaHasId ident (Object obj) =
  KM.lookup "$id" obj == Just (String ident)
    || KM.lookup "title" obj == Just (String ident)
schemaHasId _ _ = False
