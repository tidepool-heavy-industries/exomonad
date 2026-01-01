{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Habitica Routing Graph.
--
-- Tests the full graph execution using the initializeWasm pattern from E2ESpec.
-- Mocks effect responses (EffLlmComplete, EffHabitica, EffTelegramConfirm) to
-- exercise the happy path and various branches.
module HabiticaRoutingGraphSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..), object, (.=))
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Vector as V

import Tidepool.Wasm.Runner (initializeWasm, WasmResult(..))
import Tidepool.Wasm.WireTypes
  ( SerializableEffect(..)
  , EffectResult(..)
  )
import Tidepool.Wasm.HabiticaRoutingGraph
  ( runHabiticaRoutingGraph
  , RawInput(..)
  , ExecutionResult(..)
  )


spec :: Spec
spec = do
  describe "HabiticaRoutingGraph" $ do
    happyPathSpec
    userDenialSpec
    userSkipSpec
    effectSequenceSpec


-- ════════════════════════════════════════════════════════════════════════════
-- Happy Path: User approves suggestion → execute action
-- ════════════════════════════════════════════════════════════════════════════

happyPathSpec :: Spec
happyPathSpec = describe "Happy path (user approves)" $ do

  it "completes full graph with new todo creation" $ do
    let input = RawInput "Buy milk from the store"
        result = initializeWasm (runHabiticaRoutingGraph input)

    -- Run through all effects with mock responses
    case runToCompletion result mockHappyPathResponses of
      Just execResult -> do
        execResult.erSuccess `shouldBe` True
        -- Message should mention the task was created
        T.unpack execResult.erMessage `shouldSatisfy` isInfixOf "Buy milk"
      Nothing ->
        expectationFailure "Expected graph to complete successfully"

  it "creates checklist item when matching todo found" $ do
    let input = RawInput "Add eggs to grocery list"
        result = initializeWasm (runHabiticaRoutingGraph input)

    -- Mock responses that include a matching todo
    case runToCompletion result mockChecklistResponses of
      Just execResult -> do
        execResult.erSuccess `shouldBe` True
        T.unpack execResult.erMessage `shouldSatisfy` isInfixOf "checklist"
      Nothing ->
        expectationFailure "Expected graph to complete successfully"


-- ════════════════════════════════════════════════════════════════════════════
-- User Denial: Retry with feedback
-- ════════════════════════════════════════════════════════════════════════════

userDenialSpec :: Spec
userDenialSpec = describe "User denial (retry flow)" $ do

  it "retries suggestion when user denies" $ do
    let input = RawInput "Schedule meeting"
        result = initializeWasm (runHabiticaRoutingGraph input)

    -- Use stateful runner: first TelegramConfirm returns "denied", second returns "approved"
    case runToCompletionStateful result mockDenialThenApproveStateful of
      Just (execResult, confirmCount) -> do
        execResult.erSuccess `shouldBe` True
        -- Should have seen 2 TelegramConfirm effects (denied, then approved)
        confirmCount `shouldBe` 2
      Nothing ->
        expectationFailure "Expected graph to complete after retry"


-- ════════════════════════════════════════════════════════════════════════════
-- User Skip: Exit immediately
-- ════════════════════════════════════════════════════════════════════════════

userSkipSpec :: Spec
userSkipSpec = describe "User skip" $ do

  it "exits cleanly when user skips" $ do
    let input = RawInput "Do something"
        result = initializeWasm (runHabiticaRoutingGraph input)

    case runToCompletion result mockSkipResponses of
      Just execResult -> do
        execResult.erSuccess `shouldBe` True
        T.unpack execResult.erMessage `shouldSatisfy` isInfixOf "skipped"
      Nothing ->
        expectationFailure "Expected graph to complete with skip"


-- ════════════════════════════════════════════════════════════════════════════
-- Effect Sequence Verification
-- ════════════════════════════════════════════════════════════════════════════

effectSequenceSpec :: Spec
effectSequenceSpec = describe "Effect sequence" $ do

  it "yields effects in expected order" $ do
    let input = RawInput "Test task"
        result = initializeWasm (runHabiticaRoutingGraph input)

        -- Collect first N effects
        effects = collectEffects 20 result mockHappyPathResponses

    -- Verify we get the expected effect types in order:
    -- 1. LogInfo (extracting task)
    -- 2. LlmComplete (extract_task)
    -- 3. LogInfo (extracted task)
    -- 4. LogInfo (fetching todos)
    -- 5. Habitica (fetchTodos)
    -- 6. LogInfo (fetched N todos)
    -- 7. LogInfo (matching task)
    -- 8. LlmComplete (match_task)
    -- 9. LogInfo (match decision)
    -- 10. LogInfo (suggestion)
    -- 11. LogInfo (awaiting confirmation)
    -- 12. TelegramConfirm
    -- 13. LogInfo (user approved)
    -- 14. LogInfo (executing action)
    -- 15. LogInfo (creating new todo)
    -- 16. Habitica (createTodo)
    -- 17. LogInfo (execution result)

    length effects `shouldSatisfy` (> 10)

    -- Check first effect is LogInfo (using pattern match instead of partial `head`)
    case effects of
      (EffLogInfo msg : _) -> T.unpack msg `shouldSatisfy` isInfixOf "Extracting"
      [] -> expectationFailure "Expected at least one effect"
      _ -> expectationFailure "Expected first effect to be LogInfo"

    -- Check we have LlmComplete effects
    let llmEffects = filter isLlmComplete effects
    length llmEffects `shouldSatisfy` (>= 2)

    -- Check we have Habitica effects
    let habEffects = filter isHabitica effects
    length habEffects `shouldSatisfy` (>= 2)

    -- Check we have TelegramConfirm effect
    let confirmEffects = filter isTelegramConfirm effects
    length confirmEffects `shouldBe` 1

  it "LlmComplete effects have correct node names" $ do
    let input = RawInput "Check task nodes"
        result = initializeWasm (runHabiticaRoutingGraph input)
        effects = collectEffects 20 result mockHappyPathResponses
        llmEffects = [e | e@(EffLlmComplete {}) <- effects]
        nodeNames = [e.effNode | e <- llmEffects]

    -- Should have extract_task and match_task
    nodeNames `shouldSatisfy` elem "extract_task"
    nodeNames `shouldSatisfy` elem "match_task"


-- ════════════════════════════════════════════════════════════════════════════
-- Mock Responses
-- ════════════════════════════════════════════════════════════════════════════

-- | Happy path: no matching todos, user approves new todo
mockHappyPathResponses :: SerializableEffect -> EffectResult
mockHappyPathResponses eff = case eff of
  EffLogInfo _ -> ResSuccess Nothing
  EffLogError _ -> ResSuccess Nothing

  EffLlmComplete node _ _ _ -> case node of
    "extract_task" -> ResSuccess $ Just $ object
      [ "description" .= ("Buy milk from the store" :: String)
      , "context" .= Null
      ]
    "match_task" -> ResSuccess $ Just $ object
      [ "match_id" .= Null  -- No match
      , "reason" .= ("No existing todo matches this task" :: String)
      ]
    _ -> ResSuccess $ Just $ object []

  EffHabitica op _ -> case op of
    "fetchTodos" -> ResSuccess $ Just $ Array V.empty  -- Empty list
    "createTodo" -> ResSuccess $ Just $ object
      [ "id" .= ("new-todo-id" :: String)
      , "text" .= ("Buy milk from the store" :: String)
      ]
    _ -> ResSuccess $ Just $ object []

  EffTelegramConfirm _ _ -> ResSuccess $ Just $ object
    [ "response" .= ("approved" :: String)
    ]

-- | Checklist path: matching todo found, user approves adding checklist item
mockChecklistResponses :: SerializableEffect -> EffectResult
mockChecklistResponses eff = case eff of
  EffLogInfo _ -> ResSuccess Nothing
  EffLogError _ -> ResSuccess Nothing

  EffLlmComplete node _ _ _ -> case node of
    "extract_task" -> ResSuccess $ Just $ object
      [ "description" .= ("Add eggs to grocery list" :: String)
      , "context" .= Null
      ]
    "match_task" -> ResSuccess $ Just $ object
      [ "match_id" .= ("grocery-todo-id" :: String)
      , "reason" .= ("This task belongs to the Groceries todo" :: String)
      ]
    _ -> ResSuccess $ Just $ object []

  EffHabitica op _ -> case op of
    "fetchTodos" -> ResSuccess $ Just $ Array $ V.fromList
      [ object
          [ "id" .= ("grocery-todo-id" :: String)
          , "text" .= ("Groceries" :: String)
          , "checklist" .= (["milk", "bread"] :: [String])
          ]
      ]
    "addChecklistItem" -> ResSuccess $ Just $ object
      [ "id" .= ("checklist-item-id" :: String)
      ]
    _ -> ResSuccess $ Just $ object []

  EffTelegramConfirm _ _ -> ResSuccess $ Just $ object
    [ "response" .= ("approved" :: String)
    ]

-- | Skip: user skips immediately
mockSkipResponses :: SerializableEffect -> EffectResult
mockSkipResponses eff = case eff of
  EffLogInfo _ -> ResSuccess Nothing
  EffLogError _ -> ResSuccess Nothing

  EffLlmComplete node _ _ _ -> case node of
    "extract_task" -> ResSuccess $ Just $ object
      [ "description" .= ("Test task" :: String)
      , "context" .= Null
      ]
    "match_task" -> ResSuccess $ Just $ object
      [ "match_id" .= Null
      , "reason" .= ("No match found" :: String)
      ]
    _ -> ResSuccess $ Just $ object []

  EffHabitica op _ -> case op of
    "fetchTodos" -> ResSuccess $ Just $ Array V.empty
    "createTodo" -> ResSuccess $ Just $ object
      [ "id" .= ("new-todo-id" :: String)
      ]
    _ -> ResSuccess $ Just $ object []

  EffTelegramConfirm _ _ -> ResSuccess $ Just $ object
    [ "response" .= ("skipped" :: String)
    ]

-- | Stateful mock for denial-then-approve: returns different responses based on call count
-- State is the number of TelegramConfirm effects seen so far
mockDenialThenApproveStateful :: Int -> SerializableEffect -> (EffectResult, Int)
mockDenialThenApproveStateful confirmCount eff = case eff of
  EffLogInfo _ -> (ResSuccess Nothing, confirmCount)
  EffLogError _ -> (ResSuccess Nothing, confirmCount)

  EffLlmComplete node _ _ _ -> case node of
    "extract_task" -> (ResSuccess $ Just $ object
      [ "description" .= ("Schedule meeting" :: String)
      , "context" .= Null
      ], confirmCount)
    "match_task" -> (ResSuccess $ Just $ object
      [ "match_id" .= Null
      , "reason" .= ("No match found" :: String)
      ], confirmCount)
    _ -> (ResSuccess $ Just $ object [], confirmCount)

  EffHabitica op _ -> case op of
    "fetchTodos" -> (ResSuccess $ Just $ Array V.empty, confirmCount)
    "createTodo" -> (ResSuccess $ Just $ object
      [ "id" .= ("new-todo-id" :: String)
      ], confirmCount)
    _ -> (ResSuccess $ Just $ object [], confirmCount)

  EffTelegramConfirm _ _ ->
    let newCount = confirmCount + 1
        response = if confirmCount == 0
          then object  -- First call: deny with feedback
            [ "response" .= ("denied" :: String)
            , "feedback" .= ("Try a different approach" :: String)
            ]
          else object  -- Second call: approve
            [ "response" .= ("approved" :: String)
            ]
    in (ResSuccess $ Just response, newCount)


-- ════════════════════════════════════════════════════════════════════════════
-- Test Helpers
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a WasmResult to completion using the given mock response function.
-- Returns the final ExecutionResult or Nothing if max steps exceeded.
runToCompletion :: WasmResult ExecutionResult
                -> (SerializableEffect -> EffectResult)
                -> Maybe ExecutionResult
runToCompletion result mockResponses = go 100 result
  where
    go :: Int -> WasmResult ExecutionResult -> Maybe ExecutionResult
    go 0 _ = Nothing  -- Max steps exceeded
    go n (WasmYield eff k) = go (n-1) (k (mockResponses eff))
    go _ (WasmComplete execResult) = Just execResult
    go _ (WasmError _) = Nothing

-- | Run a WasmResult to completion using a stateful mock.
-- The mock function takes current state and effect, returns (result, new state).
-- Returns the final ExecutionResult and final state, or Nothing if max steps exceeded.
runToCompletionStateful :: WasmResult ExecutionResult
                        -> (Int -> SerializableEffect -> (EffectResult, Int))
                        -> Maybe (ExecutionResult, Int)
runToCompletionStateful result mockResponses = go 100 0 result
  where
    go :: Int -> Int -> WasmResult ExecutionResult -> Maybe (ExecutionResult, Int)
    go 0 _ _ = Nothing  -- Max steps exceeded
    go n state (WasmYield eff k) =
      let (response, newState) = mockResponses state eff
      in go (n-1) newState (k response)
    go _ state (WasmComplete execResult) = Just (execResult, state)
    go _ _ (WasmError _) = Nothing

-- | Collect effects yielded by the graph up to a maximum count.
collectEffects :: Int
               -> WasmResult a
               -> (SerializableEffect -> EffectResult)
               -> [SerializableEffect]
collectEffects maxSteps result mockResponses = go maxSteps result
  where
    go :: Int -> WasmResult a -> [SerializableEffect]
    go 0 _ = []
    go n (WasmYield eff k) = eff : go (n-1) (k (mockResponses eff))
    go _ _ = []

-- | Effect type predicates
isLlmComplete :: SerializableEffect -> Bool
isLlmComplete (EffLlmComplete {}) = True
isLlmComplete _ = False

isHabitica :: SerializableEffect -> Bool
isHabitica (EffHabitica {}) = True
isHabitica _ = False

isTelegramConfirm :: SerializableEffect -> Bool
isTelegramConfirm (EffTelegramConfirm {}) = True
isTelegramConfirm _ = False
