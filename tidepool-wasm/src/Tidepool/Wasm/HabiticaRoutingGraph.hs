{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Habitica Routing Graph - Routes extracted tasks to Habitica.
--
-- This graph implements a task extraction and routing workflow:
-- 1. Takes unstructured text input from Telegram
-- 2. Extracts a single task item via LLM
-- 3. Fetches existing Habitica todos
-- 4. Uses LLM to determine if any existing task is a good match
-- 5. Suggests either adding a new todo OR adding a checklist item
-- 6. User confirms/denies via Telegram buttons
-- 7. On approval: executes the Habitica action
-- 8. On denial: either retries with feedback or skips
--
-- Structure:
--   Entry(RawInput)
--     → extractTask (LLM)
--     → fetchExisting (Habitica)
--     → matchTask (LLM)
--     → suggestAction
--     → confirmWithUser (Telegram buttons)
--       ├─→ executeAction (Habitica) → Exit
--       ├─→ suggestAction (retry with feedback)
--       └─→ Exit (skip)
module Tidepool.Wasm.HabiticaRoutingGraph
  ( -- * Domain Types
    RawInput(..)
  , ExtractedTask(..)
  , ExistingTodos(..)
  , HabiticaTodo(..)
  , MatchResult(..)
  , Suggestion(..)
  , SuggestionAction(..)
  , UserConfirmation(..)
  , ExecutionResult(..)

    -- * Graph Type
  , HabiticaRoutingGraph(..)

    -- * WASM Handlers
  , extractTaskHandler
  , fetchExistingHandler
  , matchTaskHandler
  , suggestActionHandler
  , confirmWithUserHandler
  , executeActionHandler

    -- * Entry Point
  , runHabiticaRoutingGraph
  ) where

import Data.Aeson (Value(..), Result(..), object, (.=), fromJSON)
import Data.Maybe (maybeToList)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Needs, UsesEffects, Exit)
import Tidepool.Graph.Generic (GraphMode(..), type (:-))
import qualified Tidepool.Graph.Generic as G (Entry, Exit, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice(..), OneOf(..), To, gotoChoice, gotoExit)

import Tidepool.Wasm.Effect (WasmM, logInfo, logError, llmComplete, telegramAsk)
import Tidepool.Wasm.Habitica
  ( HabiticaOp(..)
  , habitica
  , FetchedTodo(..)
  , FetchedChecklistItem(..)
  , TodoId(..)
  )
import Tidepool.Wasm.HabiticaTemplates
  ( ExtractTaskContext(..)
  , SuggestActionContext(..)
  , defaultTemplateConfig
  , prepareMatchContext
  , prepareTodoForMatching
  , renderExtractTask
  , renderMatchTask
  , renderSuggestAction
  )
import Tidepool.Wasm.HabiticaTypes
  ( RawInput(..)
  , ExtractedTask(..)
  , ExistingTodos(..)
  , HabiticaTodo(..)
  , MatchResult(..)
  , Suggestion(..)
  , SuggestionAction(..)
  , UserConfirmation(..)
  , ExecutionResult(..)
  )


-- ============================================================================
-- Graph Definition
-- ============================================================================

-- | Habitica routing graph for task extraction and routing.
--
-- Flow:
-- 1. Extract task from raw input
-- 2. Fetch existing Habitica todos
-- 3. Match task against existing todos (LLM)
-- 4. Generate suggestion
-- 5. Get user confirmation
-- 6. Execute or retry/skip
data HabiticaRoutingGraph mode = HabiticaRoutingGraph
  { entry          :: mode :- G.Entry RawInput

  , extractTask    :: mode :- G.LogicNode
                          :@ Needs '[RawInput]
                          :@ UsesEffects '[Goto "fetchExisting" ExtractedTask]

  , fetchExisting  :: mode :- G.LogicNode
                          :@ Needs '[ExtractedTask]
                          :@ UsesEffects '[Goto "matchTask" ExistingTodos]

  , matchTask      :: mode :- G.LogicNode
                          :@ Needs '[ExistingTodos]
                          :@ UsesEffects '[Goto "suggestAction" MatchResult]

  , suggestAction  :: mode :- G.LogicNode
                          :@ Needs '[MatchResult]
                          :@ UsesEffects '[Goto "confirmWithUser" Suggestion]

  , confirmWithUser :: mode :- G.LogicNode
                           :@ Needs '[Suggestion]
                           :@ UsesEffects '[ Goto "executeAction" Suggestion
                                           , Goto "suggestAction" MatchResult  -- retry
                                           , Goto Exit ExecutionResult         -- skip
                                           ]

  , executeAction  :: mode :- G.LogicNode
                          :@ Needs '[Suggestion]
                          :@ UsesEffects '[Goto Exit ExecutionResult]

  , exit           :: mode :- G.Exit ExecutionResult
  }
  deriving Generic


-- ============================================================================
-- WASM Handlers
-- ============================================================================

-- | Extract a task from raw unstructured text using LLM.
extractTaskHandler :: RawInput -> WasmM (GotoChoice '[To "fetchExisting" ExtractedTask])
extractTaskHandler input = do
  logInfo $ "Extracting task from: " <> T.take 100 input.unRawInput

  let ctx = ExtractTaskContext { etcRawInput = input.unRawInput }
      prompt = renderExtractTask ctx

  result <- llmComplete
    "extract_task"
    "You are a task extraction assistant."
    prompt
    (Just extractTaskSchema)

  case parseExtractedTask result of
    Right task -> do
      logInfo $ "Extracted task: " <> task.etDescription
      pure $ gotoChoice @"fetchExisting" task
    Left err -> do
      -- Fallback: use the raw input as the task
      logError $ "Failed to parse extracted task: " <> err
      let task = ExtractedTask
            { etDescription = input.unRawInput
            , etContext = Nothing
            }
      pure $ gotoChoice @"fetchExisting" task
  where
    extractTaskSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "description" .= object ["type" .= ("string" :: Text)]
          , "context" .= object ["type" .= ("string" :: Text)]
          ]
      , "required" .= (["description"] :: [Text])
      ]

    parseExtractedTask :: Value -> Either Text ExtractedTask
    parseExtractedTask v = case fromJSON v of
      Success et -> Right et
      Error msg  -> Left (T.pack msg)

-- | Fetch existing todos from Habitica.
fetchExistingHandler :: ExtractedTask -> WasmM (GotoChoice '[To "matchTask" ExistingTodos])
fetchExistingHandler task = do
  logInfo "Fetching existing Habitica todos..."

  fetchedTodos <- habitica FetchTodos

  let todos = map toHabiticaTodo fetchedTodos
  logInfo $ "Fetched " <> T.pack (show (length todos)) <> " existing todos"

  pure $ gotoChoice @"matchTask" ExistingTodos
    { etTask = task
    , etTodos = todos
    }
  where
    -- Convert typed API response to graph domain type
    toHabiticaTodo :: FetchedTodo -> HabiticaTodo
    toHabiticaTodo ft = HabiticaTodo
      { htId = ft.ftTodoId.unTodoId
      , htTitle = ft.ftTitle
      , htChecklist = map (.fciText) ft.ftChecklist
      }


-- | Match the extracted task against existing todos using LLM.
matchTaskHandler :: ExistingTodos -> WasmM (GotoChoice '[To "suggestAction" MatchResult])
matchTaskHandler existing = do
  logInfo "Matching task against existing todos..."

  -- Use template with smart truncation for token budget
  let matchCtx = prepareMatchContext defaultTemplateConfig existing.etTask existing.etTodos
      prompt = renderMatchTask matchCtx

  result <- llmComplete
    "match_task"
    "You are a task organization assistant. Match new tasks to existing todos when they are clearly related."
    prompt
    (Just matchSchema)

  let matchResult = parseMatchResult result existing
  logInfo $ "Match decision: " <> matchResult.mrReason

  pure $ gotoChoice @"suggestAction" matchResult
  where
    matchSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "match_id" .= object ["type" .= ("string" :: Text)]
          , "reason" .= object ["type" .= ("string" :: Text)]
          ]
      , "required" .= (["reason"] :: [Text])
      ]

    parseMatchResult :: Value -> ExistingTodos -> MatchResult
    parseMatchResult v ex = MatchResult
      { mrTask = ex.etTask
      , mrTodos = ex.etTodos
      , mrMatch = findMatch (extractMatchId v) ex.etTodos
      , mrReason = extractReason v
      }

    extractMatchId :: Value -> Maybe Text
    extractMatchId (Object obj) = case KM.lookup (Key.fromText "match_id") obj of
      Just (String t) | not (T.null t) -> Just t
      _ -> Nothing
    extractMatchId _ = Nothing

    extractReason :: Value -> Text
    extractReason (Object obj) = case KM.lookup (Key.fromText "reason") obj of
      Just (String t) -> t
      _ -> "No reason provided"
    extractReason _ = "Could not parse reason"

    findMatch :: Maybe Text -> [HabiticaTodo] -> Maybe HabiticaTodo
    findMatch Nothing _ = Nothing
    findMatch (Just matchId) todos =
      case filter (\t -> t.htId == matchId) todos of
        (t:_) -> Just t
        []    -> Nothing


-- | Generate a suggestion message for the user.
suggestActionHandler :: MatchResult -> WasmM (GotoChoice '[To "confirmWithUser" Suggestion])
suggestActionHandler matchResult = do
  let action = case matchResult.mrMatch of
        Just todo -> SuggestChecklist todo.htId
        Nothing   -> SuggestNewTodo

      -- Convert HabiticaTodo to TodoForMatching for template
      matchingTodoForTemplate = prepareTodoForMatching defaultTemplateConfig <$> matchResult.mrMatch

      -- Use template to render the suggestion message
      suggestCtx = SuggestActionContext
        { sacTaskDescription = matchResult.mrTask.etDescription
        , sacAction = case action of
            SuggestNewTodo -> "new_todo"
            SuggestChecklist _ -> "checklist"
        , sacMatchingTodo = matchingTodoForTemplate
        , sacMatchReason = matchResult.mrReason
        , sacRetryFeedback = Nothing  -- Will be set on retry
        }
      message = renderSuggestAction suggestCtx

  logInfo $ "Suggestion: " <> T.take 100 message

  pure $ gotoChoice @"confirmWithUser" Suggestion
    { sgTask = matchResult.mrTask
    , sgAction = action
    , sgMessage = message
    , sgMatchingTodo = matchResult.mrMatch
    , sgFeedback = Nothing
    }


-- | Confirm with the user via Telegram buttons.
-- This handler yields a TelegramConfirm effect and processes the response.
confirmWithUserHandler
  :: Suggestion
  -> WasmM (GotoChoice '[ To "executeAction" Suggestion
                        , To "suggestAction" MatchResult
                        , To Exit ExecutionResult
                        ])
confirmWithUserHandler suggestion = do
  logInfo $ "Awaiting user confirmation: " <> suggestion.sgMessage

  -- Yield TelegramAsk effect - TypeScript will present buttons and return callback
  result <- telegramAsk suggestion.sgMessage
    [ ("✓ Yes", "approved")
    , ("✗ No", "denied")
    , ("Skip", "skipped")
    ]

  case parseConfirmation result of
    Approved -> do
      logInfo "User approved suggestion"
      pure $ gotoChoice @"executeAction" suggestion

    Denied feedback -> do
      logInfo $ "User denied with feedback: " <> feedback
      -- Retry: go back to suggestAction with feedback
      -- We need to reconstruct a MatchResult with the feedback
      let retryResult = MatchResult
            { mrTask = suggestion.sgTask
            , mrTodos = maybeToList suggestion.sgMatchingTodo
            , mrMatch = suggestion.sgMatchingTodo
            , mrReason = "Retry after user feedback: " <> feedback
            }
      pure $ gotoChoice @"suggestAction" retryResult

    Skipped -> do
      logInfo "User skipped this task"
      pure $ gotoExit ExecutionResult
        { erSuccess = True
        , erMessage = "Task skipped by user"
        }
  where
    -- Parse the callback string directly
    parseConfirmation :: Text -> UserConfirmation
    parseConfirmation "approved" = Approved
    parseConfirmation "skipped"  = Skipped
    parseConfirmation "denied"   = Denied ""  -- No feedback in simple button flow
    parseConfirmation _          = Skipped    -- Default to skip on unknown


-- | Execute the Habitica action (create todo or add checklist item).
executeActionHandler :: Suggestion -> WasmM (GotoChoice '[To Exit ExecutionResult])
executeActionHandler suggestion = do
  logInfo "Executing Habitica action..."

  result <- case suggestion.sgAction of
    SuggestNewTodo -> do
      logInfo $ "Creating new todo: " <> suggestion.sgTask.etDescription
      _ <- habitica (CreateTodo suggestion.sgTask.etDescription)
      pure $ ExecutionResult True $ "Created todo: " <> suggestion.sgTask.etDescription

    SuggestChecklist todoIdText -> do
      logInfo $ "Adding checklist item to todo " <> todoIdText
      _ <- habitica (AddChecklistItem (TodoId todoIdText) suggestion.sgTask.etDescription)
      pure $ ExecutionResult True $ "Added checklist item: " <> suggestion.sgTask.etDescription

  logInfo $ "Execution result: " <> result.erMessage
  pure $ gotoExit result


-- ============================================================================
-- Graph Execution Entry Point
-- ============================================================================

-- | Run the Habitica routing graph from entry to exit.
runHabiticaRoutingGraph :: RawInput -> WasmM ExecutionResult
runHabiticaRoutingGraph input = do
  -- Step 1: Extract task
  extractResult <- extractTaskHandler input

  -- Step 2: Fetch existing todos
  let GotoChoice (Here task) = extractResult
  fetchResult <- fetchExistingHandler task

  -- Step 3: Match task
  let GotoChoice (Here existing) = fetchResult
  matchResult <- matchTaskHandler existing

  -- Step 4: Suggest action
  let GotoChoice (Here match) = matchResult
  suggestResult <- suggestActionHandler match

  -- Step 5: Confirm with user (may loop back)
  let GotoChoice (Here suggestion) = suggestResult
  dispatchConfirm suggestion

  where
    dispatchConfirm :: Suggestion -> WasmM ExecutionResult
    dispatchConfirm suggestion = do
      confirmResult <- confirmWithUserHandler suggestion
      case confirmResult of
        GotoChoice (Here sugg) -> do
          -- Approved: execute
          executeResult <- executeActionHandler sugg
          let GotoChoice (Here result) = executeResult
          pure result

        GotoChoice (There (Here matchResult)) -> do
          -- Retry: go back to suggest
          suggestResult <- suggestActionHandler matchResult
          let GotoChoice (Here newSuggestion) = suggestResult
          dispatchConfirm newSuggestion

        GotoChoice (There (There (Here result))) -> do
          -- Skip: return result directly
          pure result
