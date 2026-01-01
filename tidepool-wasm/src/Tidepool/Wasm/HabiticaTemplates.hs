{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Jinja template contexts and definitions for Habitica routing.
--
-- These templates are used by the HabiticaRoutingGraph to:
-- 1. Extract tasks from unstructured text (extract_task.jinja)
-- 2. Match tasks against existing Habitica todos (match_task.jinja)
-- 3. Format suggestion messages for Telegram (suggest_action.jinja)
--
-- Token Budget Strategy:
-- - Todo titles: Always shown (essential for matching)
-- - Checklist items: Truncated to maxChecklistItems per todo
-- - Notes: Truncated to 100 chars
-- - Summary stats shown so LLM knows total scope
module Tidepool.Wasm.HabiticaTemplates
  ( -- * Configuration
    TemplateConfig(..)
  , defaultTemplateConfig

    -- * Context Types
  , ExtractTaskContext(..)
  , MatchTaskContext(..)
  , SuggestActionContext(..)
  , TodoForMatching(..)
  , MatchStats(..)

    -- * Template Rendering
  , renderExtractTask
  , renderMatchTask
  , renderSuggestAction

    -- * Preprocessing
  , prepareMatchContext
  , prepareTodosForMatching
  , prepareTodoForMatching
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Wasm.HabiticaTypes
  ( ExtractedTask(..)
  , HabiticaTodo(..)
  )


-- ============================================================================
-- Configuration
-- ============================================================================

-- | Configuration for template token budget management.
data TemplateConfig = TemplateConfig
  { tcMaxChecklistItems :: Int
    -- ^ Maximum checklist items to show per todo (rest are truncated)
  , tcMaxTodos :: Int
    -- ^ Maximum todos to show (rest are summarized)
  , tcMaxNoteLength :: Int
    -- ^ Maximum note length before truncation
  }
  deriving stock (Show, Eq, Generic)

-- | Default configuration balanced for typical usage.
--
-- With these settings:
-- - 50 todos × ~50 chars title = ~2500 chars = ~625 tokens
-- - 50 todos × 5 items × ~30 chars = ~7500 chars = ~1875 tokens
-- - Total data: ~2500 tokens, leaving room for instructions
defaultTemplateConfig :: TemplateConfig
defaultTemplateConfig = TemplateConfig
  { tcMaxChecklistItems = 5
  , tcMaxTodos = 50
  , tcMaxNoteLength = 100
  }


-- ============================================================================
-- Context Types
-- ============================================================================

-- | Context for extract_task.jinja template.
data ExtractTaskContext = ExtractTaskContext
  { etcRawInput :: Text
    -- ^ The raw unstructured text from user
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ExtractTaskContext where
  toJSON ctx = object
    [ "rawInput" .= ctx.etcRawInput
    ]

-- | A todo prepared for the matching template.
--
-- Checklist may be truncated based on TemplateConfig.
data TodoForMatching = TodoForMatching
  { tfmId :: Text
    -- ^ Habitica task ID
  , tfmTitle :: Text
    -- ^ Todo title
  , tfmChecklist :: [Text]
    -- ^ Checklist items (may be truncated)
  , tfmChecklistCount :: Int
    -- ^ Full checklist count (before truncation)
  , tfmNotes :: Maybe Text
    -- ^ Optional notes (may be truncated)
  , tfmCompleted :: Bool
    -- ^ Whether the todo is completed
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TodoForMatching where
  toJSON t = object
    [ "id" .= t.tfmId
    , "title" .= t.tfmTitle
    , "checklist" .= t.tfmChecklist
    , "checklistCount" .= t.tfmChecklistCount
    , "notes" .= t.tfmNotes
    , "completed" .= t.tfmCompleted
    ]

-- | Summary statistics for the match context.
data MatchStats = MatchStats
  { msTotalTodos :: Int
    -- ^ Total number of todos (including hidden completed ones)
  , msActiveTodos :: Int
    -- ^ Number of active (non-completed) todos shown
  , msTotalChecklistItems :: Int
    -- ^ Total checklist items across all todos
  , msTruncatedTodos :: Int
    -- ^ Number of todos not shown due to limit
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON MatchStats where
  toJSON s = object
    [ "totalTodos" .= s.msTotalTodos
    , "activeTodos" .= s.msActiveTodos
    , "totalChecklistItems" .= s.msTotalChecklistItems
    , "truncatedTodos" .= s.msTruncatedTodos
    ]

-- | Context for match_task.jinja template.
data MatchTaskContext = MatchTaskContext
  { mtcTaskDescription :: Text
    -- ^ The extracted task description
  , mtcTaskContext :: Maybe Text
    -- ^ Optional context from extraction
  , mtcTodos :: [TodoForMatching]
    -- ^ Existing todos prepared for display
  , mtcStats :: MatchStats
    -- ^ Summary statistics
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON MatchTaskContext where
  toJSON ctx = object
    [ "taskDescription" .= ctx.mtcTaskDescription
    , "taskContext" .= ctx.mtcTaskContext
    , "todos" .= ctx.mtcTodos
    , "stats" .= ctx.mtcStats
    ]

-- | Context for suggest_action.jinja template.
data SuggestActionContext = SuggestActionContext
  { sacTaskDescription :: Text
    -- ^ The task being added
  , sacAction :: Text
    -- ^ "new_todo" or "checklist"
  , sacMatchingTodo :: Maybe TodoForMatching
    -- ^ The todo to add to (if checklist action)
  , sacMatchReason :: Text
    -- ^ LLM's reasoning for the match
  , sacRetryFeedback :: Maybe Text
    -- ^ User's feedback from previous attempt
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SuggestActionContext where
  toJSON ctx = object
    [ "taskDescription" .= ctx.sacTaskDescription
    , "action" .= ctx.sacAction
    , "matchingTodo" .= ctx.sacMatchingTodo
    , "matchReason" .= ctx.sacMatchReason
    , "retryFeedback" .= ctx.sacRetryFeedback
    ]


-- ============================================================================
-- Preprocessing
-- ============================================================================

-- | Convert HabiticaTodo to TodoForMatching with truncation.
prepareTodoForMatching :: TemplateConfig -> HabiticaTodo -> TodoForMatching
prepareTodoForMatching cfg todo = TodoForMatching
  { tfmId = todo.htId
  , tfmTitle = todo.htTitle
  , tfmChecklist = take cfg.tcMaxChecklistItems todo.htChecklist
  , tfmChecklistCount = length todo.htChecklist
  , tfmNotes = Nothing  -- Not available in current HabiticaTodo
  , tfmCompleted = False  -- Assuming we only get active todos
  }

-- | Prepare a list of todos for the match template.
--
-- Applies token budget limits:
-- - Filters out completed todos
-- - Limits total todos shown
-- - Truncates checklists per todo
prepareTodosForMatching :: TemplateConfig -> [HabiticaTodo] -> ([TodoForMatching], MatchStats)
prepareTodosForMatching cfg todos =
  let allCount = length todos
      prepared = map (prepareTodoForMatching cfg) todos
      limited = take cfg.tcMaxTodos prepared
      totalChecklist = sum [length t.htChecklist | t <- todos]
      stats = MatchStats
        { msTotalTodos = allCount
        , msActiveTodos = length limited
        , msTotalChecklistItems = totalChecklist
        , msTruncatedTodos = max 0 (allCount - cfg.tcMaxTodos)
        }
  in (limited, stats)

-- | Prepare the full match context from extracted task and todos.
prepareMatchContext
  :: TemplateConfig
  -> ExtractedTask
  -> [HabiticaTodo]
  -> MatchTaskContext
prepareMatchContext cfg task todos =
  let (preparedTodos, stats) = prepareTodosForMatching cfg todos
  in MatchTaskContext
    { mtcTaskDescription = task.etDescription
    , mtcTaskContext = task.etContext
    , mtcTodos = preparedTodos
    , mtcStats = stats
    }


-- ============================================================================
-- Template Rendering (Stub - would use ginger in real impl)
-- ============================================================================

-- | Render extract_task prompt.
--
-- In production, this would use ginger to render the Jinja template.
-- For WASM, we inline the prompt since we can't use TH.
renderExtractTask :: ExtractTaskContext -> Text
renderExtractTask ctx = T.unlines
  [ "Extract ONE actionable task from this message."
  , ""
  , "<input>"
  , ctx.etcRawInput
  , "</input>"
  , ""
  , "<rules>"
  , "- Extract the CORE action, not meta-commentary (\"I should...\" → just the task)"
  , "- If multiple tasks mentioned, pick the most concrete/actionable one"
  , "- Strip filler words: \"maybe\", \"I guess\", \"probably should\""
  , "- Keep context if it helps clarify (deadlines, people involved, location)"
  , "- If no clear task, use the full input as description"
  , "</rules>"
  , ""
  , "<output_format>"
  , "Return valid JSON only:"
  , "{"
  , "  \"description\": \"imperative task description\","
  , "  \"context\": \"optional context or null\""
  , "}"
  , "</output_format>"
  ]

-- | Render match_task prompt.
renderMatchTask :: MatchTaskContext -> Text
renderMatchTask ctx = T.unlines $
  [ "<task>"
  , "## New Task to Route"
  , ""
  , "**Description:** " <> ctx.mtcTaskDescription
  ]
  ++ maybe [] (\c -> ["**Context:** " <> c]) ctx.mtcTaskContext
  ++
  [ ""
  , "</task>"
  , ""
  , "<existing_todos>"
  , "## Your Existing Habitica Todos"
  , ""
  ]
  ++ if null ctx.mtcTodos
     then ["*No existing todos. This will be a new todo.*"]
     else
       [ "**Summary:** " <> T.pack (show ctx.mtcStats.msActiveTodos) <> " todos, "
         <> T.pack (show ctx.mtcStats.msTotalChecklistItems) <> " total checklist items"
       , ""
       ]
       ++ concatMap renderTodoForMatching (zip [1..] ctx.mtcTodos)
  ++
  [ "</existing_todos>"
  , ""
  , "<decision_rules>"
  , "## When to ADD AS CHECKLIST ITEM (match_id = todo's ID):"
  , ""
  , "1. **Clear parent-child relationship**: The new task is a specific step toward completing an existing todo"
  , "2. **Same project/goal**: Tasks share an obvious grouping"
  , "3. **Checklist pattern exists**: The todo already has checklist items in a similar vein"
  , ""
  , "## When to CREATE NEW TODO (match_id = null):"
  , ""
  , "1. **Independent task**: Not clearly a subtask of anything"
  , "2. **Same topic but different goal**: Related but not a subtask"
  , "3. **Ambiguous fit**: Could fit multiple todos equally well → prefer new todo"
  , ""
  , "**When in doubt, prefer NEW TODO.**"
  , "</decision_rules>"
  , ""
  , "<output_format>"
  , "Return valid JSON only:"
  , "{"
  , "  \"match_id\": \"the todo ID to add checklist item to, or null for new todo\","
  , "  \"reason\": \"1-2 sentence explanation of your decision\""
  , "}"
  , "</output_format>"
  ]

-- | Render a single todo for the match template.
renderTodoForMatching :: (Int, TodoForMatching) -> [Text]
renderTodoForMatching (idx, todo) =
  [ "### " <> T.pack (show idx) <> ". " <> todo.tfmTitle ]
  ++ maybe [] (\n -> ["<small>" <> n <> "</small>"]) todo.tfmNotes
  ++ [""]
  ++ checklistSection
  ++ ["`ID: " <> todo.tfmId <> "`", ""]
  where
    checklistSection
      | null todo.tfmChecklist = ["*No checklist items yet*"]
      | otherwise =
          let shown = length todo.tfmChecklist
              total = todo.tfmChecklistCount
              header = if shown < total
                       then "**Checklist (showing " <> T.pack (show shown) <> "/" <> T.pack (show total) <> "):**"
                       else "**Checklist:**"
              truncationNote = if shown < total
                               then ["  - *[+" <> T.pack (show (total - shown)) <> " more items...]*"]
                               else []
          in [header] ++ ["  - " <> item | item <- todo.tfmChecklist] ++ truncationNote

-- | Render suggest_action message for Telegram.
renderSuggestAction :: SuggestActionContext -> Text
renderSuggestAction ctx = T.unlines $
  maybe [] (\fb -> ["*Revised suggestion based on your feedback:*", "\"" <> fb <> "\"", ""]) ctx.sacRetryFeedback
  ++
  case ctx.sacAction of
    "new_todo" ->
      [ "**Create new todo:**"
      , ctx.sacTaskDescription
      ]
    _ -> -- checklist
      [ "**Add to \"" <> maybe "?" (.tfmTitle) ctx.sacMatchingTodo <> "\":**"
      , ctx.sacTaskDescription
      , ""
      ]
      ++ case ctx.sacMatchingTodo of
           Just todo | not (null todo.tfmChecklist) ->
             [ "_Current checklist:_" ]
             ++ ["• " <> item | item <- take 3 todo.tfmChecklist]
             ++ if todo.tfmChecklistCount > 3
                then ["_...and " <> T.pack (show (todo.tfmChecklistCount - 3)) <> " more_"]
                else []
           _ -> []
  ++
  [ ""
  , "_" <> ctx.sacMatchReason <> "_"
  ]
