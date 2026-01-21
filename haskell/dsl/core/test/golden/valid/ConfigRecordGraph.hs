{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Golden test: Config record pattern with field-witness routing.
--
-- This example validates the complete phantom-tagged records design:
-- - Standalone, testable config records
-- - LLM subtype annotation ('API)
-- - Multiple entries, tools, and exits
-- - Routes annotation for exit routing
-- - NodeRef phantom wrappers for compile-time node names
-- - Field-witness routing (no string literals!)
--
-- Goal: Confirm the design compiles and provides excellent UX for library users.
module ConfigRecordGraph where

import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

import Tidepool.Prelude
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Generic.Core (NodeRef(..), type (:-))
import Tidepool.Graph.Types
  ( type (:@), Entries, Tools, Exits, Routes, Route(..)
  , EntryPoint, Tool, ExitTool, ToolMetadata(..)
  , LLMKind(..)
  )
import Tidepool.Graph.Goto (GotoChoice, gotoNode, (-->), gotoExit, LLMHandler(..))
import Tidepool.Effect.Types (State)

-- ════════════════════════════════════════════════════════════════════════════
-- DOMAIN TYPES
-- ════════════════════════════════════════════════════════════════════════════

data TaskSpec = TaskSpec
  { taskDescription :: Text
  , taskPriority :: Priority
  } deriving (Generic, ToJSON, FromJSON)

data Priority = Low | Medium | High
  deriving (Generic, ToJSON, FromJSON)

data RetryInfo = RetryInfo
  { originalTask :: TaskSpec
  , attemptCount :: Int
  , lastError :: Text
  } deriving (Generic, ToJSON, FromJSON)

data WorkResult = WorkResult
  { resultSummary :: Text
  , effortLevel :: Int
  } deriving (Generic, ToJSON, FromJSON)

data SearchQuery = SearchQuery
  { query :: Text
  , maxResults :: Int
  } deriving (Generic, ToJSON, FromJSON)

data SearchResult = SearchResult
  { results :: [Text]
  } deriving (Generic, ToJSON, FromJSON)

instance ToolMetadata SearchQuery where
  toolName = "search_database"
  toolDescription = "Search internal knowledge base for relevant information"

data CalculatorInput = CalculatorInput
  { expression :: Text
  } deriving (Generic, ToJSON, FromJSON)

data CalculatorResult = CalculatorResult
  { result :: Double
  } deriving (Generic, ToJSON, FromJSON)

instance ToolMetadata CalculatorInput where
  toolName = "calculate"
  toolDescription = "Evaluate mathematical expressions"

-- Exit tool payloads
data CompleteTaskPayload = CompleteTaskPayload
  { summary :: Text
  } deriving (Generic, ToJSON, FromJSON)

instance ToolMetadata CompleteTaskPayload where
  toolName = "complete_task"
  toolDescription = "Mark task as successfully completed"

data RequestRetryPayload = RequestRetryPayload
  { reason :: Text
  , suggestedApproach :: Text
  } deriving (Generic, ToJSON, FromJSON)

instance ToolMetadata RequestRetryPayload where
  toolName = "request_retry"
  toolDescription = "Request retry with different approach"

-- ════════════════════════════════════════════════════════════════════════════
-- CONFIG RECORDS
-- ════════════════════════════════════════════════════════════════════════════

-- | Work node entries - multiple entry points.
data WorkEntries mode = WorkEntries
  { fresh :: mode :- EntryPoint TaskSpec
  , retry :: mode :- EntryPoint RetryInfo
  } deriving Generic

-- | Work node tools - available during LLM call.
data WorkTools mode = WorkTools
  { search :: mode :- Tool SearchQuery SearchResult
  , calc   :: mode :- Tool CalculatorInput CalculatorResult
  } deriving Generic

-- | Work node exits - LLM chooses via tool calls.
data WorkExits mode = WorkExits
  { complete :: mode :- ExitTool CompleteTaskPayload '[WorkResult]
      :@ Routes '[ExitGraph]
  , needRetry :: mode :- ExitTool RequestRetryPayload '[RetryInfo]
      :@ Routes '[ToEntry "gWork" "retry"]
  } deriving Generic

-- | Work node config - groups all components.
--
-- This is the testable, reusable unit. Can be instantiated in AsGraph or AsHandler mode.
data WorkConfig mode = WorkConfig
  { entries :: WorkEntries mode
  , tools   :: WorkTools mode
  , exits   :: WorkExits mode
  } deriving Generic

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Simple graph with entry, work node (config-based), and exit.
--
-- Uses:
-- - NodeRef phantom wrappers to carry node names at compile time
-- - LLMNode 'API to specify direct API execution (vs 'CodingAgent or 'Local)
-- - Config record annotation to link WorkConfig
--
-- This enables field-witness routing: gWork graph `gotoNode` (retry . entries)
data WorkGraph mode = WorkGraph
  { gEntry :: NodeRef "gEntry" (mode :- G.EntryNode TaskSpec)
  , gWork  :: NodeRef "gWork" (mode :- G.LLMNode 'API :@ Entries WorkEntries :@ Tools WorkTools :@ Exits WorkExits)
  , gExit  :: NodeRef "gExit" (mode :- G.ExitNode WorkResult)
  } deriving Generic

-- Note: We're using traditional annotations (Entries, Tools, Exits) for now.
-- Future: Might consolidate to single Config annotation once Generic extraction works.

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLERS (Sketch - won't compile yet)
-- ════════════════════════════════════════════════════════════════════════════

-- | Work node handlers using config record pattern with field-witness routing.
--
-- The config record is instantiated in AsHandler mode, providing handler functions.
{-
-- First, define the graph in AsHandler mode
handlers :: WorkGraph (G.AsHandler '[State SessionState])
handlers = WorkGraph
  { gEntry = NodeRef $ Proxy @TaskSpec
  , gWork  = NodeRef $ WorkConfig { ... }  -- See below
  , gExit  = NodeRef $ Proxy @WorkResult
  }

-- Config record with handlers
workConfig :: WorkConfig (G.AsHandler '[State SessionState])
workConfig = WorkConfig
  { entries = WorkEntries
      { fresh = \taskSpec -> do
          -- Build template context from fresh task
          pure $ WorkContext { task = taskSpec, attempt = 1 }
      , retry = \retryInfo -> do
          -- Build template context from retry
          pure $ WorkContext
            { task = retryInfo.originalTask
            , attempt = retryInfo.attemptCount + 1
            }
      }
  , tools = WorkTools
      { search = \searchQuery -> do
          -- Execute search
          results <- searchDB searchQuery.query
          pure $ SearchResult { results = results }
      , calc = \calcInput -> do
          -- Execute calculation
          result <- evaluate calcInput.expression
          pure $ CalculatorResult { result = result }
      }
  , exits = WorkExits
      { complete = \payload -> do
          -- Route to graph exit (traditional)
          pure $ gotoExit WorkResult
            { resultSummary = payload.summary
            , effortLevel = 5
            }
      , needRetry = \payload -> do
          -- FIELD-WITNESS ROUTING: No string literals!
          -- The field accessor (retry . entries) carries type information
          -- The phantom name from gWork NodeRef validates at compile time
          pure $ gotoNode (gWork handlers) (retry . entries) RetryInfo
            { originalTask = currentTask
            , attemptCount = currentAttempt
            , lastError = payload.reason
            }

          -- Alternative syntax with operator:
          -- pure $ (gWork handlers) --> (retry . entries) $ RetryInfo { ... }
      }
  }
-}

-- ════════════════════════════════════════════════════════════════════════════
-- VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile-time validation that the graph structure is valid.
--
-- This should trigger ValidGraphRecord constraint checking.
-- _validateGraph :: G.ValidGraphRecord WorkGraph => ()
-- _validateGraph = ()

-- ════════════════════════════════════════════════════════════════════════════
-- NOTES
-- ════════════════════════════════════════════════════════════════════════════

{- Design Decisions and Benefits:

1. **Config records**: ✅ KEEPING
   - Standalone, testable units
   - Can unit test handlers: `(fresh . entries $ workConfig) exampleTask`
   - Reusable across graphs
   - Composable (mix and match)

2. **LLM subtypes** (API, CodingAgent, Local): ✅ KEEPING
   - Type families dispatch on execution model
   - Different tool formats (MCP vs PEG)
   - Different exit semantics (choice vs streaming fold)

3. **NodeRef phantom wrappers**: ✅ KEEPING
   - Complexity lives in DSL internals
   - Enables field-witness routing (huge UX win)
   - Users never write phantom types directly

4. **Field-witness routing**: ✅ KEEPING - THE KEY BENEFIT
   ```haskell
   -- OLD: String literals (typo-prone, no IDE support)
   gotoChoice @"gWork" @"retry" retryInfo

   -- NEW: Field witnesses (refactoring-safe, autocomplete)
   gotoNode (gWork graph) (retry . entries) retryInfo
   ```
   Benefits:
   - Rename a field → all call sites update
   - IDE autocomplete for entries
   - Compile-time validation of node + entry existence
   - Type-safe payload matching

5. **Result type inference** (RoutesToResults): ⏸️ DEFER
   - Exit handlers already return typed GotoChoice
   - Type errors catch mismatches anyway
   - Inference adds complexity without clear UX win
   - Can add later if needed

6. **Config annotation consolidation**: ⏸️ DEFER
   - Currently: `Entries E :@ Tools T :@ Exits X`
   - Future: `Config WorkConfig` (requires Generic extraction)
   - Current approach works, consolidation is optimization

Implementation Strategy:
1. ✅ Core types (LLMKind, NodeRef, FunctionGemma) - DONE
2. ✅ Field-witness routing (gotoNode, -->) - DONE
3. ✅ Use HasField for field validation - GHC does it for us!
4. ⏸️ Generic traversal for runtime introspection - Only if needed later
5. ⏸️ Result type inference - DEFER (handlers already type-check)
6. ⏸️ Config consolidation - DEFER (current annotations work fine)

Next Steps:
- Verify this example compiles (validates type system)
- Add unit tests for config record handlers
- Document field-witness routing pattern
- Consider migration guide from string-based routing
-}
