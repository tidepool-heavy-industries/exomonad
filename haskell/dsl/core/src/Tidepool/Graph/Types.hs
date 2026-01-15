
-- | Core types for the Tidepool Graph DSL.
--
-- This module defines core types for the Graph DSL.
-- Users should use the record-based DSL via Tidepool.Graph.Generic;
-- the NodeKind types (LLM, Logic) below are for internal use only.
module Tidepool.Graph.Types
  ( -- * Node Kind
    NodeKind(..)
  , LLMKind(..)

    -- * Annotations
  , type (:@)
  , Input
  , Schema
  , System
  , Template
  , Vision
  , Tools
  , UsesEffects
  , Memory
  , EntryPoint
  , Tool
  , ExitTool
  , ExitEffect
  , ExitPoint
  , Routes
  , Route(..)
  , ToolMetadata(..)
  , Entries
  , Exits

    -- * Fork/Barrier Annotations
  , Spawn
  , Barrier
  , Awaits
  , Arrive(..)

    -- * Parallel Fan-In (Merge)
  , Merge
  , From
  , GroupBy
  , CorrelateBy(..)

    -- * Graph-Level Annotations
  , type (:&)
  , Groups
  , Requires
  , Global
  , Backend

    -- * API Backend Types
  , CloudflareAI
  , NativeAnthropic

    -- * ClaudeCode Annotation
  , ClaudeCode
  , ModelChoice(..)
  , Haiku
  , Sonnet
  , Opus

    -- * ClaudeCode Singletons (demote type-level to runtime)
  , SingModelChoice(..)

    -- * FunctionGemma Annotation
  , FunctionGemma

    -- * MCP Export Annotations
  , MCPExport
  , ToolMeta

    -- * Special Goto Targets
  , Exit
  , Self

    -- * Heterogeneous Lists
  , HList(..)
  ) where

import Data.Aeson (Value)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.TypeLits (Symbol)

-- ════════════════════════════════════════════════════════════════════════════
-- NODE KIND
-- ════════════════════════════════════════════════════════════════════════════

-- | The kind of a node determines its behavior:
--
-- * 'LLM' nodes call the language model and produce output via 'Schema'
-- * 'Logic' nodes run pure or effect-based code and transition via 'Goto'
-- * 'Graph' nodes execute nested graphs
--
-- Note: The bare NodeKind types (LLM, Logic) are obsolete.
-- For the record-based DSL (the only supported syntax), use LLMNode and LogicNode
-- from Tidepool.Graph.Generic.
data NodeKind
  = LLM    -- ^ Node that invokes the LLM. Output flows implicitly via Schema.
  | Logic  -- ^ Node with effect stack. Transitions explicitly via Goto.
  | Graph  -- ^ Nested graph execution.

-- | LLM subtypes determine execution model and tool format.
--
-- Different LLM backends have different capabilities:
--
-- * 'API' - Direct Anthropic/Cloudflare API calls (JSON Schema via MCP)
-- * 'CodingAgent' - Claude Code subprocess via mantle (JSON Schema via MCP)
-- * 'Local' - FunctionGemma streaming (PEG grammar, streaming fold)
--
-- Type families dispatch on this to determine tool format, exit semantics, etc.
data LLMKind
  = API         -- ^ Direct API invocation (Anthropic, Cloudflare)
  | CodingAgent -- ^ Claude Code subprocess execution
  | Local       -- ^ FunctionGemma local streaming model

-- ════════════════════════════════════════════════════════════════════════════
-- ANNOTATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Attach an annotation to a node. Annotations are applied left-to-right:
-- @mode :- G.LLMNode :@ Input A :@ Schema B@ has Input and Schema annotations.
type (:@) :: Type -> Type -> Type
data node :@ annotation
infixl 7 :@

-- | Declares the input type for a node. The handler receives exactly this type.
--
-- **DEPRECATED**: Use 'Entries' for new code. This annotation supports only
-- single-input nodes. 'Entries' allows multiple named entry points per node.
--
-- For fan-in patterns (multiple sources), use 'Either':
-- @Input (Either FromNodeA FromNodeB)@
--
-- For multiple simultaneous inputs, use tuples:
-- @Input (A, B)@
--
-- **Migration path:**
--
-- @
-- -- Old style (single entry):
-- gWork :: mode :- LLMNode :@ Input TaskSpec :@ Schema Result
--
-- -- New style (single named entry):
-- data WorkEntry mode = WorkEntry
--   { weProcess :: mode :- EntryPoint TaskSpec }
--   deriving Generic
--
-- gWork :: mode :- LLMNode :@ Entries WorkEntry :@ Schema Result
-- @
{-# DEPRECATED Input "Use 'Entries' annotation for named entry points" #-}
type Input :: Type -> Type
data Input inputType

-- | Declares the output type of an LLM node. This output becomes available
-- as input to downstream nodes.
type Schema :: Type -> Type
data Schema output

-- | System prompt template for an LLM node. Rendered before the user prompt.
-- Uses a separate TemplateDef from the user 'Template' annotation.
--
-- @
-- gClassify :: mode :- G.LLMNode
--     :@ System ClassifySystemTpl   -- System prompt (optional)
--     :@ Template ClassifyUserTpl   -- User prompt
--     :@ Schema Intent
-- @
type System :: Type -> Type
data System tpl

-- | User prompt template for an LLM node. This is the main prompt that
-- contains the request/context for the LLM.
type Template :: Type -> Type
data Template tpl

-- | Marker for LLM nodes that process images/vision input.
data Vision

-- | Tools record annotation for LLM nodes.
--
-- References a record type that defines named tools available during LLM
-- execution. Each tool handler computes a result from the tool's input.
--
-- @
-- data SearchTool = SearchTool { query :: Text, maxResults :: Int }
--   deriving (Generic, ToJSON, FromJSON)
--
-- instance ToolMetadata SearchTool where
--   toolName = "search_database"
--   toolDescription = "Search the internal database..."
--
-- data WorkTools mode = WorkTools
--   { wtSearch :: mode :- Tool SearchTool SearchResult
--   , wtCalc   :: mode :- Tool CalculatorTool CalcResult
--   } deriving Generic
--
-- gWork :: mode :- LLMNode
--     :@ Entries WorkEntries
--     :@ Tools WorkTools      -- References tool record
--     :@ Exits WorkExits
--     :@ Template WorkTpl
-- @
--
-- **Handler structure:**
--
-- Tool handlers execute when the LLM invokes a tool:
--
-- @
-- workTools :: WorkTools (AsHandler '[State S])
-- workTools = WorkTools
--   { wtSearch = \input -> do
--       results <- searchDB (query input) (maxResults input)
--       pure SearchResult { srResults = results }
--   , wtCalc = \input -> do
--       result <- evalExpr (expression input)
--       pure CalcResult { crValue = result }
--   }
-- @
--
-- **Tool reusability:**
--
-- The same tool type can be used in multiple graphs with different handlers:
--
-- @
-- data AnalyzeTools mode = AnalyzeTools
--   { atSearch :: mode :- Tool SearchTool AnalysisResult }
--   deriving Generic
--
-- analyzeTools = AnalyzeTools
--   { atSearch = \input -> searchTestDB input  -- Different implementation
--   }
-- @
--
-- **Optional annotation:**
--
-- Unlike Entries and Exits, Tools is optional. LLM nodes can have no tools:
--
-- @
-- gThink :: mode :- LLMNode
--     :@ Entries ThinkEntries
--     :@ Exits ThinkExits
--     :@ Template ThinkTpl
-- -- No Tools annotation - this LLM just thinks, doesn't call tools
-- @
--
-- See 'Tool' for tool marker type and 'ToolMetadata' for tool metadata.
--
-- **Breaking change from type-level list:**
--
-- Previous versions used @Tools '[SearchTool, CalculatorTool]@. This has been
-- replaced with record-based tools for named fields and ToolMetadata instances.
--
-- Migration path:
--
-- @
-- -- Old (breaks in this version):
-- :@ Tools '[SearchTool, CalculatorTool]
--
-- -- New (required):
-- data MyTools mode = MyTools
--   { mtSearch :: mode :- Tool SearchTool SearchResult
--   , mtCalc :: mode :- Tool CalculatorTool CalcResult
--   } deriving Generic
--
-- :@ Tools MyTools
-- @
type Tools :: (Type -> Type) -> Type
data Tools toolsRecord

-- | Effect stack for Logic nodes. Contains the effects the handler can use,
-- including 'Goto' effects for transitions.
--
-- Note: This takes a list of effects with kind [Effect] where
-- Effect = (Type -> Type) -> Type -> Type
--
-- Renamed from 'Eff' to avoid conflict with Effectful's 'Eff' monad type.
--
-- @
-- UsesEffects '[State MyState, Goto "nextNode" PayloadType, Goto Exit ResultType]
-- @
type UsesEffects :: [k] -> Type
data UsesEffects effects

-- | Node-private persistent memory. Each node can declare its own state type
-- that persists across graph runs. Only this node can access its Memory.
--
-- @
-- gExplore :: mode :- G.LLMNode
--     :@ Input Query
--     :@ Schema Findings
--     :@ Memory ExploreMem   -- Private state for this node
-- @
type Memory :: Type -> Type
data Memory stateType

-- ════════════════════════════════════════════════════════════════════════════
-- ENTRY/EXIT RECORD ANNOTATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Entry point marker for named entry fields in entry records.
--
-- Used in entry record definitions to declare the payload type for each entry.
--
-- @
-- data WorkEntries mode = WorkEntries
--   { weFresh :: mode :- EntryPoint TaskSpec
--   , weRetry :: mode :- EntryPoint RetryInfo
--   }
--   deriving Generic
-- @
--
-- Each entry point becomes a handler that receives the payload and builds
-- template context:
--
-- @
-- weFresh :: TaskSpec -> Eff es TemplateContext
-- weRetry :: RetryInfo -> Eff es TemplateContext
-- @
--
-- See 'Entries' for usage in node annotations.
type EntryPoint :: Type -> Type
data EntryPoint payload

-- | Regular tool marker for LLM-invocable tools during node execution.
--
-- Used in tool record definitions to declare input and result types for tools.
-- Tools are domain types that can be reused across multiple graphs/nodes.
--
-- @
-- data SearchTool = SearchTool
--   { query :: Text
--   , maxResults :: Int
--   } deriving (Generic, ToJSON, FromJSON)
--
-- instance ToolMetadata SearchTool where
--   toolName = "search_database"
--   toolDescription = "Search the internal database..."
--
-- data WorkTools mode = WorkTools
--   { wtSearch :: mode :- Tool SearchTool SearchResult
--   , wtCalc   :: mode :- Tool CalculatorTool CalcResult
--   } deriving Generic
-- @
--
-- Each tool becomes an LLM-invocable MCP tool:
-- - Tool name from ToolMetadata instance (stable across refactors)
-- - Tool schema from ToolMetadata (JSON Schema)
-- - Field name (wtSearch) used for routing to handler
--
-- Tool handlers execute when LLM invokes the tool:
--
-- @
-- wtSearch :: SearchTool -> Eff es SearchResult
-- wtCalc :: CalculatorTool -> Eff es CalcResult
-- @
--
-- See 'Tools' for usage in node annotations and 'ToolMetadata' for metadata.
type Tool :: Type -> Type -> Type
data Tool input result

-- | Exit tool marker for LLM-driven exits with structured output.
--
-- Used in exit record definitions for exits where the LLM generates a JSON
-- payload matching the tool schema. Supports multiple result types for
-- flexible routing.
--
-- @
-- data CompleteTaskTool = CompleteTaskTool
--   { result :: TaskResult
--   , metadata :: Map Text Value
--   } deriving (Generic, ToJSON, FromJSON)
--
-- instance ToolMetadata CompleteTaskTool where
--   toolName = "complete_task"
--   toolDescription =
--     "Mark the task as successfully completed. \
--     \Includes the final result and optional metadata. \
--     \Use when all work is done and validated."
--
-- data WorkExits mode = WorkExits
--   { weComplete :: mode :- ExitTool CompleteTaskTool '[TaskResult]
--       :@ Routes '[ExitGraph]
--   , weNeedInfo :: mode :- ExitTool RequestInfoTool '[RetryInfo]
--       :@ Routes '[ToEntry "gWork" "weRetry"]
--   } deriving Generic
-- @
--
-- Exit tools become MCP tools that conclude the LLM turn:
-- - Tool name from ToolMetadata instance
-- - Tool schema from ToolMetadata (JSON Schema)
-- - LLM selects tool and generates payload
-- - Handler routes based on payload
--
-- Exit handlers receive the payload and must route via GotoChoice:
--
-- @
-- weComplete :: CompleteTaskTool -> Eff es (GotoChoice '[To Exit TaskResult])
-- weNeedInfo :: RequestInfoTool -> Eff es (GotoChoice '[To "gWork" RetryInfo])
-- @
--
-- The result type list '[TaskResult] specifies possible output types.
-- All route targets must accept one of these types (compile-time validated).
--
-- See 'ExitEffect' for effect-based exits and 'Exits' for node annotations.
type ExitTool :: Type -> [Type] -> Type
data ExitTool payload results

-- | Effect-based exit marker for tool-triggered early exit.
--
-- Used when a tool handler encounters an error or condition requiring
-- immediate exit, without LLM generation. No ToolMetadata instance needed.
--
-- @
-- data WorkTools mode = WorkTools
--   { wtSearch :: mode :- Tool SearchTool SearchResult
--   }
--
-- data WorkExits mode = WorkExits
--   { weComplete :: mode :- ExitTool CompleteTaskTool '[TaskResult]
--       :@ Routes '[ExitGraph]
--   , weSearchFailed :: mode :- ExitEffect SearchFailure
--       :@ Routes '[ToEntry "gWork" "weRetry"]
--   }
-- @
--
-- Tool handlers can trigger early exit via exitWith effect:
--
-- @
-- wtSearch :: SearchTool -> Eff (ExitEffect ': es) SearchResult
-- wtSearch input = do
--   results <- searchDB (query input)
--   when (null results) $ do
--     exitWith (SearchFailure input)  -- Triggers weSearchFailed handler
--   pure (SearchResult results)
-- @
--
-- The exit handler routes based on the failure payload:
--
-- @
-- weSearchFailed :: SearchFailure -> Eff es (GotoChoice '[To "gWork" RetryInfo])
-- weSearchFailed failure = do
--   log ("Search failed: " <> show failure)
--   pure $ gotoChoice @"gWork" @"weRetry" (buildRetry failure)
-- @
--
-- Key differences from ExitTool:
-- - No LLM generation (tool encounters error)
-- - No ToolMetadata instance required
-- - Triggered via exitWith effect (not tool call)
--
-- See 'ExitTool' for LLM-driven exits and 'Exits' for node annotations.
type ExitEffect :: Type -> Type
data ExitEffect payload

-- | Exit point marker for named exit fields in exit records.
--
-- **DEPRECATED**: Use 'ExitTool' for new code. ExitTool supports multiple
-- result types and explicit route specifications via Routes annotation.
--
-- Used in exit record definitions to declare the payload type for each exit.
--
-- @
-- data WorkExits mode = WorkExits
--   { weComplete :: mode :- ExitPoint CompletePayload
--   , weBlocked  :: mode :- ExitPoint BlockedPayload
--   }
--   deriving Generic
-- @
--
-- Each exit point becomes an LLM tool. When the LLM selects a tool, the
-- corresponding handler receives the parsed payload and routes:
--
-- @
-- weComplete :: CompletePayload -> Eff es (GotoChoice targets)
-- weBlocked  :: BlockedPayload -> Eff es (GotoChoice targets)
-- @
--
-- **Migration path:**
--
-- @
-- -- Old style:
-- weComplete :: mode :- ExitPoint CompletePayload
--
-- -- New style (adds result types and routes):
-- weComplete :: mode :- ExitTool CompletePayload '[TaskResult]
--     :@ Routes '[ExitGraph]
-- @
--
-- See 'Exits' for usage in node annotations.
{-# DEPRECATED ExitPoint "Use 'ExitTool' with Routes annotation for explicit routing" #-}
type ExitPoint :: Type -> Type
data ExitPoint payload

-- | Route specification for exit fields.
--
-- Defines where an exit handler can route to. Exit fields must be annotated
-- with Routes to specify valid routing targets.
--
-- @
-- data WorkExits mode = WorkExits
--   { weComplete :: mode :- ExitTool CompleteTaskTool '[TaskResult]
--       :@ Routes '[ExitGraph]  -- Can only route to graph exit
--   , weRetry :: mode :- ExitTool RetryTool '[RetryInfo]
--       :@ Routes '[ToEntry "gWork" "weFresh"]  -- Routes to specific entry
--   , weDecide :: mode :- ExitTool DecideTool '[TaskResult, RetryInfo]
--       :@ Routes '[ExitGraph, ToEntry "gWork" "weRetry"]  -- Multiple routes
--   }
-- @
--
-- **Route types:**
--
-- - 'ExitGraph' - Route to graph exit (final result)
-- - 'ToEntry node entry' - Route to specific entry point on named node
-- - 'ToNode node' - Route to node (deprecated Input support)
--
-- **Type validation:**
--
-- Routes are validated at compile time:
-- 1. Target node/entry must exist in graph
-- 2. Result types must match target input types
-- 3. At least one result type must work for each route
--
-- See 'ExitTool' for usage with exit tools.
data Route
  = ExitGraph              -- ^ Route to graph exit
  | ToEntry Symbol Symbol  -- ^ Route to node entry (node name, entry field name)
  | ToNode Symbol          -- ^ Route to node (deprecated Input support)

-- | Routes annotation for exit fields.
--
-- Specifies the list of valid routing targets for an exit field.
-- Each exit field must have a Routes annotation.
--
-- @
-- weComplete :: mode :- ExitTool CompleteTaskTool '[TaskResult]
--     :@ Routes '[ExitGraph]
-- @
--
-- **Multiple routes:**
--
-- Exit fields can route to multiple targets. The handler chooses at runtime:
--
-- @
-- weDecide :: mode :- ExitTool DecisionPayload '[TaskResult, RetryInfo]
--     :@ Routes '[ExitGraph, ToEntry "gWork" "weRetry"]
--
-- -- Handler picks route based on result:
-- weDecide payload = case analyzeResult payload of
--   Success r -> pure $ gotoExit r         -- Routes via ExitGraph
--   NeedsRetry i -> pure $ gotoChoice @"gWork" @"weRetry" i  -- Routes via ToEntry
-- @
--
-- **Compile-time validation:**
--
-- GHC validates routes when the graph is defined:
-- - 'ExitGraph' requires graph has Exit node
-- - 'ToEntry node entry' requires node exists with named entry
-- - Result types must match target input types
--
-- See 'Route' for available route types and 'ExitTool' for exit tool definitions.
type Routes :: [Route] -> Type
data Routes routes

-- | Tool metadata typeclass for LLM-invocable tools.
--
-- Provides metadata for generating MCP tool definitions. Used by both regular
-- tools (Tool) and exit tools (ExitTool). Metadata only - execution lives in
-- handler records.
--
-- @
-- data SearchTool = SearchTool
--   { query :: Text
--   , maxResults :: Int
--   } deriving (Generic, ToJSON, FromJSON)
--
-- instance ToolMetadata SearchTool where
--   toolName = "search_database"
--   toolDescription =
--     "Search the internal database for relevant information. \
--     \Returns up to maxResults matching documents. \
--     \Use when the LLM needs to look up specific facts."
--   -- toolInputSchema derived from Generic automatically
--   toolExamples =
--     [ object [ "query" .= ("user authentication" :: Text)
--              , "maxResults" .= (10 :: Int)
--              ]
--     ]
-- @
--
-- **Reusability:**
--
-- Tools are domain types, not graph artifacts. The same tool can be used
-- in multiple graphs with different implementations:
--
-- @
-- data WorkTools mode = WorkTools
--   { wtSearch :: mode :- Tool SearchTool SearchResult }
--   deriving Generic
--
-- data AnalyzeTools mode = AnalyzeTools
--   { atSearch :: mode :- Tool SearchTool AnalysisResult }
--   deriving Generic
--
-- -- Different handlers for same tool:
-- workSearch :: SearchTool -> Eff es SearchResult
-- workSearch = searchProdDB
--
-- analyzeSearch :: SearchTool -> Eff es AnalysisResult
-- analyzeSearch = searchTestDB
-- @
--
-- **MCP tool generation:**
--
-- Tool metadata is used to generate MCP tool definitions:
--
-- @
-- {
--   "name": "search_database",  -- from toolName
--   "description": "Search the internal database...",  -- from toolDescription
--   "input_schema": { ... }     -- from toolInputSchema (derived or manual)
-- }
-- @
--
-- **Exit tools use same typeclass:**
--
-- @
-- data CompleteTaskTool = CompleteTaskTool { ... }
--   deriving (Generic, ToJSON, FromJSON)
--
-- instance ToolMetadata CompleteTaskTool where
--   toolName = "complete_task"
--   toolDescription = "Mark the task as successfully completed..."
-- @
--
-- See 'Tool' for regular tools and 'ExitTool' for exit tools.
class ToolMetadata toolPayload where
  -- | Stable tool name used in MCP calls.
  --
  -- Should be snake_case by convention. This name appears in LLM tool calls
  -- and should remain stable across refactors (unlike field names).
  --
  -- @
  -- toolName = "search_database"
  -- @
  toolName :: Text

  -- | 2-3 sentence description for LLM context.
  --
  -- Should explain:
  -- - What the tool does
  -- - When to use it
  -- - What parameters mean (if not obvious)
  --
  -- @
  -- toolDescription =
  --   "Search the internal database for relevant information. \
  --   \Returns up to maxResults matching documents. \
  --   \Use when the LLM needs to look up specific facts."
  -- @
  toolDescription :: Text

  -- | JSON Schema for tool parameters.
  --
  -- Defaults to derivation from Generic instance. Override for custom schemas.
  --
  -- @
  -- -- Default: derived from Generic
  -- toolInputSchema = deriveSchema
  --
  -- -- Custom schema:
  -- toolInputSchema = object
  --   [ "type" .= ("object" :: Text)
  --   , "properties" .= object
  --       [ "query" .= object ["type" .= ("string" :: Text)]
  --       , "maxResults" .= object ["type" .= ("integer" :: Text)]
  --       ]
  --   , "required" .= ["query" :: Text]
  --   ]
  -- @
  toolInputSchema :: Value
  toolInputSchema = error "ToolMetadata: toolInputSchema not implemented (derive from Generic or provide manual schema)"

  -- | Optional usage examples for LLM context.
  --
  -- Provides example tool invocations to guide the LLM. Defaults to empty list.
  --
  -- @
  -- toolExamples =
  --   [ object [ "query" .= ("user login flow" :: Text)
  --            , "maxResults" .= (5 :: Int)
  --            ]
  --   , object [ "query" .= ("payment processing" :: Text)
  --            , "maxResults" .= (10 :: Int)
  --            ]
  --   ]
  -- @
  toolExamples :: [Value]
  toolExamples = []

-- | Multiple entry points annotation for LLM nodes.
--
-- References a record type that defines named entry points. Each entry
-- handler builds template context from its payload type.
--
-- @
-- data WorkEntries mode = WorkEntries
--   { weFresh :: mode :- EntryPoint TaskSpec
--   , weRetry :: mode :- EntryPoint RetryInfo
--   }
--   deriving Generic
--
-- gWork :: mode :- LLMNode
--     :@ Entries WorkEntries
--     :@ Template WorkTpl
--     :@ Exits WorkExits
-- @
--
-- **Handler structure:**
--
-- All entries share the same Template and produce the same TemplateContext type.
-- The template context type is derived from the @Template@ annotation.
--
-- @
-- -- In AsHandler mode, entries become handler functions:
-- workEntries :: WorkEntries (AsHandler '[State S])
-- workEntries = WorkEntries
--   { weFresh = \taskSpec -> pure WorkContext { ... }
--   , weRetry = \retryInfo -> pure WorkContext { ... }
--   }
-- @
--
-- **Routing to entries:**
--
-- Use 'gotoChoice' with both target node and entry field name:
--
-- @
-- gotoChoice \@\"gWork\" \@\"weFresh\" taskSpec   -- Route to weFresh entry
-- gotoChoice \@\"gWork\" \@\"weRetry\" retryInfo  -- Route to weRetry entry
-- @
--
-- Field names are validated at compile time - GHC checks that \"weFresh\"
-- exists in WorkEntries and the payload type matches.
--
-- See 'Exits' for the output side, and 'EntryPoint' for entry definitions.
type Entries :: (Type -> Type) -> Type
data Entries entriesRecord

-- | Multiple exit points annotation for LLM nodes.
--
-- References a record type that defines named exit points. Each exit becomes
-- an LLM tool that the model can select.
--
-- @
-- data WorkExits mode = WorkExits
--   { weComplete :: mode :- ExitPoint CompletePayload
--   , weBlocked  :: mode :- ExitPoint BlockedPayload
--   }
--   deriving Generic
--
-- gWork :: mode :- LLMNode
--     :@ Entries WorkEntries
--     :@ Template WorkTpl
--     :@ Exits WorkExits
-- @
--
-- **Exit = Tool:**
--
-- Each exit field generates an MCP tool:
-- - Tool name: snake_case version of field name (@weComplete@ -> @work_we_complete@)
-- - Tool schema: JSON Schema derived from payload type
--
-- **Handler structure:**
--
-- Each exit handler receives the structured output from the LLM (parsed from
-- the tool call) and must route to the next node:
--
-- @
-- -- In AsHandler mode, exits become handler functions:
-- workExits :: WorkExits (AsHandler '[State S])
-- workExits = WorkExits
--   { weComplete = \payload -> do
--       log \"Work completed\"
--       pure $ gotoExit (success payload)
--   , weBlocked = \payload -> do
--       log \"Work blocked, retrying\"
--       pure $ gotoChoice \@\"gWork\" \@\"weRetry\" (buildRetryInfo payload)
--   }
-- @
--
-- **Routing from exits:**
--
-- Exit handlers can route back to entry points, enabling retry loops:
--
-- @
-- weBlocked = \payload -> pure $ gotoChoice \@\"gWork\" \@\"weRetry\" retryInfo
-- @
--
-- See 'Entries' for the input side, and 'ExitPoint' for exit definitions.
type Exits :: (Type -> Type) -> Type
data Exits exitsRecord

-- ════════════════════════════════════════════════════════════════════════════
-- FORK/BARRIER ANNOTATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Spawn targets for ForkNode. Lists which nodes to spawn in parallel.
--
-- Uses 'To' markers (from Goto module) to specify target name and payload type:
--
-- @
-- fork :: mode :- ForkNode
--     :@ Input Task
--     :@ Spawn '[To "worker1" Task, To "worker2" Task]
--     :@ Barrier "merge"
-- @
--
-- The ForkNode handler returns an HList of payloads, one per spawn target.
type Spawn :: [Type] -> Type
data Spawn targets

-- | Barrier target for ForkNode. Names which BarrierNode collects the results.
--
-- @
-- fork :: mode :- ForkNode
--     :@ Input Task
--     :@ Spawn '[To "w1" Task, To "w2" Task]
--     :@ Barrier "merge"   -- Results collected at "merge" node
-- @
type Barrier :: Symbol -> Type
data Barrier target

-- | Types expected by a BarrierNode. Lists the result types from each path.
--
-- @
-- merge :: mode :- BarrierNode
--     :@ Awaits '[ResultA, ResultB]   -- Expects two results
--     :@ UsesEffects '[Goto Exit (ResultA, ResultB)]
-- @
--
-- The BarrierNode handler receives the collected results as an HList.
-- Supports heterogeneous types: different workers can produce different types.
type Awaits :: [Type] -> Type
data Awaits resultTypes

-- | Arrive annotation for worker nodes spawned by ForkNode.
--
-- Used in UsesEffects to indicate the worker deposits a result at its barrier.
-- The barrier name (Symbol) identifies which BarrierNode receives the result:
--
-- @
-- worker :: mode :- LLMNode
--     :@ Input Task
--     :@ Schema StepResult
--     :@ UsesEffects '[Goto Self Task, Arrive "hJoin" Result]
-- @
--
-- Unlike 'Goto Exit', which terminates the graph, 'Arrive' suspends the
-- current path and deposits the result for the named barrier to collect.
-- Workers can self-loop ('Goto Self') until ready, then 'Arrive'.
--
-- The barrier name allows the router to dispatch arrive messages to the
-- correct BarrierNode actor, enabling type-safe fan-in.
--
-- Note: The extra type parameter @r@ gives 'Arrive' the same kind as other
-- effects (Type -> Type), so it can appear in 'UsesEffects' alongside 'Goto'.
type Arrive :: Symbol -> Type -> Type -> Type
data Arrive (barrierName :: Symbol) resultType r where
  ArriveOp :: result -> Arrive barrierName result ()

-- ════════════════════════════════════════════════════════════════════════════
-- PARALLEL FAN-IN (MERGE)
-- ════════════════════════════════════════════════════════════════════════════

-- | Input annotation for nodes that gather results from parallel fan-out.
--
-- Unlike regular 'Input' which receives from a single source, 'Merge' declares
-- that this node waits for results from multiple sources, grouped by a
-- correlation key.
--
-- @
-- gather :: mode :- LogicNode
--     :@ Input (Merge '[From "payment" PaymentResult, From "inventory" InventoryResult])
--     :@ GroupBy OrderId
--     :@ UsesEffects '[Goto Exit OrderResult]
-- @
--
-- The handler receives an 'HList' of results once all sources have arrived
-- for a given correlation key.
type Merge :: [Type] -> Type
data Merge sources

-- | Source marker for 'Merge'. Identifies which node the result came from.
--
-- @
-- Merge '[From "payment" PaymentResult, From "inventory" InventoryResult]
-- @
--
-- Each 'From' specifies:
--   * The source node name (type-level Symbol)
--   * The expected payload type from that source
--
-- This enables type-safe routing: even if two sources produce the same type,
-- they're distinguished by their source node name.
type From :: Symbol -> Type -> Type
data From source payload

-- | Annotation specifying the correlation key type for 'Merge'.
--
-- Results from parallel workers are grouped by this key. All result types
-- in the 'Merge' must have a 'CorrelateBy' instance for this key type.
--
-- @
-- gather :: mode :- LogicNode
--     :@ Input (Merge '[From "a" ResultA, From "b" ResultB])
--     :@ GroupBy OrderId  -- Group results by OrderId
--     :@ UsesEffects '[Goto Exit Combined]
-- @
type GroupBy :: Type -> Type
data GroupBy key

-- | Typeclass for extracting a correlation key from a payload.
--
-- Used by 'Merge' nodes to group results from parallel workers. Each result
-- type must provide a way to extract the correlation key.
--
-- @
-- instance CorrelateBy OrderId PaymentResult where
--   correlationKey = (.orderId)
--
-- instance CorrelateBy OrderId InventoryResult where
--   correlationKey = (.orderId)
-- @
--
-- This enables the runtime to match up results that belong to the same
-- fan-out operation, even when multiple fan-outs are in flight.
class CorrelateBy key a where
  correlationKey :: a -> key

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH-LEVEL ANNOTATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Attach a graph-level annotation (not commonly used with record-based graphs).
type (:&) :: Type -> Type -> Type
data graph :& annotation
infixl 4 :&

-- | Organize nodes into named groups for Mermaid subgraph rendering.
--
-- @
-- Groups '[
--     '("intake", '["gClassify", "gRoute"])
--   , '("handlers", '["gRefund", "gTechnical", "gBilling"])
--   ]
-- @
type Groups :: [(Symbol, [Symbol])] -> Type
data Groups groups

-- | Declare effects required by the graph at the top level.
-- Used for documentation and runner configuration.
type Requires :: [Type] -> Type
data Requires effects

-- | Graph-level shared state accessible to all nodes. Unlike node-private
-- 'Memory', Global state can be read and updated by any node in the graph.
--
-- Used with the (:&) operator for graph-level annotations.
type Global :: Type -> Type
data Global stateType

-- | API backend selection. Determines whether LLM calls go to Cloudflare AI
-- or the native Anthropic API. Used with the (:&) operator.
--
-- @
-- type MyGraph = Graph '[...] :& Backend NativeAnthropic
-- @
--
-- Note: 'ClaudeCode' annotation is only valid with 'NativeAnthropic' backend.
-- Using ClaudeCode with CloudflareAI will produce a compile-time error.
type Backend :: Type -> Type
data Backend backendType

-- ════════════════════════════════════════════════════════════════════════════
-- API BACKEND TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Cloudflare AI backend. Uses Cloudflare Workers AI for LLM calls.
-- Does not support 'ClaudeCode' annotation (no local subprocess access).
data CloudflareAI

-- | Native Anthropic API backend. Calls Anthropic API directly.
-- Supports 'ClaudeCode' annotation for spawning Claude Code sessions.
data NativeAnthropic

-- ════════════════════════════════════════════════════════════════════════════
-- CLAUDE CODE ANNOTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Model selection for Claude Code sessions.
data ModelChoice
  = Haiku   -- ^ Fast, cost-effective model
  | Sonnet  -- ^ Balanced performance/cost
  | Opus    -- ^ Most capable model

-- | Type-level aliases for promoted ModelChoice constructors.
type Haiku = 'Haiku
type Sonnet = 'Sonnet
type Opus = 'Opus

-- | Marks an LLM node as executed via Claude Code subprocess instead of API.
--
-- When present, the node's template is rendered and passed to @claude -p@
-- via mantle, which spawns a Claude Code session in a managed worktree and
-- returns JSON output.
--
-- @
-- gWork :: mode :- G.LLMNode
--     :@ Input BeadInfo
--     :@ Template WorkTpl
--     :@ Schema WorkResult
--     :@ ClaudeCode 'Sonnet
-- @
--
-- Parameters:
--
-- * @model@ - Which Claude model to use (Haiku, Sonnet, Opus)
--
-- Note: Only valid with 'Backend NativeAnthropic'. Using with CloudflareAI
-- will produce a compile-time type error.
type ClaudeCode :: ModelChoice -> Type
data ClaudeCode model


-- ════════════════════════════════════════════════════════════════════════════
-- CLAUDECODE SINGLETONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Demote type-level ModelChoice to runtime value.
--
-- This enables compile-time validated ClaudeCode handlers where the model
-- is derived from the type annotation rather than passed as a runtime argument.
class SingModelChoice (m :: ModelChoice) where
  singModelChoice :: ModelChoice

instance SingModelChoice 'Haiku where singModelChoice = Haiku
instance SingModelChoice 'Sonnet where singModelChoice = Sonnet
instance SingModelChoice 'Opus where singModelChoice = Opus


-- ════════════════════════════════════════════════════════════════════════════
-- FUNCTIONGEMMA ANNOTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Marker for FunctionGemma local streaming execution (LLMKind 'Local).
--
-- Unlike API and ClaudeCode execution which make single request/response calls,
-- FunctionGemma streams delimiter-separated messages where each message triggers
-- the exit handler as a fold function.
--
-- Example usage:
--
-- @
-- gWork :: mode :- LLMNode 'Local WorkConfig
--     :@ FunctionGemma
-- @
--
-- Exit handlers for Local nodes use ExitEffect instead of ExitTool:
-- - No routing via GotoChoice (handlers return ())
-- - Early exit via `exitWith` effect
-- - Each streamed message invokes handler as fold step
--
-- Note: This is a documentation marker for future implementation.
-- Runtime support is planned but not yet implemented.
data FunctionGemma

-- ════════════════════════════════════════════════════════════════════════════
-- MCP EXPORT ANNOTATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Mark an Entry point for MCP server exposure.
--
-- When a graph Entry has MCPExport, it becomes an MCP tool that external
-- clients can invoke. The input type becomes the tool's parameter schema.
--
-- @
-- data MyGraph mode = MyGraph
--   { search :: mode :- Entry SearchInput :@ MCPExport
--       :@ ToolMeta '("search", "Search the codebase")
--   }
-- @
--
-- Use with 'ToolMeta' to provide tool name and description.
data MCPExport

-- | Provide name and description for an MCP-exported Entry.
--
-- @
-- :@ ToolMeta '("tool_name", "Tool description for LLM")
-- @
--
-- The name should be snake_case. The description appears in the MCP
-- tool listing and helps the LLM understand when to use the tool.
type ToolMeta :: (Symbol, Symbol) -> Type
data ToolMeta nameAndDesc

-- ════════════════════════════════════════════════════════════════════════════
-- SPECIAL GOTO TARGET
-- ════════════════════════════════════════════════════════════════════════════

-- | Special marker type used as a target for the Goto effect to exit the graph.
--
-- @
-- -- In a Logic node's effect stack:
-- UsesEffects '[State S, Goto "gNextNode" A, Goto Exit FinalResult]
-- @
--
-- This @Exit@ type is used as a Goto target. Record-based graphs use @G.Exit@
-- from "Tidepool.Graph.Generic" for their exit field definitions.
data Exit

-- | Self-loop marker for transitions back to the current node.
--
-- Used for retry/continuation patterns:
--
-- @
-- Goto Self UpdatedState
-- @
data Self

-- ════════════════════════════════════════════════════════════════════════════
-- HETEROGENEOUS LISTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Type-indexed heterogeneous list.
--
-- Used for spawn payloads and barrier awaits where each element can have
-- a different type. Enables recursive type class instances for dispatch.
--
-- @
-- -- A list containing Int, Text, and Bool:
-- myList :: HList '[Int, Text, Bool]
-- myList = 42 ::: "hello" ::: True ::: HNil
--
-- -- Pattern matching:
-- processFirst :: HList '[a, b, c] -> a
-- processFirst (x ::: _ ::: _ ::: HNil) = x
-- @
--
-- For ForkNode spawn payloads:
--
-- @
-- SpawnPayloads '[To "w1" TaskA, To "w2" TaskB]
--   = HList '[TaskA, TaskB]
-- @
--
-- For BarrierNode awaits:
--
-- @
-- AwaitsHList '[ResultA, ResultB]
--   = HList '[ResultA, ResultB]
-- @
data HList (ts :: [Type]) where
  HNil  :: HList '[]
  (:::) :: t -> HList ts -> HList (t ': ts)

infixr 5 :::
