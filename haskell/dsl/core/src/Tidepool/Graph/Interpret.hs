{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-redundant-constraints #-}
-- Pattern exhaustiveness checker doesn't understand GADT constraints for OneOf
-- Redundant constraints are documentation for type-level dispatch

-- | Typed graph interpreter using OneOf dispatch.
--
-- This module provides fully typed graph execution without Dynamic or
-- unsafeCoerce. The key insight is that 'GotoChoice' wraps 'OneOf', where
-- position encodes which target was chosen and pattern matching gives exact
-- typed payloads.
--
-- = Design
--
-- @
-- GotoChoice '[To \"nodeA\" PayloadA, To \"nodeB\" PayloadB, To Exit Result]
--   = GotoChoice (OneOf '[PayloadA, PayloadB, Result])
--
-- Pattern matching:
--   Here payloadA      → call nodeA handler with PayloadA
--   There (Here payloadB) → call nodeB handler with PayloadB
--   There (There (Here result)) → return Result (exit)
-- @
--
-- The 'DispatchGoto' typeclass inductively pattern matches on the target list,
-- using 'HasField' to get handlers from the graph record by name.
module Tidepool.Graph.Interpret
  ( -- * Dispatch Typeclass
    DispatchGoto(..)
    -- * Graph Interpretation
  , runGraph
  , runGraphFrom
  , runGraphWith
    -- * EntryNode Handler Discovery
  , FindEntryHandler
    -- * Handler Invocation
  , CallHandler(..)
    -- * LLM Handler Execution
  , executeLLMHandler
  , executeClaudeCodeHandler
  , executeGeminiHandler

    -- * Self-Loop Dispatch
  , DispatchGotoWithSelf(..)

    -- * Transition Conversion
  , ConvertTransitionHint(..)

    -- * Fork/Barrier Orchestration
  , SpawnWorkers(..)
  ) where

import Control.Monad (when)
import Debug.Trace (trace)
import Data.Aeson (Value, toJSON)
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Freer (Eff, Member)
import GHC.Generics (Generic(..))
import GHC.Records (HasField(..))
import GHC.TypeLits (Symbol, KnownSymbol, ErrorMessage(..), symbolVal)
import Tidepool.Graph.Errors
  ( HR, Blank, WhatHappened, HowItWorks, Fixes
  , Indent, CodeLine, Bullet
  , Unsatisfiable, unsatisfiable
  , SelfLoopDispatchError
  )
import Text.Ginger.TH (TypedTemplate, runTypedTemplate)
import Text.Parsec.Pos (SourcePos)

import Tidepool.Effect (LLM)
import Tidepool.Effect.Gemini (GeminiOp, GeminiModel(..), SGeminiModel(..), SingGeminiModel(..), runGemini, GeminiResult(..))
import Tidepool.Effect.NodeMeta (NodeMeta, NodeMetadata(..), withNodeMeta, GraphMeta, GraphMetadata(..), getGraphMeta)
import Tidepool.Effect.Session (Session, SessionOutput(..), SessionId(..), SessionOperation(..), startSession, continueSession, forkSession, ToolCall(..))
import Tidepool.Effect.Types (TurnOutcome(..), TurnParseResult(..), TurnResult(..), runTurn)
import Tidepool.Graph.Edges (GetInput, GetSpawnTargets, GetBarrierTarget, GetAwaits)
import Tidepool.Graph.Generic (AsHandler, FieldsWithNamesOf, SpawnPayloads, SpawnPayloadsInner, AwaitsHList, GetNodeDef)
import Tidepool.Graph.Reify (IsForkNode)
import Tidepool.Graph.Generic.Core (EntryNode, AsGraph)
import qualified Tidepool.Graph.Generic.Core as G (ExitNode)
import Tidepool.Graph.Goto (GotoChoice, To, LLMHandler(..), ClaudeCodeLLMHandler(..), ClaudeCodeResult(..), GeminiLLMHandler(..))
import Tidepool.Graph.Goto.Internal (GotoChoice(..), OneOf(..))
import Tidepool.Graph.Template (GingerContext)
import Tidepool.Graph.Types (Exit, Self, Arrive, SingModelChoice(..), HList(..), ModelChoice)
import Tidepool.Schema (schemaToValue)
import Tidepool.StructuredOutput (StructuredOutput(..), ValidStructuredOutput, ClaudeCodeSchema(..), DecisionTool, formatDiagnostic)
import qualified Tidepool.StructuredOutput.DecisionTools as DT

-- | Effect type alias (freer-simple effects have kind Type -> Type).
type Effect = Type -> Type


-- ════════════════════════════════════════════════════════════════════════════
-- TYPE-LEVEL UTILITIES (local definitions)
-- ════════════════════════════════════════════════════════════════════════════

-- | Type-level If (polykinded).
type IfMaybe :: Bool -> Maybe k -> Maybe k -> Maybe k
type family IfMaybe cond t f where
  IfMaybe 'True  t _ = t
  IfMaybe 'False _ f = f


-- ════════════════════════════════════════════════════════════════════════════
-- ENTRY HANDLER DISCOVERY
-- ════════════════════════════════════════════════════════════════════════════

-- | Find the first field whose node definition accepts the entry type.
--
-- Iterates through (fieldName, nodeDef) pairs from 'FieldsWithNamesOf' and
-- returns the first field name where 'GetInput' matches the entry type.
--
-- @
-- -- For a graph with:
-- --   entry   :: mode :- EntryNode Int
-- --   compute :: mode :- LogicNode :@ Input Int :@ UsesEffects '[...]
-- --   exit    :: mode :- ExitNode Int
-- --
-- FindEntryHandler Int fields = 'Just "compute"
-- @
type FindEntryHandler :: Type -> [(Symbol, Type)] -> Maybe Symbol
type family FindEntryHandler entryType fields where
  FindEntryHandler _ '[] = 'Nothing
  FindEntryHandler entryType ('(name, EntryNode _) ': rest) =
    FindEntryHandler entryType rest  -- Skip EntryNode marker
  FindEntryHandler entryType ('(name, G.ExitNode _) ': rest) =
    FindEntryHandler entryType rest  -- Skip ExitNode marker
  FindEntryHandler entryType ('(name, def) ': rest) =
    IfMaybe (MatchesInput entryType (GetInput def))
            ('Just name)
            (FindEntryHandler entryType rest)

-- | Check if the entry type matches the Input type.
type MatchesInput :: Type -> Maybe Type -> Bool
type family MatchesInput entryType mInput where
  MatchesInput _ 'Nothing = 'False   -- No Input annotation means no match
  MatchesInput t ('Just t) = 'True   -- Exact match
  MatchesInput _ _ = 'False          -- Different types


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a graph starting from a named handler.
--
-- Use this when you want to explicitly specify which handler to start from,
-- bypassing automatic entry handler discovery.
--
-- @
-- result <- runGraphFrom @"compute" handlers inputValue
-- @
runGraphFrom
  :: forall (name :: Symbol) graph entryType targets exitType es handler.
     ( KnownSymbol name
     , HasField name (graph (AsHandler es)) handler
     , CallHandler handler entryType es targets
     , DispatchGoto graph targets es exitType
     )
  => graph (AsHandler es)
  -> entryType
  -> Eff es exitType
runGraphFrom graph input = do
  let handler = getField @name graph
  choice <- callHandler handler input
  dispatchGoto graph choice


-- | Run a graph from EntryNode to Exit.
--
-- Automatically discovers the first handler that accepts the entry type
-- (via the 'Input' annotation), calls it with the input, and dispatches
-- through the graph until Exit is reached.
--
-- @
-- -- Define graph
-- data TestGraph mode = TestGraph
--   { entry   :: mode :- EntryNode Int
--   , compute :: mode :- LogicNode :@ Input Int :@ UsesEffects '[Goto Exit Int]
--   , exit    :: mode :- ExitNode Int
--   }
--
-- -- Run it
-- result <- runGraph handlers 5  -- Returns 6 (if compute does +1)
-- @
runGraph
  :: forall graph entryType targets exitType es entryHandlerName handler.
     ( Generic (graph AsGraph)
     , FindEntryHandler entryType (FieldsWithNamesOf graph) ~ 'Just entryHandlerName
     , KnownSymbol entryHandlerName
     , HasField entryHandlerName (graph (AsHandler es)) handler
     , CallHandler handler entryType es targets
     , DispatchGoto graph targets es exitType
     )
  => graph (AsHandler es)
  -> entryType
  -> Eff es exitType
runGraph = runGraphFrom @entryHandlerName


-- | Run a graph with an explicitly provided entry handler.
--
-- This avoids the 'HasField' constraint that 'runGraph' and 'runGraphFrom'
-- require. Use this when GHC can't derive HasField for your graph record
-- (common when field types involve type families).
--
-- @
-- -- Instead of:
-- result <- runGraph handlers input  -- Requires HasField
--
-- -- Use:
-- result <- runGraphWith handlers.v3Scaffold handlers input  -- No HasField needed
-- @
runGraphWith
  :: forall graph entryType targets exitType es handler.
     ( CallHandler handler entryType es targets
     , DispatchGoto graph targets es exitType
     )
  => handler              -- ^ EntryNode handler (e.g., @handlers.v3Scaffold@)
  -> graph (AsHandler es) -- ^ Full handler record (for dispatch to other nodes)
  -> entryType            -- ^ Input to the entry handler
  -> Eff es exitType
runGraphWith entryHandler graph input = do
  choice <- callHandler entryHandler input
  dispatchGoto graph choice


-- ════════════════════════════════════════════════════════════════════════════
-- LLM HANDLER EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Execute an LLMBoth handler, returning a GotoChoice.
--
-- This function:
-- 1. Calls the before-handler to build template context
-- 2. Renders the system template (if provided) and user template
-- 3. Calls the LLM effect with rendered prompts and JSON schema
-- 4. Parses the structured output
-- 5. Calls the after-handler to determine the next transition
--
-- = Type Parameters
--
-- * @needs@ - The input type from Needs annotation
-- * @schema@ - The LLM output schema type
-- * @targets@ - The transition targets from UsesEffects
-- * @es@ - The effect stack (must include LLM)
-- * @tpl@ - The template context type
--
-- = Example
--
-- @
-- result <- executeLLMHandler
--   Nothing                          -- no system template
--   userTemplate                     -- user prompt template
--   (\\input -> pure MyContext {...}) -- build context
--   (\\output -> pure (gotoExit output)) -- route based on output
--   inputValue
-- @
executeLLMHandler
  :: forall needs schema targets es tpl.
     ( Member LLM es
     , Member NodeMeta es
     , StructuredOutput schema
     , ValidStructuredOutput schema
     , GingerContext tpl
     , ConvertTransitionHint targets
     )
  => Maybe (TypedTemplate tpl SourcePos)  -- ^ Optional system prompt template
  -> TypedTemplate tpl SourcePos         -- ^ User prompt template
  -> (needs -> Eff es tpl)               -- ^ Before function: build template context
  -> (schema -> Eff es (GotoChoice targets)) -- ^ After function: handle LLM output
  -> needs
  -> Eff es (GotoChoice targets)
executeLLMHandler mSystemTpl userTpl beforeFn afterFn input = do
  -- Build context from before-handler
  ctx <- beforeFn input
  -- Render templates
  let systemPrompt = maybe "" (runTypedTemplate ctx) mSystemTpl
      userPrompt = runTypedTemplate ctx userTpl
      schemaVal = schemaToValue (structuredSchema @schema)
  -- Call LLM with rendered prompts and handle tool-initiated transitions
  turnResult <- runTurn @schema systemPrompt userPrompt schemaVal []
  case turnResult of
    TurnBroken reason -> error $ "LLM turn broken: " <> T.unpack reason
    TurnTransitionHint targetName payload ->
      case convertTransitionHint @targets targetName payload of
        Just choice -> pure choice
        Nothing -> error $ "Tool transition to unknown target or wrong type: " <> T.unpack targetName
    TurnCompleted (TurnParsed (TurnResult {trOutput})) -> afterFn trOutput
    TurnCompleted (TurnParseFailed {tpfError}) -> error $ "Parse failed: " <> tpfError


-- ════════════════════════════════════════════════════════════════════════════
-- CLAUDE CODE HANDLER EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Execute a ClaudeCodeLLMHandler, returning a GotoChoice.
--
-- Similar to 'executeLLMHandler', but uses the Session effect to spawn
-- a dockerized Claude Code session via mantle.
--
-- This function:
-- 1. Calls the before-handler to build template context AND session strategy
-- 2. Renders the user template (system template appended to prompt)
-- 3. Detects if schema is a sum type → generates decision tools
-- 4. Executes the appropriate Session operation (start/continue/fork)
-- 5. Parses output from tool calls (sum types) or structured output (others)
-- 6. Implements nag logic if sum type but no tool call (max 5 retries)
-- 7. Calls the after-handler with output AND session ID for routing
--
-- = Type Parameters
--
-- * @model@ - The ModelChoice from ClaudeCode annotation (type-level)
-- * @needs@ - The input type from Input annotation
-- * @schema@ - The LLM output schema type
-- * @targets@ - The transition targets from UsesEffects
-- * @es@ - The effect stack (must include Session)
-- * @tpl@ - The template context type
--
-- = Session Management
--
-- The before handler returns a 'SessionOperation' alongside the template context:
--
-- * 'StartFresh slug' - Create a new session with the given slug
-- * 'ContinueFrom sid' - Resume an existing session (preserves conversation history)
-- * 'ForkFrom parentSid childSlug' - Create a read-only fork from parent session
--
-- The after handler receives both the parsed output and the session ID that was
-- used/created, enabling downstream handlers to register sessions for later reuse.
--
-- = Sum Type Handling (Decision Tools)
--
-- For sum types with data (detected via 'ClaudeCodeSchema'), this function:
--
-- 1. Generates MCP decision tools where each tool = one constructor
-- 2. Passes tools to the session; Claude Code calls one to "select" a branch
-- 3. Parses the tool call back to the Haskell sum type
-- 4. If Claude doesn't call a tool, nags with a retry prompt (max 5 retries)
--
-- This works around Anthropic's lack of oneOf support in structured output.
executeClaudeCodeHandler
  :: forall model needs schema targets es tpl.
     ( Member Session es
     , ClaudeCodeSchema schema
     , GingerContext tpl
     , SingModelChoice model
     , ConvertTransitionHint targets
     )
  => Maybe (TypedTemplate tpl SourcePos)                    -- ^ Optional system prompt template
  -> TypedTemplate tpl SourcePos                            -- ^ User prompt template (required)
  -> (needs -> Eff es (tpl, SessionOperation))              -- ^ Before handler: builds context AND session strategy
  -> ((ClaudeCodeResult schema, SessionId) -> Eff es (GotoChoice targets))  -- ^ After handler: routes with output AND session ID
  -> needs                                                  -- ^ Input from Input annotation
  -> Eff es (GotoChoice targets)
executeClaudeCodeHandler mSystemTpl userTpl beforeFn afterFn input = do
  let model = singModelChoice @model

  -- Build context and get session operation from before-handler
  (ctx, sessionOp) <- beforeFn input

  -- Render templates
  let systemPrompt :: Text
      systemPrompt = maybe "" (runTypedTemplate ctx) mSystemTpl
      userPrompt :: Text
      userPrompt = runTypedTemplate ctx userTpl
      -- Combine system and user prompts for Claude Code
      -- (Claude Code doesn't have separate system prompt, so prepend it)
      fullPrompt :: Text
      fullPrompt = if systemPrompt == ""
                   then userPrompt
                   else systemPrompt <> "\n\n" <> userPrompt

  -- Get decision tools if schema is a sum type with data
  let mDecisionTools :: Maybe [DecisionTool]
      mDecisionTools = ccDecisionTools @schema

      -- Convert to JSON Value for session API
      mToolsJson :: Maybe Value
      mToolsJson = toJSON <$> mDecisionTools

      -- Schema is only used when NOT using decision tools
      -- (Sum types use tools instead of structured output schema)
      schemaVal :: Maybe Value
      schemaVal = case mDecisionTools of
        Just _ -> Nothing  -- Don't pass schema when using decision tools
        Nothing -> Just $ schemaToValue (structuredSchema @schema)

  -- Execute session and parse result with nag logic for sum types
  executeWithNag @schema fullPrompt schemaVal mToolsJson sessionOp model afterFn 0
  where
    -- Maximum nag retries when Claude doesn't call a decision tool or parse fails
    -- NOTE: Set to 1 for debugging while decision tool termination is being fixed
    maxNagRetries :: Int
    maxNagRetries = 1

    -- Nag prompt when Claude doesn't call a decision tool
    decisionToolNagPrompt :: Text
    decisionToolNagPrompt = "You must call one of the decision:: tools to indicate your choice. " <>
                            "Please review the available tools and call the appropriate one."

    -- Nag prompt for parse failures
    parseErrorNagPrompt :: Text -> Text
    parseErrorNagPrompt diagText =
      "Your output didn't match the expected JSON schema. Error:\n" <>
      diagText <> "\n\n" <>
      "Please try again with valid JSON matching the schema."

    -- Execute session, parsing output and handling nag retries
    executeWithNag
      :: forall s.
         ( ClaudeCodeSchema s )
      => Text              -- ^ Prompt
      -> Maybe Value       -- ^ JSON schema (Nothing for sum types)
      -> Maybe Value       -- ^ Decision tools JSON (Just for sum types)
      -> SessionOperation  -- ^ Session operation
      -> ModelChoice       -- ^ Model to use
      -> ((ClaudeCodeResult s, SessionId) -> Eff es (GotoChoice targets))
      -> Int               -- ^ Current retry count
      -> Eff es (GotoChoice targets)
    executeWithNag prompt schema tools sessionOp_ model_ afterFn_ retryCount = do
      -- Dispatch to appropriate Session operation
      result <- case sessionOp_ of
        StartFresh slug ->
          startSession slug prompt model_ schema tools

        ContinueFrom sid worktree branch ->
          -- Guard against empty session ID (indicates prior mantle failure)
          if T.null sid.unSessionId
          then error "ClaudeCode: Cannot continue session - no valid session ID (prior mantle failure)"
          else continueSession sid worktree branch model_ prompt schema tools

        ForkFrom parentSid parentWorktree parentBranch childSlug ->
          forkSession parentSid parentWorktree parentBranch model_ childSlug prompt schema tools

      -- Check for mantle/process-level failure BEFORE parsing output
      -- This distinguishes "mantle crashed" from "Claude didn't call tool"
      when result.soIsError $ do
        let errDetails = fromMaybe "unknown error" result.soError
            stderrInfo = maybe "" (\s -> "\n\nStderr:\n" <> T.unpack s) result.soStderrOutput
        error $ "ClaudeCode session failed (mantle/process error): " <> T.unpack errDetails
          <> " [exit code: " <> show result.soExitCode <> "]"
          <> stderrInfo

      -- Extract cc_session_id (required for resume/fork) - fail early if missing
      ccSessionId <- case result.soCcSessionId of
        Just sid -> pure (SessionId sid)
        Nothing -> do
          let errDetails = fromMaybe "no cc_session_id in output" result.soError
              stderrInfo = maybe "" (\s -> "\n\nStderr:\n" <> T.unpack s) result.soStderrOutput
          error $ "ClaudeCode session failed to return cc_session_id (required for resume/fork). "
            <> "Error: " <> T.unpack errDetails
            <> " [exit code: " <> show result.soExitCode <> "]"
            <> stderrInfo

      -- Parse result based on whether we're using decision tools
      case tools of
        Just _ -> do
          -- Sum type: parse from tool calls
          case result.soToolCalls of
            Just (tc:rest) ->
              -- Got at least one tool call, take the first
              -- (Claude sometimes calls multiple despite "STOP NOW"; ignore subsequent)
              trace ("[DECISION] Taking first of " <> show (1 + length rest) <> " decision tool calls") $
              case ccParseToolCall @s (convertToolCall tc) of
                Right output -> afterFn_ (ClaudeCodeResult output ccSessionId result.soWorktree result.soBranch, ccSessionId)
                Left err -> error $ "ClaudeCode decision tool parse error: " <> err

            _ ->
              -- No tool call - nag and retry
              if retryCount >= maxNagRetries
              then error $ "ClaudeCode: Claude completed but failed to call a decision tool after "
                        <> show maxNagRetries <> " retries. "
                        <> "Session output: " <> maybe "(no text)" T.unpack result.soResultText
              else do
                -- Continue the same session with nag prompt
                executeWithNag @s decisionToolNagPrompt schema tools
                  (ContinueFrom ccSessionId result.soWorktree result.soBranch) model_ afterFn_ (retryCount + 1)

        Nothing ->
          -- Regular type: parse from structured output
          case result.soStructuredOutput of
            Nothing -> error $ "ClaudeCode session returned no structured output"
              <> maybe "" (\e -> ": " <> T.unpack e) result.soError
            Just outputVal ->
              case ccParseStructured @s outputVal of
                Right output -> afterFn_ (ClaudeCodeResult output ccSessionId result.soWorktree result.soBranch, ccSessionId)
                Left diag ->
                  -- Parse failed - nag and retry
                  if retryCount >= maxNagRetries
                  then error $ "ClaudeCode output parse error after "
                            <> show maxNagRetries <> " retries: "
                            <> T.unpack (formatDiagnostic diag)
                  else do
                    -- Continue the same session with parse error nag prompt
                    let nagText = parseErrorNagPrompt (formatDiagnostic diag)
                    executeWithNag @s nagText schema tools
                      (ContinueFrom ccSessionId result.soWorktree result.soBranch) model_ afterFn_ (retryCount + 1)

    -- Convert Session.ToolCall to DecisionTools.ToolCall format
    convertToolCall :: ToolCall -> DT.ToolCall
    convertToolCall tc = DT.ToolCall
      { DT.tcName = tc.tcName
      , DT.tcInput = tc.tcInput
      }

-- | Execute a GeminiLLMHandler, returning a GotoChoice.
--
-- Spawns Gemini CLI subprocess via the Gemini effect.
executeGeminiHandler
  :: forall model needs schema targets es tpl.
     ( Member GeminiOp es
     , Member NodeMeta es
     , Member GraphMeta es
     , StructuredOutput schema
     , ValidStructuredOutput schema
     , GingerContext tpl
     , SingGeminiModel model
     , ConvertTransitionHint targets
     )
  => Maybe (TypedTemplate tpl SourcePos)
  -> TypedTemplate tpl SourcePos
  -> (needs -> Eff es tpl)
  -> (schema -> Eff es (GotoChoice targets))
  -> needs
  -> Eff es (GotoChoice targets)
executeGeminiHandler mSystemTpl userTpl beforeFn afterFn input = do
  -- Build context from before-handler
  ctx <- beforeFn input
  -- Render templates
  let systemPrompt = maybe "" (runTypedTemplate ctx) mSystemTpl
      userPrompt = runTypedTemplate ctx userTpl
      fullPrompt = if systemPrompt == "" then userPrompt else systemPrompt <> "\n\n" <> userPrompt
  
  -- Demote model to term-level for the Gemini effect
  let modelVal = case singGeminiModel @model of
        SFlash -> Flash
        SPro   -> Pro
        SUltra -> Ultra

  -- Run Gemini effect
  GeminiResult{grOutput} <- runGemini modelVal fullPrompt
  
  -- Parse output
  case parseStructured grOutput of
    Right parsed -> afterFn parsed
    Left diag -> error $ "Parse failed: Gemini parse error: " <> T.unpack (formatDiagnostic diag)


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER INVOCATION TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for invoking handlers uniformly.
--
-- This abstracts over the difference between:
-- * Logic node handlers: @payload -> Eff es (GotoChoice targets)@
-- * LLM node handlers: @LLMHandler payload schema targets es tpl@
--
-- By using this typeclass, the main dispatch instance doesn't need to know
-- which kind of handler it's dealing with - it just calls 'callHandler'.
type CallHandler :: Type -> Type -> [Effect] -> [Type] -> Constraint
class CallHandler handler payload es targets | handler -> payload es targets where
  callHandler :: handler -> payload -> Eff es (GotoChoice targets)

-- | Logic node handler: simple function invocation.
instance CallHandler (payload -> Eff es (GotoChoice targets)) payload es targets where
  callHandler = id

-- | LLM node handler: execute via executeLLMHandler.
instance
  ( Member LLM es
  , Member NodeMeta es
  , StructuredOutput schema
  , ValidStructuredOutput schema
  , GingerContext tpl
  , ConvertTransitionHint targets
  ) => CallHandler (LLMHandler payload schema targets es tpl) payload es targets where
  callHandler (LLMHandler mSysTpl userTpl beforeFn afterFn) =
    executeLLMHandler mSysTpl userTpl beforeFn afterFn

-- | ClaudeCode LLM node handler: execute via executeClaudeCodeHandler.
--
-- Dispatches to dockerized Claude Code session via mantle.
-- Model is derived from type parameters, ensuring compile-time
-- validation that the handler matches the ClaudeCode annotation.
--
-- For sum types with data, generates MCP decision tools and parses
-- tool calls. For other types, uses standard structured output.
instance
  ( Member Session es
  , ClaudeCodeSchema schema
  , GingerContext tpl
  , SingModelChoice model
  , ConvertTransitionHint targets
  ) => CallHandler (ClaudeCodeLLMHandler model payload schema targets es tpl) payload es targets where
  callHandler (ClaudeCodeLLMHandler mSysTpl userTpl beforeFn afterFn) =
    executeClaudeCodeHandler @model mSysTpl userTpl beforeFn afterFn

-- | Gemini LLM node handler: execute via executeGeminiHandler.
instance
  ( Member GeminiOp es
  , Member NodeMeta es
  , Member GraphMeta es
  , StructuredOutput schema
  , ValidStructuredOutput schema
  , GingerContext tpl
  , SingGeminiModel model
  , ConvertTransitionHint targets
  ) => CallHandler (GeminiLLMHandler model payload schema targets es tpl) payload es targets where
  callHandler (GeminiLLMHandler mSysTpl userTpl beforeFn afterFn) =
    executeGeminiHandler @model mSysTpl userTpl beforeFn afterFn


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPHNODE EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Execute a GraphNode by running the child graph to completion.
--
-- The child graph's handlers must be provided. The input is passed to
-- the child graph's EntryNode, and execution continues until Exit is reached.
--
-- This function is a thin wrapper around 'runGraph' that documents the
-- GraphNode execution pattern. Use it in parent graph handlers when
-- manually delegating to child graphs.
--
-- @
-- -- In a parent graph handler:
-- childResult <- executeGraphNode childHandlers input
-- pure $ gotoChoice @"nextNode" childResult
-- @
-- @
-- executeGraphNode
--   :: forall childGraph es entryType targets exitType entryHandlerName handler.
--      ( Generic (childGraph AsGraph)
--      , FindEntryHandler entryType (FieldsWithNamesOf childGraph) ~ 'Just entryHandlerName
--      , KnownSymbol entryHandlerName
--      , HasField entryHandlerName (childGraph (AsHandler es)) handler
--      , CallHandler handler entryType es targets
--      , DispatchGoto childGraph targets es exitType
--      )
--   => childGraph (AsHandler es)  -- ^ Child graph handlers
--   -> entryType                   -- ^ Input to child graph's EntryNode
--   -> Eff es exitType             -- ^ Child graph's Exit value
-- executeGraphNode = runGraph


-- ════════════════════════════════════════════════════════════════════════════
-- DISPATCH TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Dispatch on a 'GotoChoice', recursively executing handlers until Exit.
--
-- This typeclass enables fully typed dispatch through a graph. Each instance
-- handles one case of the target list, recursively calling handlers until
-- an Exit target is reached.
--
-- = Type Parameters
--
-- * @graph@ - The graph record type (e.g., @TestGraph@)
-- * @targets@ - The current target list to dispatch on
-- * @es@ - The effect stack available to handlers
-- * @exitType@ - The graph's exit type (what 'runGraph' returns)
--
-- = How It Works
--
-- Given @GotoChoice '[To \"a\" A, To \"b\" B, To Exit R]@:
--
-- 1. Pattern match on @OneOf '[A, B, R]@
-- 2. If @Here payload@: call handler \"a\" with payload :: A
-- 3. If @There (Here payload)@: call handler \"b\" with payload :: B
-- 4. If @There (There (Here result))@: return result :: R
--
-- Each handler returns its own @GotoChoice@, so dispatch recurses through
-- the graph until Exit is reached.
--
-- = Example
--
-- @
-- -- Given a graph with compute handler that exits
-- result <- dispatchGoto testHandlers (gotoChoice \@"compute" 5)
-- -- Internally: calls compute handler, gets GotoChoice '[To Exit Int]
-- -- Pattern matches Here 6, returns 6
-- @
type DispatchGoto :: (Type -> Type) -> [Type] -> [Effect] -> Type -> Constraint
class DispatchGoto graph targets es exitType where
  dispatchGoto :: graph (AsHandler es) -> GotoChoice targets -> Eff es exitType


-- ════════════════════════════════════════════════════════════════════════════
-- CONVERT UNTYPED TRANSITIONS TO TYPED CHOICES
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert untyped transition (Text + Value) to typed GotoChoice by matching
-- against node's UsesEffects targets.
--
-- When a tool returns ToolTransition with an untyped target name and payload,
-- this typeclass recursively matches the target name against the list of valid
-- targets, constructs the correct OneOf position, and type-checks the payload JSON.
--
-- = How It Works
--
-- Given targets @'[To \"nodeA\" Int, To \"nodeB\" Text, To Exit Bool]@:
--
-- 1. Check if target name == "nodeA"
--    - If yes, parse payload as Int, construct @Here (To payload)@
--    - If no, skip to rest
-- 2. Check if target name == "nodeB"
--    - If yes, parse payload as Text, construct @There (Here (To payload))@
--    - If no, skip to rest
-- 3. Check if target name == "Exit"
--    - If yes, parse payload as Bool, construct @There (There (Here (To payload)))@
--    - If no, return Nothing
--
-- The recursive structure ensures we find the right target and type-check the payload.
type ConvertTransitionHint :: [Type] -> Constraint
class ConvertTransitionHint targets where
  convertTransitionHint :: Text -> Value -> Maybe (GotoChoice targets)

-- | Base case: empty target list has no valid transitions.
instance ConvertTransitionHint '[] where
  convertTransitionHint _ _ = Nothing

-- | Recursive case: named target (symbol literal like @"nodeA"@).
--
-- Check if the target name matches the symbol, parse payload if it does,
-- and recurse for the rest of the target list.
instance
  ( KnownSymbol name
  , StructuredOutput payload
  , ConvertTransitionHint rest
  ) => ConvertTransitionHint (To name payload ': rest) where
  convertTransitionHint targetName payloadVal
    | targetName == T.pack (symbolVal (Proxy @name)) =
        case parseStructured payloadVal of
          Right val -> Just $ GotoChoice (Here val)
          Left _ -> Nothing
    | otherwise =
        case convertTransitionHint @rest targetName payloadVal of
          Just (GotoChoice oneOf) -> Just $ GotoChoice (There oneOf)
          Nothing -> Nothing

-- | Exit target case.
--
-- Check if target name is "Exit", parse payload if it is, and recurse for rest.
instance
  ( StructuredOutput payload
  , ConvertTransitionHint rest
  ) => ConvertTransitionHint (To Exit payload ': rest) where
  convertTransitionHint targetName payloadVal
    | targetName == "Exit" =
        case parseStructured payloadVal of
          Right val -> Just $ GotoChoice (Here val)
          Left _ -> Nothing
    | otherwise =
        case convertTransitionHint @rest targetName payloadVal of
          Just (GotoChoice oneOf) -> Just $ GotoChoice (There oneOf)
          Nothing -> Nothing

-- | Self target case (for nodes with self-loops).
--
-- Check if target name is "Self", parse payload if it is, and recurse for rest.
instance
  ( StructuredOutput payload
  , ConvertTransitionHint rest
  ) => ConvertTransitionHint (To Self payload ': rest) where
  convertTransitionHint targetName payloadVal
    | targetName == "Self" =
        case parseStructured payloadVal of
          Right val -> Just $ GotoChoice (Here val)
          Left _ -> Nothing
    | otherwise =
        case convertTransitionHint @rest targetName payloadVal of
          Just (GotoChoice oneOf) -> Just $ GotoChoice (There oneOf)
          Nothing -> Nothing

-- | Arrive target case (for parallel workers depositing results at barrier).
--
-- Check if target name is "arrive", parse payload if it is, and recurse for rest.
-- Used by ForkNode workers to signal completion and deliver their result.
-- The barrier name (Symbol) is encoded in the message for routing, but dispatch
-- just needs to know it's an Arrive target.
instance
  ( StructuredOutput payload
  , ConvertTransitionHint rest
  ) => ConvertTransitionHint (To (Arrive barrierName) payload ': rest) where
  convertTransitionHint targetName payloadVal
    | targetName == "arrive" =
        case parseStructured payloadVal of
          Right val -> Just $ GotoChoice (Here val)
          Left _ -> Nothing
    | otherwise =
        case convertTransitionHint @rest targetName payloadVal of
          Just (GotoChoice oneOf) -> Just $ GotoChoice (There oneOf)
          Nothing -> Nothing


-- ════════════════════════════════════════════════════════════════════════════
-- ERROR CASE: EMPTY TARGET LIST
-- ════════════════════════════════════════════════════════════════════════════

-- | Error case: Empty target list is invalid.
--
-- A GotoChoice must have at least one target. This instance produces a clear
-- type error rather than an opaque "No instance" message.
--
-- Uses 'Unsatisfiable' rather than 'TypeError' because an empty target list
-- is logically impossible - there's no way to construct 'OneOf '[]'.
instance Unsatisfiable
  ( HR
    ':$$: 'Text "  Cannot dispatch: handler has no exit points"
    ':$$: HR
    ':$$: Blank
    ':$$: WhatHappened
    ':$$: Indent "dispatchGoto was called with GotoChoice '[]"
    ':$$: Indent "This type has no constructors - the handler can never return!"
    ':$$: Blank
    ':$$: HowItWorks
    ':$$: Indent "The graph interpreter pattern-matches on GotoChoice to determine"
    ':$$: Indent "which handler to call next. With an empty target list, there's"
    ':$$: Indent "nothing to match on."
    ':$$: Blank
    ':$$: CodeLine "dispatchGoto graph (GotoChoice oneOf)"
    ':$$: CodeLine "                             ^^^^"
    ':$$: CodeLine "                             OneOf '[] has no constructors!"
    ':$$: Blank
    ':$$: Fixes
    ':$$: Bullet "Add at least one target to your UsesEffects annotation:"
    ':$$: CodeLine "  UsesEffects '[Goto Exit ResultType]"
    ':$$: Bullet "Or add transitions to other nodes:"
    ':$$: CodeLine "  UsesEffects '[Goto \"nextNode\" Payload, Goto Exit Result]"
  ) => DispatchGoto graph '[] es exitType where
  -- UNREACHABLE: The Unsatisfiable constraint means this code is never executed.
  -- If a GotoChoice '[] type is constructed, compilation fails with the error message above.
  dispatchGoto = unsatisfiable


-- ════════════════════════════════════════════════════════════════════════════
-- EXIT INSTANCES
-- ════════════════════════════════════════════════════════════════════════════

-- | Base case: Exit is the only target.
--
-- When there's only @To Exit exitType@ in the target list, pattern matching
-- gives us the exit value directly.
instance {-# OVERLAPPING #-} DispatchGoto graph '[To Exit exitType] es exitType where
  dispatchGoto _ (GotoChoice (Here result)) = pure result

-- | Exit is first, but there are more targets.
--
-- Handle the Exit case (return result) or skip to rest of targets.
instance {-# OVERLAPPABLE #-}
  ( DispatchGoto graph rest es exitType
  ) => DispatchGoto graph (To Exit exitType ': rest) es exitType where
  dispatchGoto _ (GotoChoice (Here result)) = pure result
  dispatchGoto graph (GotoChoice (There rest)) =
    dispatchGoto @graph @rest graph (GotoChoice rest)


-- ════════════════════════════════════════════════════════════════════════════
-- ARRIVE INSTANCES (for ForkNode workers)
-- ════════════════════════════════════════════════════════════════════════════

-- | Base case: Arrive is the only target.
--
-- When a parallel worker can only Arrive, it terminates and returns the result.
-- The full Fork/Barrier orchestration layer collects these results at the barrier.
--
-- Note: In standalone dispatch (outside Fork context), Arrive behaves like Exit
-- for that path. The fork orchestration layer interprets this differently.
-- The barrier name (Symbol) is for routing; dispatch just returns the result.
instance {-# OVERLAPPING #-} DispatchGoto graph '[To (Arrive barrierName) resultType] es resultType where
  dispatchGoto _ (GotoChoice (Here result)) = pure result

-- | Arrive is first, but there are more targets.
--
-- Handle the Arrive case (return result) or skip to rest of targets.
-- The barrier name (Symbol) is for routing; dispatch just returns the result.
instance {-# OVERLAPPABLE #-}
  ( DispatchGoto graph rest es exitType
  ) => DispatchGoto graph (To (Arrive barrierName) exitType ': rest) es exitType where
  dispatchGoto _ (GotoChoice (Here result)) = pure result
  dispatchGoto graph (GotoChoice (There rest)) =
    dispatchGoto @graph @rest graph (GotoChoice rest)


-- ════════════════════════════════════════════════════════════════════════════
-- SELF-LOOP INSTANCES (error guidance)
-- ════════════════════════════════════════════════════════════════════════════

-- | Self-loop as only target: use DispatchGotoWithSelf instead.
--
-- Self-loops require tracking the "current" handler to re-invoke.
-- This instance provides a clear error directing users to the correct API.
--
-- Uses 'Unsatisfiable' rather than 'TypeError' because using 'dispatchGoto'
-- with a self-looping handler is API misuse that cannot be fixed by adding
-- annotations - it requires using a different function entirely.
instance Unsatisfiable SelfLoopDispatchError
  => DispatchGoto graph '[To Self payload] es exitType where
  -- UNREACHABLE: The Unsatisfiable constraint means this code is never executed.
  -- If gotoSelf is used with dispatchGoto (instead of dispatchGotoWithSelf),
  -- compilation fails with the error directing to the correct API.
  dispatchGoto = unsatisfiable

-- | Self first with more targets: use DispatchGotoWithSelf instead.
--
-- Uses 'Unsatisfiable' rather than 'TypeError' because using 'dispatchGoto'
-- with a self-looping handler is API misuse that cannot be fixed by adding
-- annotations - it requires using a different function entirely.
instance {-# OVERLAPPABLE #-} Unsatisfiable SelfLoopDispatchError
  => DispatchGoto graph (To Self payload ': rest) es exitType where
  -- UNREACHABLE: The Unsatisfiable constraint means this code is never executed.
  -- If gotoSelf is used with dispatchGoto (instead of dispatchGotoWithSelf),
  -- compilation fails with the error directing to the correct API.
  dispatchGoto = unsatisfiable


-- ════════════════════════════════════════════════════════════════════════════
-- NAMED NODE DISPATCH (with automatic ForkNode detection)
-- ════════════════════════════════════════════════════════════════════════════

-- | Helper class for dispatching named nodes, parameterized by IsForkNode result.
--
-- GHC distinguishes instances by their heads, not constraints. Since both regular
-- nodes and ForkNodes have the same DispatchGoto head, we use this helper class
-- with a type-level Bool to differentiate them.
--
-- The Bool parameter comes from @IsForkNode (GetNodeDef name graph)@:
-- - 'False → regular dispatch (call handler, recurse on its targets)
-- - 'True  → fork dispatch (spawn workers, collect results, route to barrier)
type DispatchNamedNode :: (Type -> Type) -> Symbol -> Type -> [Effect] -> Type -> Bool -> Constraint
class DispatchNamedNode graph name payload es exitType (isFork :: Bool) where
  dispatchNamedNode
    :: graph (AsHandler es)
    -> payload
    -> Eff es exitType

-- | Regular node dispatch: call handler and recurse on its targets.
--
-- Wraps handler invocation with 'withNodeMeta' to provide node context for
-- teaching/observability. The node name is extracted from the type-level
-- Symbol, and graph name is read from the 'GraphMeta' effect (set by 'runGraph').
instance
  ( KnownSymbol name
  , Member NodeMeta es
  , Member GraphMeta es
  , HasField name (graph (AsHandler es)) handler
  , CallHandler handler payload es handlerTargets
  , DispatchGoto graph handlerTargets es exitType
  ) => DispatchNamedNode graph name payload es exitType 'False where
  dispatchNamedNode graph payload = do
    -- Get graph name from GraphMeta effect
    GraphMetadata graphName <- getGraphMeta
    -- Build node metadata from type-level name and graph context
    let nodeName = T.pack $ symbolVal (Proxy @name)
        meta = NodeMetadata
          { nmNodeName = nodeName
          , nmGraphName = graphName
          }
    let handler = getField @name graph
    -- Wrap handler call with NodeMeta context (interpose, not interpret)
    nextChoice <- withNodeMeta meta (callHandler handler payload)
    dispatchGoto graph nextChoice

-- | ForkNode dispatch: spawn workers, collect results, route to barrier.
--
-- When a named node is a ForkNode:
-- 1. Call fork handler with payload → SpawnPayloads
-- 2. Use SpawnWorkers to dispatch workers and collect results
-- 3. Look up barrier handler (from Barrier annotation)
-- 4. Call barrier handler with collected results
-- 5. Continue dispatch from barrier's GotoChoice
--
-- Both fork and barrier handlers are wrapped with 'withNodeMeta' to provide
-- node context for teaching/observability.
instance
  ( KnownSymbol name
  , Member NodeMeta es
  , Member GraphMeta es
  , Generic (graph AsGraph)
  , HasField name (graph (AsHandler es)) (payload -> Eff es (SpawnPayloads spawnTargets))
  , GetSpawnTargets (GetNodeDef name graph) ~ spawnTargets
  , GetBarrierTarget (GetNodeDef name graph) ~ 'Just barrierName
  , KnownSymbol barrierName
  , GetAwaits (GetNodeDef barrierName graph) ~ resultTypes
  , SpawnWorkers graph spawnTargets resultTypes es
  , HasField barrierName (graph (AsHandler es)) barrierHandler
  , CallHandler barrierHandler (AwaitsHList resultTypes) es barrierTargets
  , DispatchGoto graph barrierTargets es exitType
  ) => DispatchNamedNode graph name payload es exitType 'True where
  dispatchNamedNode graph payload = do
    -- Get graph name from GraphMeta effect
    GraphMetadata graphName <- getGraphMeta
    -- Build metadata for fork node
    let forkNodeName = T.pack $ symbolVal (Proxy @name)
        forkMeta = NodeMetadata
          { nmNodeName = forkNodeName
          , nmGraphName = graphName
          }
    let forkHandler = getField @name graph
    -- Wrap fork handler with NodeMeta context (interpose, not interpret)
    spawnPayloads <- withNodeMeta forkMeta (forkHandler payload)
    results <- spawnWorkers @graph @spawnTargets @resultTypes graph spawnPayloads
    -- Build metadata for barrier node
    let barrierNodeName = T.pack $ symbolVal (Proxy @barrierName)
        barrierMeta = NodeMetadata
          { nmNodeName = barrierNodeName
          , nmGraphName = graphName
          }
    let barrierHandler = getField @barrierName graph
    -- Wrap barrier handler with NodeMeta context (interpose, not interpret)
    barrierChoice <- withNodeMeta barrierMeta (callHandler barrierHandler results)
    dispatchGoto graph barrierChoice


-- ════════════════════════════════════════════════════════════════════════════
-- NAMED NODE INSTANCES (delegate to DispatchNamedNode)
-- ════════════════════════════════════════════════════════════════════════════

-- | Single named node target: dispatch via DispatchNamedNode helper.
--
-- When there's only one target @To (name :: Symbol) payload@ and no rest:
--
-- 1. Compute @IsForkNode (GetNodeDef name graph)@ to get type-level Bool
-- 2. Delegate to @DispatchNamedNode@ which has different instances for 'True/'False
--
-- This instance allows intermediate handlers to return single-target GotoChoice
-- without requiring Exit or multiple targets in the type.
--
-- Example:
--
-- @
-- fetchHandler :: Input -> Eff es (GotoChoice '[To "process" Data])
-- fetchHandler input = do
--   data <- fetchData input
--   pure $ gotoChoice \@"process" data
-- @
instance {-# OVERLAPPING #-}
  ( Generic (graph AsGraph)
  , DispatchNamedNode graph name payload es exitType (IsForkNode (GetNodeDef name graph))
  ) => DispatchGoto graph '[To (name :: Symbol) payload] es exitType where

  dispatchGoto graph (GotoChoice (Here payload)) =
    dispatchNamedNode @graph @name @payload @es @exitType @(IsForkNode (GetNodeDef name graph)) graph payload


-- | Named node target with additional targets: dispatch via DispatchNamedNode helper.
--
-- When the first target is @To (name :: Symbol) payload@:
--
-- 1. Compute @IsForkNode (GetNodeDef name graph)@ to get type-level Bool
-- 2. Delegate to @DispatchNamedNode@ which handles both regular and ForkNode dispatch
--
-- The @handler@ type is inferred from the graph record, and 'DispatchNamedNode'
-- determines how to invoke it based on whether it's a ForkNode or regular node.
instance {-# OVERLAPPABLE #-}
  ( Generic (graph AsGraph)
  , DispatchNamedNode graph name payload es exitType (IsForkNode (GetNodeDef name graph))
  , DispatchGoto graph rest es exitType
  ) => DispatchGoto graph (To (name :: Symbol) payload ': rest) es exitType where

  dispatchGoto graph (GotoChoice (Here payload)) =
    dispatchNamedNode @graph @name @payload @es @exitType @(IsForkNode (GetNodeDef name graph)) graph payload

  dispatchGoto graph (GotoChoice (There rest)) =
    dispatchGoto @graph @rest graph (GotoChoice rest)


-- ════════════════════════════════════════════════════════════════════════════
-- SELF-LOOP DISPATCH
-- ════════════════════════════════════════════════════════════════════════════

-- | Dispatch with an explicit self-handler for nodes that use 'Goto Self'.
--
-- When a node can transition back to itself via 'Goto Self', the standard
-- 'dispatchGoto' doesn't know which handler to re-invoke. This typeclass
-- solves this by taking the self-handler as an explicit parameter.
--
-- The key difference from 'DispatchGoto' is that the self-handler is threaded
-- through all recursive calls, so when 'Goto Self' is encountered, the handler
-- can be invoked with the new payload.
--
-- = Usage
--
-- For nodes with self-loops:
--
-- @
-- -- Define handler that may loop back to itself
-- loopHandler :: Int -> Eff es (GotoChoice '[To Self Int, To Exit Int])
-- loopHandler n
--   | n >= 10   = pure $ gotoExit n
--   | otherwise = pure $ gotoSelf (n + 1)
--
-- -- Run with explicit self-handler
-- initialChoice <- loopHandler 0
-- result <- dispatchGotoWithSelf loopHandler graph initialChoice
-- @
--
-- Note: The @allTargets@ parameter is the full target list that the self-handler
-- returns. This ensures type consistency when recursing back via Self.
type DispatchGotoWithSelf :: (Type -> Type) -> Type -> [Type] -> [Type] -> [Effect] -> Type -> Constraint
class DispatchGotoWithSelf graph selfPayload allTargets targets es exitType where
  dispatchGotoWithSelf
    :: (selfPayload -> Eff es (GotoChoice allTargets))  -- ^ Self-handler (returns full target list)
    -> graph (AsHandler es)                              -- ^ Graph handlers
    -> GotoChoice targets                                -- ^ Current choice (may be subset)
    -> Eff es exitType

-- | Base case: Exit is the only target.
instance DispatchGotoWithSelf graph selfPayload allTargets '[To Exit exitType] es exitType where
  dispatchGotoWithSelf _ _ (GotoChoice (Here result)) = pure result

-- | Self is first target: call self-handler and recurse with full target list.
instance
  ( DispatchGotoWithSelf graph selfPayload allTargets allTargets es exitType
  , DispatchGotoWithSelf graph selfPayload allTargets rest es exitType
  ) => DispatchGotoWithSelf graph selfPayload allTargets (To Self selfPayload ': rest) es exitType where
  dispatchGotoWithSelf selfHandler graph (GotoChoice (Here payload)) = do
    -- Call self-handler, which returns GotoChoice allTargets
    nextChoice <- selfHandler payload
    -- Recurse with full target list
    dispatchGotoWithSelf @graph @selfPayload @allTargets @allTargets selfHandler graph nextChoice
  dispatchGotoWithSelf selfHandler graph (GotoChoice (There rest)) =
    dispatchGotoWithSelf @graph @selfPayload @allTargets @rest selfHandler graph (GotoChoice rest)

-- | Exit in current position: handle exit or skip.
instance
  ( DispatchGotoWithSelf graph selfPayload allTargets rest es exitType
  ) => DispatchGotoWithSelf graph selfPayload allTargets (To Exit exitType ': rest) es exitType where
  dispatchGotoWithSelf _ _ (GotoChoice (Here result)) = pure result
  dispatchGotoWithSelf selfHandler graph (GotoChoice (There rest)) =
    dispatchGotoWithSelf @graph @selfPayload @allTargets @rest selfHandler graph (GotoChoice rest)

-- | Arrive in current position: return result or skip (like Exit for parallel workers).
-- The barrier name (Symbol) is for routing; dispatch just returns the result.
instance
  ( DispatchGotoWithSelf graph selfPayload allTargets rest es exitType
  ) => DispatchGotoWithSelf graph selfPayload allTargets (To (Arrive barrierName) exitType ': rest) es exitType where
  dispatchGotoWithSelf _ _ (GotoChoice (Here result)) = pure result
  dispatchGotoWithSelf selfHandler graph (GotoChoice (There rest)) =
    dispatchGotoWithSelf @graph @selfPayload @allTargets @rest selfHandler graph (GotoChoice rest)

-- | Named node target: call handler and recurse.
--
-- Note: When a named handler returns a GotoChoice, we dispatch on it with the
-- handler's target list, not allTargets. The self-handler is still threaded
-- through for nested Self transitions.
instance
  ( KnownSymbol name
  , HasField name (graph (AsHandler es)) (payload -> Eff es (GotoChoice handlerTargets))
  , DispatchGotoWithSelf graph selfPayload allTargets handlerTargets es exitType
  , DispatchGotoWithSelf graph selfPayload allTargets rest es exitType
  ) => DispatchGotoWithSelf graph selfPayload allTargets (To (name :: Symbol) payload ': rest) es exitType where
  dispatchGotoWithSelf selfHandler graph (GotoChoice (Here payload)) = do
    let handler = getField @name graph
    nextChoice <- handler payload
    dispatchGotoWithSelf @graph @selfPayload @allTargets @handlerTargets selfHandler graph nextChoice
  dispatchGotoWithSelf selfHandler graph (GotoChoice (There rest)) =
    dispatchGotoWithSelf @graph @selfPayload @allTargets @rest selfHandler graph (GotoChoice rest)


-- ════════════════════════════════════════════════════════════════════════════
-- FORK/BARRIER ORCHESTRATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Spawn workers for each target in a ForkNode's spawn list.
--
-- This typeclass recursively processes the spawn targets and expected result
-- types together, dispatching a worker for each target and collecting results.
--
-- = Type Parameters
--
-- * @graph@ - The graph record type
-- * @targets@ - Spawn targets list (e.g., @'[To \"w1\" TaskA, To \"w2\" TaskB]@)
-- * @resultTypes@ - Expected result types (from BarrierNode's Awaits annotation)
-- * @es@ - The effect stack
--
-- = Usage
--
-- @
-- -- Given ForkNode with Spawn '[To "w1" Task, To "w2" Task]
-- -- and BarrierNode with Awaits '[ResultA, ResultB]
-- results <- spawnWorkers \@graph \@spawnTargets \@awaitsTypes graph payloads
-- @
--
-- Note: Current implementation is sequential. Parallel execution with 'async'
-- requires lifting to IO, which needs effect interpreter access. The sequential
-- version validates the type infrastructure; parallelism can be added by
-- interpreting workers to IO and using concurrently/async.
type SpawnWorkers :: (Type -> Type) -> [Type] -> [Type] -> [Effect] -> Constraint
class SpawnWorkers graph targets resultTypes es where
  spawnWorkers
    :: graph (AsHandler es)
    -> SpawnPayloads targets
    -> Eff es (AwaitsHList resultTypes)

-- | Base case: no more targets, no more results.
instance SpawnWorkers graph '[] '[] es where
  spawnWorkers _ HNil = pure HNil

-- | Recursive case: dispatch to named node, collect result, continue.
--
-- For each spawn target, we:
-- 1. Get the handler from the graph by name
-- 2. Call the handler with the payload (via CallHandler)
-- 3. Dispatch the resulting GotoChoice until we hit Arrive
-- 4. Collect the result and continue with remaining targets
--
-- Note: The HList pattern match gives us (payload, rest) where the rest
-- has type HList (SpawnPayloadsInner restTargets). We need the recursive
-- call to accept this type, which works because:
--   SpawnPayloads restTargets ~ HList (SpawnPayloadsInner restTargets)
-- for non-empty restTargets by definition of SpawnPayloads.
instance
  ( KnownSymbol name
  , HasField name (graph (AsHandler es)) handler
  , CallHandler handler payload es workerTargets
  , DispatchGoto graph workerTargets es result
  , SpawnWorkersInner graph restTargets restResults es
  ) => SpawnWorkers graph (To name payload ': restTargets) (result ': restResults) es where
  spawnWorkers graph (payload ::: rest) = do
    let handler = getField @name graph
    firstChoice <- callHandler handler payload
    thisResult <- dispatchGoto graph firstChoice
    restResults <- spawnWorkersInner @graph @restTargets @restResults graph rest
    pure (thisResult ::: restResults)


-- | Inner spawn workers that works directly with HList (SpawnPayloadsInner targets).
--
-- This helper avoids the type family reduction issue where GHC can't see that
-- SpawnPayloads restTargets ~ HList (SpawnPayloadsInner restTargets).
type SpawnWorkersInner :: (Type -> Type) -> [Type] -> [Type] -> [Effect] -> Constraint
class SpawnWorkersInner graph targets resultTypes es where
  spawnWorkersInner
    :: graph (AsHandler es)
    -> HList (SpawnPayloadsInner targets)
    -> Eff es (HList resultTypes)

-- | Base case: empty targets list.
instance SpawnWorkersInner graph '[] '[] es where
  spawnWorkersInner _ HNil = pure HNil

-- | Recursive case for inner spawning.
instance
  ( KnownSymbol name
  , HasField name (graph (AsHandler es)) handler
  , CallHandler handler payload es workerTargets
  , DispatchGoto graph workerTargets es result
  , SpawnWorkersInner graph restTargets restResults es
  ) => SpawnWorkersInner graph (To name payload ': restTargets) (result ': restResults) es where
  spawnWorkersInner graph (payload ::: rest) = do
    let handler = getField @name graph
    firstChoice <- callHandler handler payload
    thisResult <- dispatchGoto graph firstChoice
    restResults <- spawnWorkersInner @graph @restTargets @restResults graph rest
    pure (thisResult ::: restResults)


-- Note: For parallel execution with 'async', we would need to:
-- 1. Add 'async' to build-depends
-- 2. Create a 'ParallelSpawnWorkers' variant that takes an interpreter
-- 3. Use a typed async pool for heterogeneous result collection
--
-- The current sequential implementation validates the type infrastructure.
-- Parallelism can be added incrementally when effect interpreter access is
-- available (e.g., by passing a `forall a. Eff es a -> IO a` function).
