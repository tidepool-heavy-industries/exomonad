{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Tidying Agent as a V2 Graph
--
-- This module defines the tidying agent using the typed graph DSL.
-- The graph replaces the manual OODA loop in Tidying.Loop with
-- automatic typed dispatch.
--
-- = Graph Structure
--
-- @
-- Entry(UserInput)
--   → analyzePhotos (LLM vision)
--   → extract (LLM)
--   → route (Logic - pure routing)
--     → act (LLM) → Exit(Response)
--     → Exit(Response)  [for IntentStop]
-- @
module Tidying.Graph
  ( -- * Graph Definition
    TidyingGraph(..)

    -- * Payload types (for threading data between nodes)
  , PhotoResult(..)
  , ExtractResult(..)
  , ActInput(..)
  ) where

import Data.Proxy (Proxy)
import GHC.Generics (Generic)

import Tidepool.Graph.Goto (Goto, GotoChoice, To)
import Tidepool.Graph.Generic (type (:-), AsHandler)
import Tidepool.Graph.Generic.Core (Entry, Exit, LogicNode)
import Tidepool.Graph.Types (type (:@), Needs, UsesEffects)
import qualified Tidepool.Graph.Types as G (Exit)
import Tidepool.Effect (State)

import Tidying.State (UserInput, SessionState)
import Tidying.Action (Action)
import Tidying.Context (PhotoAnalysis, TidyingContext)
import Tidying.Output (Extract)
import Tidying.Loop (Response)


-- ════════════════════════════════════════════════════════════════════════════
-- PAYLOAD TYPES
--
-- These types carry data between graph nodes. Each handler receives its
-- Needs inputs and produces a GotoChoice with the payload for the next node.
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of photo analysis node
--
-- Carries both the original input and the analysis result so the next
-- node has access to both.
data PhotoResult = PhotoResult
  { prInput    :: UserInput
  , prAnalysis :: Maybe PhotoAnalysis
  } deriving (Show, Eq, Generic)

-- | Result of extraction node
--
-- Carries the extraction result plus photo analysis for routing decisions.
data ExtractResult = ExtractResult
  { erExtract  :: Extract
  , erAnalysis :: Maybe PhotoAnalysis
  , erInput    :: UserInput
  } deriving (Show, Eq, Generic)

-- | Input to the act node
--
-- Contains everything needed to generate a response.
data ActInput = ActInput
  { aiContext :: TidyingContext
  , aiAction  :: Action
  } deriving (Show, Eq, Generic)


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Tidying agent graph
--
-- The graph follows OODA (Observe-Orient-Decide-Act):
--
-- 1. @analyzePhotos@ (OBSERVE): Vision LLM analyzes photos
-- 2. @extract@ (ORIENT): LLM extracts intent/item/choice from input
-- 3. @route@ (DECIDE): Pure logic routes to appropriate action
-- 4. @act@ (ACT): LLM generates natural language response
--
-- The @route@ node is a LogicNode (pure Haskell) that:
-- - Updates SessionState based on Extract
-- - Decides the Action and next Phase
-- - Either routes to @act@ for LLM response generation
-- - Or directly exits (for IntentStop with Summary)
--
-- = Effect Stack
--
-- Handlers run in @Eff es@ where @es@ includes:
-- - @State SessionState@ for session state
-- - @LLM@ for LLM calls (via the graph executor)
-- - @Emit TidyingEvent@ for GUI events
-- - @Log@ for debugging
-- - @Time@ for timestamps
data TidyingGraph mode = TidyingGraph
  { tgEntry :: mode :- Entry UserInput
    -- ^ Graph entry point: receives user input (photos + text)

  , tgAnalyzePhotos :: mode :- LogicNode
                            :@ Needs '[UserInput]
                            :@ UsesEffects '[Goto "tgExtract" PhotoResult]
    -- ^ Analyze photos via vision LLM
    --
    -- Input: UserInput (may contain photos)
    -- Output: PhotoResult with Maybe PhotoAnalysis
    --
    -- Note: This is a LogicNode because it needs to handle the LLM call
    -- internally with vision content blocks. The graph DSL's LLMNode
    -- doesn't support vision content directly yet.

  , tgExtract :: mode :- LogicNode
                      :@ Needs '[PhotoResult]
                      :@ UsesEffects '[Goto "tgRoute" ExtractResult]
    -- ^ Extract structured info from user input via LLM
    --
    -- Input: PhotoResult (input + photo analysis)
    -- Output: ExtractResult with Extract (intent, item, choice, etc.)
    --
    -- Note: LogicNode because extraction needs custom prompt building
    -- with photo analysis context.

  , tgRoute :: mode :- LogicNode
                    :@ Needs '[ExtractResult]
                    :@ UsesEffects '[ Goto "tgAct" ActInput
                                    , Goto G.Exit Response
                                    ]
    -- ^ Route based on extraction (pure logic)
    --
    -- Input: ExtractResult
    -- Output: Either ActInput (for LLM response) or Response (direct exit)
    --
    -- This node:
    -- 1. Applies state transitions (updates SessionState)
    -- 2. Calls decideFromExtract to get (Action, Phase)
    -- 3. Builds TidyingContext from updated state
    -- 4. For IntentStop: exits directly with Summary response
    -- 5. Otherwise: routes to tgAct with (Context, Action)

  , tgAct :: mode :- LogicNode
                  :@ Needs '[ActInput]
                  :@ UsesEffects '[Goto G.Exit Response]
    -- ^ Generate response via LLM
    --
    -- Input: ActInput (context + action)
    -- Output: Response
    --
    -- Note: LogicNode because response generation uses tools and
    -- custom prompt building based on Action type.

  , tgExit :: mode :- Exit Response
    -- ^ Graph exit: returns Response to caller
  }
  deriving Generic


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER TYPE SIGNATURES (for reference)
--
-- These show what the handlers will look like when implemented.
-- The actual implementations go in Tidying.Graph.Handlers.
-- ════════════════════════════════════════════════════════════════════════════

{- Handler signatures (for documentation):

-- Effect stack for handlers
type TidyingEffects = '[State SessionState, LLM, Emit TidyingEvent, Log, Time]

analyzePhotosHandler
  :: UserInput
  -> Eff TidyingEffects (GotoChoice '[To "tgExtract" PhotoResult])

extractHandler
  :: PhotoResult
  -> Eff TidyingEffects (GotoChoice '[To "tgRoute" ExtractResult])

routeHandler
  :: ExtractResult
  -> Eff TidyingEffects (GotoChoice '[To "tgAct" ActInput, To Exit Response])

actHandler
  :: ActInput
  -> Eff TidyingEffects (GotoChoice '[To Exit Response])

-}
