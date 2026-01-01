{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | DM Graph Runner
--
-- Entry points for running the DM graph.
module DM.Graph.Run
  ( -- * Graph Execution
    runDMGraph
  , runDMGraphFromResume

    -- * Effect Interpretation
  , interpretDMEffects
  ) where

import Data.Text (Text)
import Effectful
import Effectful.State.Static.Local (State, runState)

import Tidepool.Effect (LLM, RequestInput, Log, Random, Emit)
import Tidepool.Graph.Execute (runGraph, runGraphFrom)

import DM.State (WorldState)
import DM.Tools (DMEvent)
import DM.Graph (DMGraph)
import DM.Graph.Types (PlayerInput(..), Response)
import DM.Graph.Handlers (DMEffects, dmHandlers)

-- ══════════════════════════════════════════════════════════════
-- GRAPH EXECUTION
-- ══════════════════════════════════════════════════════════════

-- | Run the DM graph from entry to exit.
--
-- Takes player input and returns the final response after executing
-- all graph transitions until Exit is reached.
--
-- @
-- response <- runDMGraph (PlayerInput "I look around" Nothing)
-- @
runDMGraph
  :: PlayerInput
  -> Eff DMEffects Response
runDMGraph = runGraph dmHandlers

-- | Run the DM graph from the resume router.
--
-- This is the main entry point - it uses resumeRouter to determine
-- which node to start from based on the saved mood.
--
-- For testing specific paths, use runGraphFrom directly:
-- @
-- response <- runGraphFrom @"action" dmHandlers actionSetup
-- @
runDMGraphFromResume
  :: PlayerInput
  -> Eff DMEffects Response
runDMGraphFromResume = runGraphFrom @"resumeRouter" dmHandlers

-- ══════════════════════════════════════════════════════════════
-- EFFECT INTERPRETATION
-- ══════════════════════════════════════════════════════════════

-- | Interpret DM effects into IO.
--
-- This is the runner that provides actual implementations for all
-- effects used by the graph handlers.
--
-- Usage:
-- @
-- finalResponse <- runEff
--   . interpretDMEffects initialState eventHandler inputHandler llmConfig
--   $ runDMGraph playerInput
-- @
--
-- Note: This is a placeholder - the actual implementation needs to
-- wire up all the effect interpreters from Tidepool.Effect.
interpretDMEffects
  :: WorldState                    -- ^ Initial game state
  -> (DMEvent -> IO ())            -- ^ Event handler (for GUI)
  -> Eff DMEffects a               -- ^ Graph computation
  -> IO (a, WorldState)            -- ^ Result and final state
interpretDMEffects _initialState _eventHandler _computation = do
  -- TODO: Wire up effect interpreters
  -- This should use:
  -- - runState for WorldState
  -- - runLLMWithTools for LLM
  -- - runRequestInput for player choices
  -- - runEmit for events
  -- - runRandom for dice
  -- - runLog for debug
  error "interpretDMEffects: not yet implemented - use existing Loop.hs runners for now"
