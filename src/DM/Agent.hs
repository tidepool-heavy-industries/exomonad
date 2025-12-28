{-# LANGUAGE OverloadedStrings #-}
-- | DM Agent
--
-- Wraps the DM game loop as a Tidepool Agent.
--
module DM.Agent
  ( dm
  , DMM
  ) where

import Data.Text (Text)

import Tidepool
import DM.State (WorldState, initialWorld)
import DM.Loop (dmTurn, PlayerInput(..), Response(..))
import DM.Tools (DMEvent(..), makeDMDispatcher)

-- | The DM agent monad (no extra effects)
type DMM = AgentM WorldState DMEvent '[]

-- | The DM agent
--
-- A Blades in the Dark-style dungeon master agent with:
-- - Mood-based state machine (Scene → Action → Aftermath → Downtime)
-- - FitD dice mechanics
-- - Tool support (ThinkAsDM, SpeakAsNPC, SpendDie, etc.)
--
-- Usage:
--
-- @
-- main = do
--   finalState <- tidepool config dm
--   putStrLn "Game over"
-- @
dm :: SimpleAgent WorldState DMEvent
dm = Agent
  { agentName     = "dungeon-master"
  , agentInit     = initialWorld
  , agentTurn     = dmTurnWrapper
  , agentStartup  = dmStartup
  , agentShutdown = dmShutdown
  , agentDispatcher = dmDispatcher
  }

-- | Wrap dmTurn to match Agent turn signature
--
-- dmTurn expects PlayerInput, we receive Text
-- TODO: Add Narration constructor to DMEvent for proper narrative output
dmTurnWrapper :: Text -> DMM ()
dmTurnWrapper inputText = do
  let input = PlayerInput
        { piActionText = inputText
        , piActionTags = []  -- Could parse from input if needed
        }
  response <- dmTurn input
  -- Emit the narrative response as a thought (temporary)
  -- TODO: Use proper Narration event type
  emit $ DMThought response.responseText

-- | Startup: welcome message
dmStartup :: DMM ()
dmStartup = do
  emit $ DMThought "The streets of Doskvol await. What's your first move?"

-- | Shutdown: session summary
dmShutdown :: DMM ()
dmShutdown = do
  emit $ DMThought "Session ends. The city remembers..."

-- | DM dispatcher wrapping makeDMDispatcher
dmDispatcher :: AgentDispatcher WorldState DMEvent
dmDispatcher = AgentDispatcher $ \name input ->
  makeDMDispatcher name input
