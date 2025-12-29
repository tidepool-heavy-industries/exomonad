-- | DM Structured Output Types (Simplified for API grammar limits)
module DM.Output
  ( -- * Turn Output
    TurnOutput(..)
  , NarrativeConnector(..)
  , emptyTurnOutput
  , applyTurnOutput

    -- * Dice Action (for action mode structured output)
  , DiceAction(..)
  , DieOutcome(..)

    -- * Compression Output
  , CompressionOutput(..)
  , applyCompression
  ) where

import DM.State
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- DICE ACTION (for action mode structured output)
-- ══════════════════════════════════════════════════════════════

-- | A single precommitted outcome for one die
-- LLM provides these in structured output; player chooses; game applies
data DieOutcome = DieOutcome
  { dieValue :: Int       -- Which die this outcome is for (must match pool)
  , hint :: Text          -- 3-8 words shown during choice
  , stressCost :: Int     -- Stress delta (usually 0-3, can be negative for relief)
  , heatCost :: Int       -- Heat delta (0-2 typical)
  , coinDelta :: Int      -- Coin change (positive = gain, negative = cost)
  , narrative :: Text     -- 1-3 sentences revealed after choice
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Dice action in structured output - REQUIRED in action mode
-- Player sees hint + costs BEFORE choosing; game applies chosen outcome
data DiceAction = DiceAction
  { situation :: Text           -- What's at stake
  , position :: Position        -- Controlled/Risky/Desperate
  , outcomes :: [DieOutcome]    -- One outcome per die in pool (parallel to pool)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- TURN OUTPUT
-- ══════════════════════════════════════════════════════════════

-- | Narrative connector - forces causal thinking
-- "And then" is lazy; these connectors demand causality
data NarrativeConnector
  = Therefore   -- This happened because of what came before
  | But         -- This happened despite what came before
  | Meanwhile   -- Parallel action (use sparingly)
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Simplified turn output that fits within API grammar limits
-- NPC dialogue should be included directly in narration
data TurnOutput = TurnOutput
  { narration :: Text                       -- Narrative prose for this turn
  , narrativeConnector :: Maybe NarrativeConnector  -- How this connects to previous beat
  , stressDelta :: Int                      -- Change in stress (-9 to +9)
  , coinDelta :: Int                        -- Change in coin
  , heatDelta :: Int                        -- Change in heat (0 to +4)
  , continueScene :: Bool                   -- True to continue, False to end scene
  , costDescription :: Maybe Text           -- If costly/setback, describe the cost for echoing
  , threatDescription :: Maybe Text         -- If unresolved threat, describe for echoing
  , suggestedActions :: [Text]              -- 2-3 suggested next actions for player
  , traumaAssigned :: Maybe Text            -- If trauma turn, the trauma gained (e.g. "Cold", "Haunted")
  , diceAction :: Maybe DiceAction          -- Required in action mode: dice outcomes for player choice
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

emptyTurnOutput :: TurnOutput
emptyTurnOutput = TurnOutput
  { narration = ""
  , narrativeConnector = Nothing
  , stressDelta = 0
  , coinDelta = 0
  , heatDelta = 0
  , continueScene = True
  , costDescription = Nothing
  , threatDescription = Nothing
  , suggestedActions = []
  , traumaAssigned = Nothing
  , diceAction = Nothing
  }

-- | Apply turn output to world state (cross-cutting updates only)
--
-- Phase transitions (scene ending, etc.) are handled by the Loop.
-- This function only updates player state and consequence echoing.
applyTurnOutput :: TurnOutput -> WorldState -> WorldState
applyTurnOutput output state = state
  { player = (state.player)
      { stress = newStress
      , coin = max 0 (state.player.coin + output.coinDelta)
      , heat = clamp 0 10 (state.player.heat + output.heatDelta)
      -- Add trauma if assigned (and reset stress to 0)
      , trauma = case output.traumaAssigned of
          Just t  -> Trauma t : state.player.trauma
          Nothing -> state.player.trauma
      }
  -- Track costs for consequence echoing (keep last 3)
  , recentCosts = take 3 $ case output.costDescription of
      Just cost -> cost : state.recentCosts
      Nothing -> state.recentCosts
  -- Track unresolved threats
  , unresolvedThreats = case output.threatDescription of
      Just threat -> threat : state.unresolvedThreats
      Nothing -> state.unresolvedThreats
  }
  where
    clamp lo hi x = max lo (min hi x)
    -- If trauma was assigned, stress resets to 0 regardless of stressDelta
    newStress = case output.traumaAssigned of
      Just _  -> 0
      Nothing -> clamp 0 9 (state.player.stress + output.stressDelta)

-- ══════════════════════════════════════════════════════════════
-- COMPRESSION OUTPUT
-- ══════════════════════════════════════════════════════════════

-- | Simplified compression output - complex extractions removed
data CompressionOutput = CompressionOutput
  { summary :: Text               -- One paragraph summary
  , keyMoments :: Text            -- Comma-separated key moments
  , consequenceSeeds :: Text      -- Comma-separated consequence seeds
  , stressChange :: Int           -- Net stress change
  , coinChange :: Int             -- Net coin change
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Apply simplified compression output to world state
applyCompression :: CompressionOutput -> WorldState -> WorldState
applyCompression output state = state
  { player = (state.player)
      { stress = clamp 0 9 (state.player.stress + output.stressChange)
      , coin = max 0 (state.player.coin + output.coinChange)
      }
  -- Store consequence seeds as unresolved threats for future echoing
  , unresolvedThreats = if T.null output.consequenceSeeds
      then state.unresolvedThreats
      else output.consequenceSeeds : state.unresolvedThreats
  }
  where
    clamp lo hi x = max lo (min hi x)
