-- | DM Structured Output Types (Simplified for API grammar limits)
module DM.Output
  ( -- * Turn Output
    TurnOutput(..)
  , NarrativeConnector(..)
  , emptyTurnOutput
  , applyTurnOutput

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
  }

-- | Apply turn output to world state
-- Tracks costs and threats for consequence echoing
applyTurnOutput :: TurnOutput -> WorldState -> WorldState
applyTurnOutput output state = state
  { player = (state.player)
      { stress = newStress
      , coin = max 0 (state.player.coin + output.coinDelta)
      , heat = clamp 0 10 (state.player.heat + output.heatDelta)
      }
  -- Preserve scene if stress will trigger trauma (we need scene for trauma processing)
  , scene = if output.continueScene || traumaWillTrigger
            then state.scene
            else Nothing
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
    newStress = clamp 0 9 (state.player.stress + output.stressDelta)
    -- Trauma triggers when stress crosses from <9 to 9
    traumaWillTrigger = state.player.stress < 9 && newStress >= 9

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
