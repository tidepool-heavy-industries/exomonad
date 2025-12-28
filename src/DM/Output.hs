{-# LANGUAGE TemplateHaskell #-}
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

    -- * Schemas (derived from Haddock comments)
  , turnOutputJSONSchema
  , compressionOutputJSONSchema
  ) where

import DM.State
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Tidepool.Schema (JSONSchema, deriveJSONSchema)

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
  { -- | Narrative prose describing what happens this turn. Include NPC dialogue inline.
    narration :: Text
    -- | How this beat connects causally to previous events.
  , narrativeConnector :: Maybe NarrativeConnector
    -- | Change in stress from -9 to +9. Positive for costs, negative for relief.
  , stressDelta :: Int
    -- | Change in coin. Positive for gains, negative for expenses.
  , coinDelta :: Int
    -- | Change in heat from 0 to +4. Heat draws faction attention.
  , heatDelta :: Int
    -- | True to continue the current scene, False to end it.
  , continueScene :: Bool
    -- | If there was a cost or setback, describe it for consequence echoing.
  , costDescription :: Maybe Text
    -- | If there is an unresolved threat, describe it for future echoing.
  , threatDescription :: Maybe Text
    -- | 2-3 suggested next actions the player might take.
  , suggestedActions :: [Text]
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
  { -- | One paragraph summary of what happened during the compressed segment.
    summary :: Text
    -- | Comma-separated list of key dramatic moments worth remembering.
  , keyMoments :: Text
    -- | Comma-separated seeds for future consequences to echo.
  , consequenceSeeds :: Text
    -- | Net stress change over the compressed segment.
  , stressChange :: Int
    -- | Net coin change over the compressed segment.
  , coinChange :: Int
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

-- TH stage separator - required for deriveJSONSchema to see the types
$(return [])

-- ══════════════════════════════════════════════════════════════
-- SCHEMAS (derived from Haddock comments via TH)
-- ══════════════════════════════════════════════════════════════

-- | Schema for turn output, derived from field Haddocks
turnOutputJSONSchema :: JSONSchema
turnOutputJSONSchema = $(deriveJSONSchema ''TurnOutput)

-- | Schema for compression output, derived from field Haddocks
compressionOutputJSONSchema :: JSONSchema
compressionOutputJSONSchema = $(deriveJSONSchema ''CompressionOutput)
