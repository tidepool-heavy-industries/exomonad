{-# LANGUAGE TemplateHaskell #-}
-- | DM Structured Output
--
-- Re-exports types from Types module and derives schemas.
-- Types are in separate module so getDoc can access their Haddock comments.
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
import qualified Data.Text as T
import Tidepool.Schema (JSONSchema, deriveJSONSchema)

-- Re-export types from Types module
import DM.Output.Types

-- ══════════════════════════════════════════════════════════════
-- APPLY FUNCTIONS
-- ══════════════════════════════════════════════════════════════

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

-- ══════════════════════════════════════════════════════════════
-- SCHEMAS (derived from Haddock comments via TH)
-- ══════════════════════════════════════════════════════════════

-- | Schema for turn output, derived from field Haddocks
turnOutputJSONSchema :: JSONSchema
turnOutputJSONSchema = $(deriveJSONSchema ''TurnOutput)

-- | Schema for compression output, derived from field Haddocks
compressionOutputJSONSchema :: JSONSchema
compressionOutputJSONSchema = $(deriveJSONSchema ''CompressionOutput)
