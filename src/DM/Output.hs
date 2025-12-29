-- | DM Structured Output Types (Simplified for API grammar limits)
module DM.Output
  ( -- * Turn Output
    TurnOutput(..)
  , NarrativeConnector(..)
  , emptyTurnOutput
  , applyTurnOutput

    -- * Die Outcome (for spend_die tool input)
  , DieOutcome(..)

    -- * Compression Output
  , CompressionOutput(..)
  , RumorInit(..)
  , applyCompression
  ) where

import DM.State
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON(..), (.:), (.:?), (.!=), withObject)

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

-- Note: DiceAction type removed - dice mechanics now handled by spend_die tool
-- DieOutcome is still used by SpendDie tool input

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
  , costDescription :: Maybe Text           -- If costly/setback, describe the cost for echoing
  , threatDescription :: Maybe Text         -- If unresolved threat, describe for echoing
  , suggestedActions :: [Text]              -- 2-3 suggested next actions for player
  , traumaAssigned :: Maybe Text            -- If trauma turn, the trauma gained (e.g. "Cold", "Haunted")
  -- Downtime-specific fields (The Tide)
  , diceRecovered :: Int                    -- Dice restored to pool during downtime
  , hookDescription :: Maybe Text           -- What pulls them back from downtime
  , timeElapsed :: Maybe Text               -- How long downtime lasted ("three days", etc.)
  }
  deriving (Show, Eq, Generic, ToJSON)

-- | Custom FromJSON to handle missing fields gracefully
-- Different mood schemas include different subsets of fields, so we need defaults
-- Downtime uses stressHealed/heatDecay (positive = reduction), which we translate
instance FromJSON TurnOutput where
  parseJSON = withObject "TurnOutput" $ \o -> do
    narration <- o .: "narration"
    narrativeConnector <- o .:? "narrativeConnector"
    -- Handle both stressDelta (direct) and stressHealed (inverted for downtime)
    stressHealed <- o .:? "stressHealed" .!= 0
    stressDeltaDirect <- o .:? "stressDelta" .!= 0
    let stressDelta = if stressHealed > 0 then negate stressHealed else stressDeltaDirect
    coinDelta <- o .:? "coinDelta" .!= 0
    -- Handle both heatDelta (direct) and heatDecay (inverted for downtime)
    heatDecay <- o .:? "heatDecay" .!= 0
    heatDeltaDirect <- o .:? "heatDelta" .!= 0
    let heatDelta = if heatDecay > 0 then negate heatDecay else heatDeltaDirect
    costDescription <- o .:? "costDescription"
    threatDescription <- o .:? "threatDescription"
    suggestedActions <- o .:? "suggestedActions" .!= []
    traumaAssigned <- o .:? "traumaAssigned"
    -- Note: diceAction no longer parsed - dice handled by spend_die tool
    -- Downtime-specific fields
    diceRecovered <- o .:? "diceRecovered" .!= 0
    hookDescription <- o .:? "hookDescription"
    timeElapsed <- o .:? "timeElapsed"
    pure TurnOutput
      { narration = narration
      , narrativeConnector = narrativeConnector
      , stressDelta = stressDelta
      , coinDelta = coinDelta
      , heatDelta = heatDelta
      , costDescription = costDescription
      , threatDescription = threatDescription
      , suggestedActions = suggestedActions
      , traumaAssigned = traumaAssigned
      , diceRecovered = diceRecovered
      , hookDescription = hookDescription
      , timeElapsed = timeElapsed
      }

emptyTurnOutput :: TurnOutput
emptyTurnOutput = TurnOutput
  { narration = ""
  , narrativeConnector = Nothing
  , stressDelta = 0
  , coinDelta = 0
  , heatDelta = 0
  , costDescription = Nothing
  , threatDescription = Nothing
  , suggestedActions = []
  , traumaAssigned = Nothing
  , diceRecovered = 0
  , hookDescription = Nothing
  , timeElapsed = Nothing
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
  -- Recover dice during downtime (add to pool, capped at max pool size)
  , dicePool = if output.diceRecovered > 0
      then recoverDice output.diceRecovered state.dicePool
      else state.dicePool
  -- Track costs for consequence echoing (keep last 3)
  , recentCosts = take 3 $ case output.costDescription of
      Just cost -> cost : state.recentCosts
      Nothing -> state.recentCosts
  -- Track unresolved threats (keep last 5)
  , unresolvedThreats = take 5 $ case output.threatDescription of
      Just threat -> threat : state.unresolvedThreats
      Nothing -> state.unresolvedThreats
  }
  where
    clamp lo hi x = max lo (min hi x)
    -- If trauma was assigned, stress resets to 0 regardless of stressDelta
    newStress = case output.traumaAssigned of
      Just _  -> 0
      Nothing -> clamp 0 9 (state.player.stress + output.stressDelta)
    -- Add dice to pool during downtime recovery
    recoverDice n pool =
      let maxPool = 6  -- Standard Blades pool size
          currentCount = length pool.poolDice
          toRecover = min n (maxPool - currentCount)
          -- Generate dice values 1-6 for recovered dice (simple approach)
          newDice = take toRecover [4, 3, 5, 2, 6, 1]  -- Placeholder values
      in pool { poolDice = pool.poolDice ++ newDice }

-- ══════════════════════════════════════════════════════════════
-- COMPRESSION OUTPUT
-- ══════════════════════════════════════════════════════════════

-- | Rumor extracted during compression
data RumorInit = RumorInit
  { riContent :: Text             -- What the rumor says
  , riSpread :: Text              -- "whisper", "tavern", "common_knowledge"
  , riTrue :: Maybe Bool          -- True/False/null for unknown
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Simplified compression output - complex extractions removed
data CompressionOutput = CompressionOutput
  { summary :: Text               -- One paragraph summary
  , keyMoments :: [Text]          -- Key moments from the scene (3-5 items)
  , consequenceSeeds :: Text      -- Comma-separated consequence seeds
  , stressChange :: Int           -- Net stress change
  , coinChange :: Int             -- Net coin change
  , newRumors :: [RumorInit]      -- Rumors that emerged from this scene
  }
  deriving (Show, Eq, Generic, ToJSON)

-- | Custom FromJSON to default newRumors to [] if missing
instance FromJSON CompressionOutput where
  parseJSON = withObject "CompressionOutput" $ \o ->
    CompressionOutput
      <$> o .: "summary"
      <*> o .: "keyMoments"
      <*> o .:? "consequenceSeeds" .!= ""
      <*> o .:? "stressChange" .!= 0
      <*> o .:? "coinChange" .!= 0
      <*> o .:? "newRumors" .!= []

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
  -- Add new rumors from compression
  , rumors = state.rumors ++ zipWith (initToRumor baseId) [0..] output.newRumors
  }
  where
    clamp lo hi x = max lo (min hi x)
    baseId = length state.rumors

-- | Convert RumorInit to Rumor with generated ID
initToRumor :: Int -> Int -> RumorInit -> Rumor
initToRumor baseId idx ri = Rumor
  { rumorId = RumorId $ "rumor_" <> T.pack (show (baseId + idx))
  , rumorContent = ri.riContent
  , rumorSource = PublicKnowledge  -- Compression extracts from scene, so public
  , rumorTruthValue = case ri.riTrue of
      Just True  -> TrueRumor
      Just False -> FalseRumor
      Nothing    -> Unknown
  , rumorSpread = parseSpread ri.riSpread
  }
  where
    parseSpread s = case T.toLower s of
      "whisper"          -> Whisper
      "tavern"           -> Tavern
      "common_knowledge" -> CommonKnowledge
      "universal"        -> Universal
      _                  -> Tavern  -- Default to tavern-level spread
