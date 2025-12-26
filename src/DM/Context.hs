-- | DM Template Context
module DM.Context
  ( -- * Turn Context
    DMContext(..)
  , buildDMContext

    -- * Dice State Context
  , DiceContext(..)
  , LockedOutcome(..)

    -- * Precarity
  , Precarity(..)
  , calculatePrecarity
  , precarityScore

    -- * Compression Context
  , CompressionContext(..)
  , buildCompressionContext

    -- * Helper Types
  , NpcWithDisposition(..)
  , FactionSummary(..)

    -- * Enrichment
  , enrichNpc
  , summarizeFaction
  ) where

import DM.State
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- TURN CONTEXT
-- ══════════════════════════════════════════════════════════════

data DMContext = DMContext
  { -- Player state (for resource bars, precarity)
    ctxPlayer :: PlayerState
  , ctxPrecarity :: Precarity
  -- Dice mechanics (the key "see your decision space" feature)
  , ctxDice :: DiceContext
  -- Scene state
  , ctxLocation :: Location
  , ctxPresentNpcs :: [NpcWithDisposition]
  , ctxSceneBeats :: [Text]
  , ctxStakes :: Text
  -- World state
  , ctxVisibleClocks :: [Clock]
  , ctxHiddenClocks :: [Clock]
  , ctxActiveThreads :: [Thread]
  , ctxRelevantRumors :: [Rumor]
  , ctxFactionsInPlay :: [FactionSummary]
  , ctxTone :: Tone
  , ctxSessionGoals :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Dice state for the template
-- Either we're waiting for player to pick a die, or we have a locked outcome to narrate
data DiceContext = DiceContext
  { dcPool :: [Int]                     -- Available dice (1-6 values)
  , dcLockedOutcome :: Maybe LockedOutcome  -- Outcome waiting to be narrated
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A locked outcome - the die is chosen, now LLM must narrate this exact result
data LockedOutcome = LockedOutcome
  { loContext :: Text       -- What the player was trying to do
  , loPosition :: Position  -- Controlled/Risky/Desperate
  , loEffect :: Effect      -- Limited/Standard/Great
  , loStakes :: Text        -- What was at stake
  , loDieValue :: Int       -- The die they chose (1-6)
  , loTier :: OutcomeTier   -- Calculated outcome: Critical/Success/Partial/Bad/Disaster
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Precarity level determines narrative voice and tone
-- Based on heist-engine's precarity calculation
data Precarity
  = OperatingFromStrength  -- Score < 5: Expansive, but comfort breeds carelessness
  | RoomToManeuver         -- Score 5-9: Noir cool, mistakes compound
  | WallsClosingIn         -- Score 10-14: Tense, consequences arriving
  | HangingByThread        -- Score >= 15: Urgent, no comfort
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | Calculate precarity from player state
-- precarity = stress + heat + (3 if hunted) + (2 if recovering) + wanted*2
calculatePrecarity :: PlayerState -> Bool -> Bool -> Precarity
calculatePrecarity ps hunted recovering =
  let score = precarityScore ps hunted recovering
  in if score >= 15 then HangingByThread
     else if score >= 10 then WallsClosingIn
     else if score >= 5 then RoomToManeuver
     else OperatingFromStrength

-- | Raw precarity score for calculations
precarityScore :: PlayerState -> Bool -> Bool -> Int
precarityScore ps hunted recovering =
  ps.stress + ps.heat + (ps.wanted * 2)
  + (if hunted then 3 else 0)
  + (if recovering then 2 else 0)

data NpcWithDisposition = NpcWithDisposition
  { nwdNpc :: Npc
  , nwdDispositionLabel :: Text
  , nwdCurrentWant :: Maybe Text
  , nwdVoiceNotes :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data FactionSummary = FactionSummary
  { fsFactionName :: Text
  , fsAttitudeLabel :: Text
  , fsCurrentGoal :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

buildDMContext :: WorldState -> DMContext
buildDMContext world = error "TODO: buildDMContext - extract current scene, enrich NPCs, filter clocks/threads/rumors"

enrichNpc :: WorldState -> NpcId -> NpcWithDisposition
enrichNpc world npcId = error "TODO: enrichNpc - lookup NPC, render disposition, find most urgent want"

summarizeFaction :: Faction -> FactionSummary
summarizeFaction faction = error "TODO: summarizeFaction - extract name, attitude label, active goal"

-- ══════════════════════════════════════════════════════════════
-- COMPRESSION CONTEXT
-- ══════════════════════════════════════════════════════════════

data CompressionContext = CompressionContext
  { ccSceneToCompress :: ActiveScene
  , ccAllBeats :: [Text]
  , ccFactionStates :: [FactionSummary]
  , ccNpcStates :: [NpcWithDisposition]
  , ccActiveClocks :: [Clock]
  , ccActiveThreads :: [Thread]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

buildCompressionContext :: ActiveScene -> WorldState -> CompressionContext
buildCompressionContext scene world = error "TODO: buildCompressionContext - render beats, collect faction/npc states"
