-- | DM Template Context
module DM.Context
  ( -- * Turn Context
    DMContext(..)
  , buildDMContext
  
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
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- TURN CONTEXT
-- ══════════════════════════════════════════════════════════════

data DMContext = DMContext
  { ctxLocation :: Location
  , ctxPresentNpcs :: [NpcWithDisposition]
  , ctxSceneBeats :: [Text]
  , ctxStakes :: Text
  , ctxVisibleClocks :: [Clock]
  , ctxHiddenClocks :: [Clock]
  , ctxActiveThreads :: [Thread]
  , ctxRelevantRumors :: [Rumor]
  , ctxFactionsInPlay :: [FactionSummary]
  , ctxTone :: Tone
  , ctxSessionGoals :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

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
