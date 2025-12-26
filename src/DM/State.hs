-- | DM World State Types
module DM.State
  ( -- * Core State
    WorldState(..)
  , initialWorld
  
    -- * Factions
  , Faction(..)
  , FactionId(..)
  , Attitude(..)
  , Goal(..)
  , GoalId(..)
  , GoalStatus(..)
  , ResourcePool(..)
  , Secret(..)
  , Fact(..)
  
    -- * NPCs
  , Npc(..)
  , NpcId(..)
  , Disposition(..)
  , Want(..)
  , Fear(..)
  , Urgency(..)
  , Severity(..)
  
    -- * Clocks
  , Clock(..)
  , ClockId(..)
  , Trigger(..)
  , Consequence(..)
  , ActionPattern(..)
  , FactionAction(..)
  , Target(..)
  , LocationDelta(..)
  , Escalation(..)
  
    -- * Locations
  , Location(..)
  , LocationId(..)
  
    -- * Narrative
  , Rumor(..)
  , RumorId(..)
  , TruthValue(..)
  , SpreadLevel(..)
  , RumorSource(..)
  , Thread(..)
  , ThreadId(..)
  , Tension(..)
  
    -- * Scenes
  , ActiveScene(..)
  , SceneBeat(..)
  , SceneSummary(..)
  , Stakes(..)
  , Tone(..)
  
    -- * Misc
  , Duration(..)
  , Tag(..)
  ) where

import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- CORE STATE
-- ══════════════════════════════════════════════════════════════

data WorldState = WorldState
  { factions :: HashMap FactionId Faction
  , clocks :: HashMap ClockId Clock
  , locations :: HashMap LocationId Location
  , npcs :: HashMap NpcId Npc
  , rumors :: [Rumor]
  , threads :: [Thread]
  , scene :: Maybe ActiveScene
  , tone :: Tone
  , sessionHistory :: Seq SceneSummary
  , sessionGoals :: [Text]
  }
  deriving (Show, Eq, Generic)

data SceneSummary = SceneSummary
  { summaryText :: Text
  , summaryKeyBeats :: [SceneBeat]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

initialWorld :: WorldState
initialWorld = WorldState
  { factions = HM.empty
  , clocks = HM.empty
  , locations = HM.empty
  , npcs = HM.empty
  , rumors = []
  , threads = []
  , scene = Nothing
  , tone = ToneNeutral
  , sessionHistory = Seq.empty
  , sessionGoals = []
  }

-- ══════════════════════════════════════════════════════════════
-- FACTIONS
-- ══════════════════════════════════════════════════════════════

newtype FactionId = FactionId { unFactionId :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, Hashable)

data Faction = Faction
  { factionName :: Text
  , factionAttitude :: Attitude
  , factionGoals :: [Goal]
  , factionResources :: ResourcePool
  , factionSecrets :: [Secret]
  , factionKnownFacts :: [Fact]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Attitude 
  = Hostile 
  | Wary 
  | Neutral 
  | Favorable 
  | Allied
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

newtype GoalId = GoalId { unGoalId :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, Hashable)

data Goal = Goal
  { goalId :: GoalId
  , goalDescription :: Text
  , goalStatus :: GoalStatus
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GoalStatus
  = Pursuing
  | Blocked Text
  | Achieved
  | Abandoned
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ResourcePool = ResourcePool
  { gold :: Int
  , soldiers :: Int
  , influence :: Int
  , specialResources :: [(Text, Int)]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Secret = Secret
  { secretContent :: Text
  , secretKnownBy :: [Either FactionId NpcId]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Fact = Fact
  { factContent :: Text
  , factSource :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- NPCS
-- ══════════════════════════════════════════════════════════════

newtype NpcId = NpcId { unNpcId :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, Hashable)

data Npc = Npc
  { npcName :: Text
  , npcFaction :: Maybe FactionId
  , npcDisposition :: Disposition
  , npcWants :: [Want]
  , npcFears :: [Fear]
  , npcKnows :: [Secret]
  , npcLocation :: Maybe LocationId
  , npcVoiceNotes :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Disposition
  = DispHostile
  | Suspicious
  | DispNeutral
  | Friendly
  | Loyal
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data Want = Want
  { wantDescription :: Text
  , wantUrgency :: Urgency
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Fear = Fear
  { fearDescription :: Text
  , fearSeverity :: Severity
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Urgency = Low | Medium | High | Desperate
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data Severity = Minor | Moderate | Severe | Existential
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- CLOCKS
-- ══════════════════════════════════════════════════════════════

newtype ClockId = ClockId { unClockId :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, Hashable)

data Clock = Clock
  { clockName :: Text
  , clockSegments :: Int
  , clockFilled :: Int
  , clockVisible :: Bool
  , clockConsequence :: Consequence
  , clockTriggers :: [Trigger]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Trigger
  = OnPlayerAction ActionPattern
  | OnFactionMove FactionId
  | OnClockFilled ClockId
  | OnTimePass Duration
  | OnRumorSpread RumorId
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype ActionPattern = ActionPattern { unActionPattern :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

data Consequence
  = FactionMoves FactionId FactionAction
  | RevealSecret Secret
  | SpawnThread Thread
  | ChangeLocation LocationId LocationDelta
  | Escalate Escalation
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data FactionAction
  = Attack Target
  | Negotiate FactionId
  | Retreat
  | Scheme Goal
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype Target = Target { unTarget :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

data LocationDelta = LocationDelta
  { deltaDescription :: Text
  , deltaEffects :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Escalation = Escalation
  { escalationDescription :: Text
  , escalationSeverity :: Severity
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- LOCATIONS
-- ══════════════════════════════════════════════════════════════

newtype LocationId = LocationId { unLocationId :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, Hashable)

data Location = Location
  { locationName :: Text
  , locationDescription :: Text
  , locationControlledBy :: Maybe FactionId
  , locationFeatures :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- NARRATIVE
-- ══════════════════════════════════════════════════════════════

newtype RumorId = RumorId { unRumorId :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, Hashable)

data Rumor = Rumor
  { rumorId :: RumorId
  , rumorContent :: Text
  , rumorSource :: RumorSource
  , rumorTruthValue :: TruthValue
  , rumorSpread :: SpreadLevel
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data RumorSource
  = OverheardFrom NpcId
  | PublicKnowledge
  | FromFaction FactionId
  | PlayerStarted
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TruthValue
  = TrueRumor
  | FalseRumor
  | PartiallyTrue Text
  | Unknown
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SpreadLevel
  = Whisper
  | Tavern
  | CommonKnowledge
  | Universal
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

newtype ThreadId = ThreadId { unThreadId :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, Hashable)

data Thread = Thread
  { threadId :: ThreadId
  , threadHook :: Text
  , threadInvolves :: [Either FactionId NpcId]
  , threadTension :: Tension
  , threadDeadline :: Maybe ClockId
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Tension
  = Simmering
  | Rising
  | Urgent
  | Critical
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- SCENES
-- ══════════════════════════════════════════════════════════════

data ActiveScene = ActiveScene
  { sceneLocation :: LocationId
  , scenePresent :: [NpcId]
  , sceneStakes :: Stakes
  , sceneBeats :: Seq SceneBeat
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SceneBeat
  = PlayerAction Text [Tag]
  | NpcAction NpcId Text
  | EnvironmentShift Text
  | Revelation Secret
  | ClockTick ClockId Int
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype Stakes = Stakes { unStakes :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

data Tone
  = Tense
  | ToneNeutral
  | Comedic
  | Dark
  | Hopeful
  | Mysterious
  deriving (Show, Eq, Enum, Bounded, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- MISC
-- ══════════════════════════════════════════════════════════════

data Duration
  = Minutes Int
  | Hours Int
  | Days Int
  | Weeks Int
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype Tag = Tag { unTag :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, Hashable)
