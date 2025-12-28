-- | DM World State Types
module DM.State
  ( -- * Core State
    WorldState(..)
  , initialWorld
  , GamePhase(..)
    -- ** Phase Accessors
  , currentScene
  , currentMood
    -- ** Phase Updaters (for non-tool code)
  , updateMood

    -- * DM Mood (State Machine)
  , DMMood(..)
  , SceneVariant(..)
  , SceneUrgency(..)
  , DowntimeVariant(..)
  , ActionVariant(..)
  , ActionDomain(..)
  , AftermathVariant(..)
  , TraumaVariant(..)
  , BargainVariant(..)
  , BargainOption(..)
  , BargainCost(..)
  , ClockInterrupt(..)
  , defaultMood

    -- * Player
  , PlayerState(..)
  , initialPlayer
  , Trauma(..)

    -- * Dice Mechanics
  , DicePool(..)
  , Position(..)
  , Effect(..)
  , OutcomeTier(..)
  , PendingOutcome(..)
  , calculateOutcome

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
  , ClockType(..)
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

    -- * BetweenScenes
  , BetweenScenesOption(..)
  , ClockSummary(..)
  , BetweenScenesContext(..)
  , optionLabel

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
import Data.Aeson (ToJSON(..), FromJSON(..), ToJSONKey, FromJSONKey, object, (.=), (.:), withObject)
import qualified Data.Text as T
import Text.Ginger.GVal (ToGVal(..), toGVal, dict, list)
import Text.Ginger.GVal.Generic (genericToGVal)
import qualified Data.Foldable as F

import DM.CharacterCreation (CharacterChoices, ClockType(..))

-- ══════════════════════════════════════════════════════════════
-- DM MOOD (State Machine)
-- ══════════════════════════════════════════════════════════════

-- | The DM's current mood determines template, tools, and output schema
data DMMood
  = MoodScene SceneVariant
  | MoodAction ActionVariant (Maybe ActionDomain)
  | MoodAftermath AftermathVariant
  | MoodDowntime DowntimeVariant       -- Promoted from scene variant
  | MoodTrauma TraumaVariant
  | MoodBargain BargainVariant         -- Out of dice, must make a deal
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Scene variants - every scene has a hook (no more FreePlay)
data SceneVariant
  = Encounter                        -- Someone/something demands attention
      { svSource :: Text             -- NPC, faction, complication
      , svUrgency :: SceneUrgency    -- How pressing is this
      , svEscapable :: Bool          -- Can they walk away?
      }
  | Opportunity                      -- Something offered
      { svOfferedBy :: Maybe NpcId   -- Who's offering (if anyone)
      , svNature :: Text             -- job, intel, alliance, gear, favor
      , svCatch :: Text              -- There's always a catch
      }
  | Discovery                        -- Found something
      { svWhat :: Text               -- Location, secret, evidence, person
      , svImplications :: [Text]     -- What this means
      }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Scene urgency levels (separate from NPC Want urgency)
data SceneUrgency
  = UrgencyLow       -- Relaxed, can take time
  | UrgencyMedium    -- Pressing, shouldn't dawdle
  | UrgencyHigh      -- Urgent, needs attention now
  | UrgencyCritical  -- Immediate, no time to plan
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | Downtime variants (promoted to top-level mood)
data DowntimeVariant
  = Recovery                         -- Healing stress/harm
      { dvActivities :: [Text]       -- What's available
      , dvTimeAvailable :: Text      -- "a few hours", "overnight", "a week"
      }
  | Project                          -- Long-term work
      { dvProjectName :: Text
      , dvProgress :: Int            -- Current progress
      , dvRequired :: Int            -- Total needed
      }
  | Entanglement                     -- Heat catches up
      { dvEntanglementType :: Text   -- Arrest, questioning, shakedown, etc.
      , dvEscapeOptions :: [Text]    -- Ways out
      }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Action variants - the position (controlled/risky/desperate)
data ActionVariant
  = AvControlled
      { avAdvantageSource :: Text
      , avRiskIfFails :: Text
      }
  | AvRisky
      { avThreat :: Text
      , avOpportunity :: Text
      }
  | AvDesperate
      { avWhyDesperate :: Text
      , avStakes :: Text
      , avPotentialTrauma :: Bool
      }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Action domain overlays - what kind of action
data ActionDomain
  = DomainInfiltration
  | DomainSocial
  | DomainViolence
  | DomainPursuit
  | DomainArcane
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Aftermath variants - the outcome type
data AftermathVariant
  = AmClean
      { amWhatAchieved :: Text
      }
  | AmCostly
      { amWhatAchieved :: Text
      , amCostsPaid :: [Text]
      , amNewComplications :: [Text]
      }
  | AmSetback
      { amWhatWentWrong :: Text
      , amImmediateDanger :: Bool
      , amEscapeRoute :: Text
      }
  | AmDisaster
      { amCatastrophe :: Text
      , amTraumaRisk :: Bool
      , amWorldStateChanges :: [Text]
      }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Trauma variant - the breaking point
data TraumaVariant = Breaking
  { tvWhatBroke :: Text
  , tvTraumaType :: Trauma
  , tvTrigger :: Text
  , tvAdrenaline :: Bool  -- If true, can snap back to action
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Bargain variant - out of dice, must make a deal or exit
-- LLM generates contextual bargains; player picks one or exits
data BargainVariant = Bargaining
  { bvWhatDrained :: Text        -- "the desperate chase", "holding the door"
  , bvCanRetreat :: Bool         -- Can they "go home to rest"? (not mid-combat)
  , bvRetreatDesc :: Text        -- "slip away to your garret in Crow's Foot"
  , bvPassOutDesc :: Text        -- "collapse behind the coal bins"
  , bvPreviousMood :: DMMood     -- Where to return after bargain accepted
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A bargain option offered when out of dice
-- LLM generates these based on context (nearby NPCs, factions, items, etc.)
data BargainOption = BargainOption
  { boDescription :: Text        -- "The Lampblacks' fence could help, but..."
  , boCost :: BargainCost        -- Mechanical cost
  , boDiceGained :: Int          -- 1-3 dice
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Mechanical costs for bargains - must result in state changes
data BargainCost
  = CostStress Int               -- Take stress
  | CostHeat Int                 -- Take heat
  | CostWanted                   -- Increase wanted level
  | CostClockTick ClockId Int    -- Advance a clock
  | CostFactionDebt FactionId    -- Owe a faction (attitude shift)
  | CostTrauma                   -- Accept a trauma (big cost, big reward)
  | CostItem Text                -- Burn a narrative item
  | CostMultiple [BargainCost]   -- Compound cost
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Clock interrupt - world asserting itself (not a mood, but forces transition)
data ClockInterrupt = ClockInterrupt
  { ciClockId :: ClockId
  , ciEventType :: Text
  , ciDescription :: Text
  , ciForcesAction :: Bool  -- If true, forces immediate Action state
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Default mood for new games (Encounter with no pressure)
defaultMood :: DMMood
defaultMood = MoodScene (Encounter "Starting scene" UrgencyLow True)

-- | Game phase as sum type - each constructor carries phase-specific state
--
-- This makes illegal states unrepresentable:
-- - PhasePlaying ALWAYS has a scene and mood
-- - PhaseScenarioInit ALWAYS has character choices
-- - PhaseBetweenScenes ALWAYS has context
data GamePhase
  = PhaseCharacterCreation
    -- ^ Initial state: needs character setup
  | PhaseScenarioInit CharacterChoices
    -- ^ Has character choices, needs scenario generation
  | PhasePlaying ActiveScene DMMood
    -- ^ Active gameplay with scene and mood
  | PhaseBetweenScenes BetweenScenesContext
    -- ^ Between scenes, player choosing what's next
  | PhaseSessionEnded
    -- ^ Player chose to end session
  deriving (Show, Eq, Generic)

-- Custom JSON instances for GamePhase sum type with payloads
instance ToJSON GamePhase where
  toJSON PhaseCharacterCreation = object ["tag" .= ("PhaseCharacterCreation" :: Text)]
  toJSON (PhaseScenarioInit choices) = object
    ["tag" .= ("PhaseScenarioInit" :: Text), "choices" .= choices]
  toJSON (PhasePlaying scene mood) = object
    ["tag" .= ("PhasePlaying" :: Text), "scene" .= scene, "mood" .= mood]
  toJSON (PhaseBetweenScenes ctx) = object
    ["tag" .= ("PhaseBetweenScenes" :: Text), "context" .= ctx]
  toJSON PhaseSessionEnded = object ["tag" .= ("PhaseSessionEnded" :: Text)]

instance FromJSON GamePhase where
  parseJSON = withObject "GamePhase" $ \v -> do
    tag <- v .: "tag"
    case tag :: Text of
      "PhaseCharacterCreation" -> pure PhaseCharacterCreation
      "PhaseScenarioInit" -> PhaseScenarioInit <$> v .: "choices"
      "PhasePlaying" -> PhasePlaying <$> v .: "scene" <*> v .: "mood"
      "PhaseBetweenScenes" -> PhaseBetweenScenes <$> v .: "context"
      "PhaseSessionEnded" -> pure PhaseSessionEnded
      other -> fail $ "Unknown GamePhase tag: " <> T.unpack other

-- ══════════════════════════════════════════════════════════════
-- CORE STATE
-- ══════════════════════════════════════════════════════════════

-- | World state with phase-specific data on GamePhase constructors
--
-- Cross-cutting state (persists across phases) stays here.
-- Phase-specific state (scene, mood, choices, context) lives on GamePhase.
data WorldState = WorldState
  { phase :: GamePhase                -- Phase with its specific state
  , player :: PlayerState             -- Stress/coin/heat persist across phases
  , factions :: HashMap FactionId Faction
  , clocks :: HashMap ClockId Clock
  , locations :: HashMap LocationId Location
  , npcs :: HashMap NpcId Npc
  , rumors :: [Rumor]
  , threads :: [Thread]
  , tone :: Tone
  , sessionHistory :: Seq SceneSummary
  , sessionGoals :: [Text]
  -- Dice mechanics (session state)
  , dicePool :: DicePool
  , pendingOutcome :: Maybe PendingOutcome
  -- Consequence echoing (narrative continuity)
  , recentCosts :: [Text]             -- From last 3 costly/setback outcomes
  , unresolvedThreats :: [Text]       -- Complications not yet addressed
  , pendingInterrupt :: Maybe ClockInterrupt  -- Clock triggered, awaiting handling
  -- UI state (persisted for resume)
  , suggestedActions :: [Text]        -- Last LLM suggestions for quick-pick buttons
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SceneSummary = SceneSummary
  { summaryText :: Text
  , summaryKeyBeats :: [SceneBeat]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

initialWorld :: WorldState
initialWorld = WorldState
  { phase = PhaseCharacterCreation
  , player = initialPlayer
  , factions = HM.empty
  , clocks = HM.empty
  , locations = HM.empty
  , npcs = HM.empty
  , rumors = []
  , threads = []
  , tone = ToneNeutral
  , sessionHistory = Seq.empty
  , sessionGoals = []
  , dicePool = DicePool []
  , pendingOutcome = Nothing
  , recentCosts = []
  , unresolvedThreats = []
  , pendingInterrupt = Nothing
  , suggestedActions = []
  }

-- | Get scene if in PhasePlaying (for read-only contexts)
currentScene :: WorldState -> Maybe ActiveScene
currentScene s = case s.phase of
  PhasePlaying scene _ -> Just scene
  _ -> Nothing

-- | Get mood if in PhasePlaying
currentMood :: WorldState -> Maybe DMMood
currentMood s = case s.phase of
  PhasePlaying _ mood -> Just mood
  _ -> Nothing

-- | Update mood if in PhasePlaying (no-op otherwise)
-- Note: Tools should use PlayingState effect's putMood instead
updateMood :: DMMood -> WorldState -> WorldState
updateMood newMood s = case s.phase of
  PhasePlaying scene _ -> s { phase = PhasePlaying scene newMood }
  _ -> s

-- ══════════════════════════════════════════════════════════════
-- PLAYER STATE
-- ══════════════════════════════════════════════════════════════

data PlayerState = PlayerState
  { stress :: Int           -- 0-9, at 9 you break and gain trauma
  , coin :: Int             -- Currency for bribes, gear, flashbacks
  , heat :: Int             -- 0-10, attention from authorities
  , wanted :: Int           -- 0-4, law enforcement escalation
  , trauma :: [Trauma]      -- Permanent scars that change how you play
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Trauma is freeform text - LLM can invent new ones
-- Common examples: paranoid, cold, obsessed, reckless, haunted, vicious
newtype Trauma = Trauma { unTrauma :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, Hashable)

initialPlayer :: PlayerState
initialPlayer = PlayerState
  { stress = 0
  , coin = 0
  , heat = 0
  , wanted = 0
  , trauma = []
  }

-- ══════════════════════════════════════════════════════════════
-- DICE MECHANICS
-- ══════════════════════════════════════════════════════════════

-- | Pool of available dice (1-6 values). Player sees all options.
newtype DicePool = DicePool { poolDice :: [Int] }
  deriving (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Position determines danger level of the action
data Position
  = Controlled  -- You have the upper hand
  | Risky       -- Standard danger, things could go either way
  | Desperate   -- Serious danger, consequences will be severe
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | Effect determines magnitude of success/failure
data Effect
  = Limited     -- Partial progress, small impact
  | Standard    -- Expected outcome, full progress
  | Great       -- Extra benefit, impressive success
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | Outcome tier based on die roll and position
data OutcomeTier
  = Critical    -- 6: Exceptional success, extra benefit
  | Success     -- 4-5: You do it
  | Partial     -- 2-3: You do it, but there's a cost
  | Bad         -- 1 at risky: Things go wrong
  | Disaster    -- 1 at desperate: Things go very wrong
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | Pending action waiting for player to select a die
data PendingOutcome = PendingOutcome
  { outcomeContext :: Text      -- What the player is trying to do
  , outcomePosition :: Position
  , outcomeEffect :: Effect
  , outcomeStakes :: Text       -- What happens on failure
  , chosenDie :: Maybe Int      -- Filled after player selection
  , chosenTier :: Maybe OutcomeTier  -- Calculated from die + position
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Calculate outcome tier from die value and position
--
-- Controlled: 6=Critical, 4-5=Success, 1-3=Partial (no bad outcomes)
-- Risky:      6=Critical, 4-5=Success, 2-3=Partial, 1=Bad
-- Desperate:  6=Critical, 4-5=Success, 2-3=Partial, 1=Disaster
calculateOutcome :: Position -> Int -> OutcomeTier
calculateOutcome pos die = case die of
  6 -> Critical
  5 -> Success
  4 -> Success
  3 -> Partial
  2 -> Partial
  1 -> case pos of
    Controlled -> Partial  -- Controlled never goes bad
    Risky      -> Bad
    Desperate  -> Disaster
  _ -> Partial  -- Invalid die value, treat as partial

-- ══════════════════════════════════════════════════════════════
-- FACTIONS
-- ══════════════════════════════════════════════════════════════

newtype FactionId = FactionId { unFactionId :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

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
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

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
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

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

data Urgency = Low | Medium | High | UrgencyDesperate
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data Severity = Minor | Moderate | Severe | Existential
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- CLOCKS
-- ══════════════════════════════════════════════════════════════

newtype ClockId = ClockId { unClockId :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

data Clock = Clock
  { clockName :: Text
  , clockSegments :: Int
  , clockFilled :: Int
  , clockVisible :: Bool
  , clockType :: ClockType            -- Threat or Goal
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
  -- Threat consequences (bad things happen):
  = FactionMoves FactionId FactionAction
  | RevealSecret Secret
  | SpawnThread Thread
  | ChangeLocation LocationId LocationDelta
  | Escalate Escalation
  -- Goal consequences (good things happen):
  | GainCoin Int
  | GainRep FactionId Int
  | GainAsset Text
  | OpenOpportunity Text
  | RemoveThreat ClockId
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
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

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
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

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
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

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
  | TensionCritical
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
  | DMNarration Text          -- DM's narrative response
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

-- ══════════════════════════════════════════════════════════════
-- TOGVAL INSTANCES (for template rendering via genericToGVal)
-- ══════════════════════════════════════════════════════════════

-- Container instances (not provided by ginger)
instance (ToGVal m k, ToGVal m v) => ToGVal m (HashMap k v) where
  toGVal hm = list [toGVal (k, v) | (k, v) <- HM.toList hm]

instance ToGVal m a => ToGVal m (Seq a) where
  toGVal s = list (map toGVal (F.toList s))

instance (ToGVal m a, ToGVal m b) => ToGVal m (Either a b) where
  toGVal (Left a) = dict [("Left", toGVal a)]
  toGVal (Right b) = dict [("Right", toGVal b)]

-- Core State
instance ToGVal m GamePhase where toGVal = genericToGVal
instance ToGVal m WorldState where toGVal = genericToGVal
instance ToGVal m SceneSummary where toGVal = genericToGVal

-- DM Mood (State Machine)
instance ToGVal m DMMood where toGVal = genericToGVal
instance ToGVal m SceneVariant where toGVal = genericToGVal
instance ToGVal m SceneUrgency where toGVal = genericToGVal
instance ToGVal m DowntimeVariant where toGVal = genericToGVal
instance ToGVal m ActionVariant where toGVal = genericToGVal
instance ToGVal m ActionDomain where toGVal = genericToGVal
instance ToGVal m AftermathVariant where toGVal = genericToGVal
instance ToGVal m TraumaVariant where toGVal = genericToGVal
instance ToGVal m BargainVariant where toGVal = genericToGVal
instance ToGVal m BargainOption where toGVal = genericToGVal
instance ToGVal m BargainCost where toGVal = genericToGVal
instance ToGVal m ClockInterrupt where toGVal = genericToGVal

-- Player
instance ToGVal m PlayerState where toGVal = genericToGVal
instance ToGVal m Trauma where toGVal = genericToGVal

-- Dice Mechanics
instance ToGVal m DicePool where toGVal = genericToGVal
instance ToGVal m Position where toGVal = genericToGVal
instance ToGVal m Effect where toGVal = genericToGVal
instance ToGVal m OutcomeTier where toGVal = genericToGVal
instance ToGVal m PendingOutcome where toGVal = genericToGVal

-- Factions
instance ToGVal m FactionId where toGVal = genericToGVal
instance ToGVal m Faction where toGVal = genericToGVal
instance ToGVal m Attitude where toGVal = genericToGVal
instance ToGVal m GoalId where toGVal = genericToGVal
instance ToGVal m Goal where toGVal = genericToGVal
instance ToGVal m GoalStatus where toGVal = genericToGVal
instance ToGVal m ResourcePool where toGVal = genericToGVal
instance ToGVal m Secret where toGVal = genericToGVal
instance ToGVal m Fact where toGVal = genericToGVal

-- NPCs
instance ToGVal m NpcId where toGVal = genericToGVal
instance ToGVal m Npc where toGVal = genericToGVal
instance ToGVal m Disposition where toGVal = genericToGVal
instance ToGVal m Want where toGVal = genericToGVal
instance ToGVal m Fear where toGVal = genericToGVal
instance ToGVal m Urgency where toGVal = genericToGVal
instance ToGVal m Severity where toGVal = genericToGVal

-- Clocks
instance ToGVal m ClockId where toGVal = genericToGVal
instance ToGVal m Clock where toGVal = genericToGVal
instance ToGVal m Trigger where toGVal = genericToGVal
instance ToGVal m ActionPattern where toGVal = genericToGVal
instance ToGVal m Consequence where toGVal = genericToGVal
instance ToGVal m FactionAction where toGVal = genericToGVal
instance ToGVal m Target where toGVal = genericToGVal
instance ToGVal m LocationDelta where toGVal = genericToGVal
instance ToGVal m Escalation where toGVal = genericToGVal

-- ══════════════════════════════════════════════════════════════
-- BETWEEN SCENES
-- ══════════════════════════════════════════════════════════════

-- | Options available during BetweenScenes phase
data BetweenScenesOption
  = BSLayLow
    -- ^ Reduce heat, but threat clocks tick
  | BSRecover
    -- ^ Spend coin to reduce stress
  | BSWorkGoal Text
    -- ^ Work toward a goal clock (name)
  | BSNewScene
    -- ^ Start a new scene
  | BSEndSession
    -- ^ Save and quit
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Summary of a clock for BetweenScenes display
data ClockSummary = ClockSummary
  { csName :: Text
  , csFilled :: Int
  , csSegments :: Int
  , csIsThreat :: Bool  -- True = threat, False = goal
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Context for the BetweenScenes UI
data BetweenScenesContext = BetweenScenesContext
  { bscClocks :: [ClockSummary]
    -- ^ All visible clocks with their current state
  , bscTransitionNarration :: Text
    -- ^ LLM-generated bridge text (~50 words)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Get display label for a BetweenScenes option
optionLabel :: BetweenScenesOption -> Text
optionLabel BSLayLow = "Lay Low"
optionLabel BSRecover = "Recover"
optionLabel (BSWorkGoal name) = "Work on: " <> name
optionLabel BSNewScene = "Start New Scene"
optionLabel BSEndSession = "Save & Quit"

instance ToGVal m BetweenScenesOption where toGVal = genericToGVal
instance ToGVal m ClockSummary where toGVal = genericToGVal
instance ToGVal m BetweenScenesContext where toGVal = genericToGVal

-- Locations
instance ToGVal m LocationId where toGVal = genericToGVal
instance ToGVal m Location where toGVal = genericToGVal

-- Narrative
instance ToGVal m RumorId where toGVal = genericToGVal
instance ToGVal m Rumor where toGVal = genericToGVal
instance ToGVal m RumorSource where toGVal = genericToGVal
instance ToGVal m TruthValue where toGVal = genericToGVal
instance ToGVal m SpreadLevel where toGVal = genericToGVal
instance ToGVal m ThreadId where toGVal = genericToGVal
instance ToGVal m Thread where toGVal = genericToGVal
instance ToGVal m Tension where toGVal = genericToGVal

-- Scenes
instance ToGVal m ActiveScene where toGVal = genericToGVal
instance ToGVal m SceneBeat where toGVal = genericToGVal
instance ToGVal m Stakes where toGVal = genericToGVal
instance ToGVal m Tone where toGVal = genericToGVal

-- Misc
instance ToGVal m Duration where toGVal = genericToGVal
instance ToGVal m Tag where toGVal = genericToGVal
