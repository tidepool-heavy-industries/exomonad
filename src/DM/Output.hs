-- | DM Structured Output Types
module DM.Output
  ( -- * Turn Output
    TurnOutput(..)
  , emptyTurnOutput
  , applyTurnOutput
  
    -- * Clock Operations
  , ClockTick(..)
  , NewClock(..)
  
    -- * Thread Operations
  , NewThread(..)
  , ThreadResolution(..)
  
    -- * Faction Operations
  , AttitudeShift(..)
  , Direction(..)
  , Degree(..)
  
    -- * NPC Operations
  , DispositionShift(..)
  , NpcReveal(..)
  
    -- * Scene Control
  , SceneControl(..)
  
    -- * Rumor Operations
  , NewRumor(..)
  
    -- * Compression Output
  , CompressionOutput(..)
  , applyCompression
  , SceneOutcome(..)
  , PlayerChoice(..)
  , ChoiceWeight(..)
  , WorldDeltas(..)
  , FactionDelta(..)
  , Extractions(..)
  , Decay(..)
  ) where

import DM.State
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- TURN OUTPUT
-- ══════════════════════════════════════════════════════════════

data TurnOutput = TurnOutput
  { narration :: Text
  , clockTicks :: [ClockTick]
  , newClocks :: [NewClock]
  , revealClocks :: [ClockId]
  , newThreads :: [NewThread]
  , threadEscalations :: [(ThreadId, Tension, Text)]
  , resolvedThreads :: [(ThreadId, ThreadResolution)]
  , attitudeShifts :: [AttitudeShift]
  , factionLearns :: [(FactionId, Fact, Text)]
  , dispositionShifts :: [DispositionShift]
  , npcMoves :: [(NpcId, LocationId, Text)]
  , npcReveals :: [NpcReveal]
  , sceneControl :: SceneControl
  , spreadRumors :: [NewRumor]
  , confirmRumors :: [(RumorId, Text)]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

emptyTurnOutput :: TurnOutput
emptyTurnOutput = TurnOutput
  { narration = ""
  , clockTicks = []
  , newClocks = []
  , revealClocks = []
  , newThreads = []
  , threadEscalations = []
  , resolvedThreads = []
  , attitudeShifts = []
  , factionLearns = []
  , dispositionShifts = []
  , npcMoves = []
  , npcReveals = []
  , sceneControl = Continue
  , spreadRumors = []
  , confirmRumors = []
  }

-- ══════════════════════════════════════════════════════════════
-- OPERATION TYPES
-- ══════════════════════════════════════════════════════════════

data ClockTick = ClockTick
  { tickClock :: ClockId
  , tickSegments :: Int
  , tickBecause :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data NewClock = NewClock
  { newClockName :: Text
  , newClockSegments :: Int
  , newClockVisible :: Bool
  , newClockConsequence :: Text
  , newClockBecause :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data NewThread = NewThread
  { newThreadHook :: Text
  , newThreadTension :: Tension
  , newThreadInvolves :: [NpcId]
  , newThreadBecause :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ThreadResolution = ThreadResolution
  { resolutionOutcome :: Text
  , resolutionBecause :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AttitudeShift = AttitudeShift
  { shiftFaction :: FactionId
  , shiftDirection :: Direction
  , shiftDegree :: Degree
  , shiftBecause :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Direction = Toward | Away
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Degree = Slight | Notable | Major
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data DispositionShift = DispositionShift
  { dispShiftNpc :: NpcId
  , dispShiftDirection :: Direction
  , dispShiftDegree :: Degree
  , dispShiftBecause :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data NpcReveal = NpcReveal
  { revealNpc :: NpcId
  , revealSecret :: Text
  , revealBecause :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SceneControl
  = Continue
  | EndScene { endResolution :: Text }
  | ShiftLocation { shiftTo :: LocationId, shiftTransition :: Text }
  | TimeJump { jumpDuration :: Duration, jumpMontage :: Text }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data NewRumor = NewRumor
  { newRumorContent :: Text
  , newRumorTruth :: TruthValue
  , newRumorSpread :: SpreadLevel
  , newRumorBecause :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- APPLY TURN OUTPUT
-- ══════════════════════════════════════════════════════════════

applyTurnOutput :: TurnOutput -> WorldState -> WorldState
applyTurnOutput output state = error "TODO: applyTurnOutput - apply all mutations from TurnOutput to WorldState"
  -- Should compose:
  -- applyClockTicks (clockTicks output)
  -- applyNewClocks (newClocks output)
  -- applyRevealClocks (revealClocks output)
  -- applyNewThreads (newThreads output)
  -- applyThreadEscalations (threadEscalations output)
  -- applyResolvedThreads (resolvedThreads output)
  -- applyAttitudeShifts (attitudeShifts output)
  -- applyFactionLearns (factionLearns output)
  -- applyDispositionShifts (dispositionShifts output)
  -- applyNpcMoves (npcMoves output)
  -- applyNpcReveals (npcReveals output)
  -- applySceneControl (sceneControl output)
  -- applyNewRumors (spreadRumors output)
  -- applyConfirmRumors (confirmRumors output)

-- ══════════════════════════════════════════════════════════════
-- COMPRESSION OUTPUT
-- ══════════════════════════════════════════════════════════════

data CompressionOutput = CompressionOutput
  { sceneOutcome :: SceneOutcome
  , worldDeltas :: WorldDeltas
  , extracted :: Extractions
  , decay :: Decay
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SceneOutcome = SceneOutcome
  { outcomeSummary :: Text
  , outcomeKeyBeats :: [SceneBeat]
  , outcomePlayerChoices :: [PlayerChoice]
  , outcomeConsequenceSeeds :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PlayerChoice = PlayerChoice
  { choiceDescription :: Text
  , choiceAlternatives :: [Text]
  , choiceWeight :: ChoiceWeight
  , choiceBecause :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ChoiceWeight = Trivial | Meaningful | Pivotal
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data WorldDeltas = WorldDeltas
  { factionDeltas :: [FactionDelta]
  , npcMemories :: [(NpcId, Text)]
  , locationChanges :: [(LocationId, Text)]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data FactionDelta = FactionDelta
  { deltaFaction :: FactionId
  , deltaAttitudeChange :: Maybe (Attitude, Attitude, Text)
  , deltaGoalProgress :: [(GoalId, GoalStatus)]
  , deltaNewSecrets :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Extractions = Extractions
  { extractedThreads :: [NewThread]
  , extractedRumors :: [NewRumor]
  , extractedPromises :: [(NpcId, Text)]
  , extractedDebts :: [(NpcId, Text)]
  , extractedInsults :: [(FactionId, Text)]
  , extractedFavors :: [(NpcId, Text)]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Decay = Decay
  { decayRumors :: [RumorId]
  , decayThreads :: [ThreadId]
  , decayTensionReductions :: [(ThreadId, Tension)]
  , decayClocks :: [(ClockId, Int)]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

applyCompression :: CompressionOutput -> WorldState -> WorldState
applyCompression output state = error "TODO: applyCompression - apply world deltas, extractions, decay, record outcome"
