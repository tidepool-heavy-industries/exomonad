-- | DM Structured Output Types
module DM.Output
  ( -- * Turn Output
    TurnOutput(..)
  , emptyTurnOutput
  , applyTurnOutput

    -- * Player Resource Deltas
  , PlayerDeltas(..)
  , emptyPlayerDeltas

    -- * Dice Mechanics
  , RequestOutcomes(..)

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
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- TURN OUTPUT
-- ══════════════════════════════════════════════════════════════

data TurnOutput = TurnOutput
  { narration :: Text
  -- Player resource changes (delta fields - LLM says +2 stress, not "set to 5")
  , playerDeltas :: PlayerDeltas
  , newTrauma :: Maybe Trauma        -- Only when stress would exceed 9
  -- Dice mechanics
  , requestOutcomes :: Maybe RequestOutcomes  -- Ask player to choose die
  , clearPendingOutcome :: Bool               -- Clear after narrating outcome
  -- World state changes
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

-- | Delta fields for player resources
-- LLM writes "+2" to stress, engine applies it with validation
data PlayerDeltas = PlayerDeltas
  { stressDelta :: Int    -- Can be negative (recovery) or positive
  , coinDelta :: Int      -- Spending or earning
  , heatDelta :: Int      -- Attention from authorities
  , wantedDelta :: Int    -- Escalation level
  , deltaBecause :: Text  -- Why these changes happened
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

emptyPlayerDeltas :: PlayerDeltas
emptyPlayerDeltas = PlayerDeltas 0 0 0 0 ""

-- | Request for player to select a die from their pool
-- This is the core "see your decision space" mechanic
data RequestOutcomes = RequestOutcomes
  { requestContext :: Text    -- What the player is trying to do
  , requestPosition :: Position  -- Controlled/Risky/Desperate
  , requestEffect :: Effect      -- Limited/Standard/Great
  , requestStakes :: Text        -- What happens on failure
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

emptyTurnOutput :: TurnOutput
emptyTurnOutput = TurnOutput
  { narration = ""
  , playerDeltas = emptyPlayerDeltas
  , newTrauma = Nothing
  , requestOutcomes = Nothing
  , clearPendingOutcome = False
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

-- | Apply all mutations from a turn output to the world state
applyTurnOutput :: TurnOutput -> WorldState -> WorldState
applyTurnOutput output =
  -- Compose all appliers in order
    applyPlayerDeltas output.playerDeltas
  . applyNewTrauma output.newTrauma
  . applyRequestOutcomes output.requestOutcomes
  . applyClearPendingOutcome output.clearPendingOutcome
  . applyClockTicks output.clockTicks
  . applyNewClocks output.newClocks
  . applyRevealClocks output.revealClocks
  . applyNewThreads output.newThreads
  . applyThreadEscalations output.threadEscalations
  . applyResolvedThreads output.resolvedThreads
  . applyAttitudeShifts output.attitudeShifts
  . applyFactionLearns output.factionLearns
  . applyDispositionShifts output.dispositionShifts
  . applyNpcMoves output.npcMoves
  . applyNpcReveals output.npcReveals
  . applySceneControl output.sceneControl
  . applyNewRumors output.spreadRumors
  . applyConfirmRumors output.confirmRumors

-- | Apply player resource deltas with bounds checking
applyPlayerDeltas :: PlayerDeltas -> WorldState -> WorldState
applyPlayerDeltas deltas state = state
  { player = (state.player)
      { stress = clamp 0 9 (state.player.stress + deltas.stressDelta)
      , coin = max 0 (state.player.coin + deltas.coinDelta)
      , heat = clamp 0 10 (state.player.heat + deltas.heatDelta)
      , wanted = clamp 0 4 (state.player.wanted + deltas.wantedDelta)
      }
  }
  where
    clamp lo hi x = max lo (min hi x)

-- | Apply new trauma if present
applyNewTrauma :: Maybe Trauma -> WorldState -> WorldState
applyNewTrauma Nothing state = state
applyNewTrauma (Just trauma) state = state
  { player = (state.player)
      { trauma = trauma : state.player.trauma
      , stress = 0  -- Reset stress after trauma
      }
  }

-- | Set up pending outcome for dice selection
applyRequestOutcomes :: Maybe RequestOutcomes -> WorldState -> WorldState
applyRequestOutcomes Nothing state = state
applyRequestOutcomes (Just req) state = state
  { pendingOutcome = Just PendingOutcome
      { outcomeContext = req.requestContext
      , outcomePosition = req.requestPosition
      , outcomeEffect = req.requestEffect
      , outcomeStakes = req.requestStakes
      , chosenDie = Nothing
      , chosenTier = Nothing
      }
  }

-- | Clear pending outcome after it's been narrated
applyClearPendingOutcome :: Bool -> WorldState -> WorldState
applyClearPendingOutcome False state = state
applyClearPendingOutcome True state = state { pendingOutcome = Nothing }

-- | Advance clocks by specified segments
applyClockTicks :: [ClockTick] -> WorldState -> WorldState
applyClockTicks ticks state = foldr applyTick state ticks
  where
    applyTick tick s = s
      { clocks = HM.adjust advanceClock tick.tickClock s.clocks }
    advanceClock clock = clock
      { clockFilled = min clock.clockSegments (clock.clockFilled + 1) }

-- | Create new clocks
applyNewClocks :: [NewClock] -> WorldState -> WorldState
applyNewClocks newClks state = foldr addClock state newClks
  where
    addClock nc s =
      let clockId = ClockId (T.toLower $ T.replace " " "-" nc.newClockName)
          clock = Clock
            { clockName = nc.newClockName
            , clockSegments = nc.newClockSegments
            , clockFilled = 0
            , clockVisible = nc.newClockVisible
            , clockConsequence = Escalate $ Escalation nc.newClockConsequence Moderate
            , clockTriggers = []
            }
      in s { clocks = HM.insert clockId clock s.clocks }

-- | Reveal hidden clocks
applyRevealClocks :: [ClockId] -> WorldState -> WorldState
applyRevealClocks ids state = foldr reveal state ids
  where
    reveal clockId s = s
      { clocks = HM.adjust (\c -> c { clockVisible = True }) clockId s.clocks }

-- | Create new narrative threads
applyNewThreads :: [NewThread] -> WorldState -> WorldState
applyNewThreads newThreads' state = foldr addThread state newThreads'
  where
    addThread nt s =
      let threadId = ThreadId (T.toLower $ T.replace " " "-" nt.newThreadHook)
          thread = Thread
            { threadId = threadId
            , threadHook = nt.newThreadHook
            , threadInvolves = map Right nt.newThreadInvolves
            , threadTension = nt.newThreadTension
            , threadDeadline = Nothing
            }
      in s { threads = thread : s.threads }

-- | Escalate thread tensions
applyThreadEscalations :: [(ThreadId, Tension, Text)] -> WorldState -> WorldState
applyThreadEscalations escalations state = state
  { threads = map escalate state.threads }
  where
    escalateMap = HM.fromList [(tid, t) | (tid, t, _) <- escalations]
    escalate thread = case HM.lookup thread.threadId escalateMap of
      Nothing -> thread
      Just newTension -> thread { threadTension = newTension }

-- | Resolve (remove) completed threads
applyResolvedThreads :: [(ThreadId, ThreadResolution)] -> WorldState -> WorldState
applyResolvedThreads resolved state = state
  { threads = filter (not . isResolved) state.threads }
  where
    resolvedIds = map fst resolved
    isResolved thread = thread.threadId `elem` resolvedIds

-- | Shift faction attitudes
applyAttitudeShifts :: [AttitudeShift] -> WorldState -> WorldState
applyAttitudeShifts shifts state = foldr applyShift state shifts
  where
    applyShift shift s = s
      { factions = HM.adjust (shiftAttitude shift) shift.shiftFaction s.factions }
    shiftAttitude shift faction = faction
      { factionAttitude = adjustAttitude shift.shiftDirection shift.shiftDegree faction.factionAttitude }
    adjustAttitude dir degree att =
      let steps = case degree of
            Slight -> 1
            Notable -> 2
            Major -> 3
          newOrd = case dir of
            Toward -> fromEnum att + steps
            Away -> fromEnum att - steps
      in toEnum (max 0 (min 4 newOrd))

-- | Add facts to faction knowledge
applyFactionLearns :: [(FactionId, Fact, Text)] -> WorldState -> WorldState
applyFactionLearns learns state = foldr applyLearn state learns
  where
    applyLearn (fid, theFact, _) s = s
      { factions = HM.adjust (addFact theFact) fid s.factions }
    addFact theFact faction = faction
      { factionKnownFacts = theFact : faction.factionKnownFacts }

-- | Shift NPC dispositions
applyDispositionShifts :: [DispositionShift] -> WorldState -> WorldState
applyDispositionShifts shifts state = foldr applyShift state shifts
  where
    applyShift shift s = s
      { npcs = HM.adjust (shiftDisp shift) shift.dispShiftNpc s.npcs }
    shiftDisp shift npc = npc
      { npcDisposition = adjustDisposition shift.dispShiftDirection shift.dispShiftDegree npc.npcDisposition }
    adjustDisposition dir degree disp =
      let steps = case degree of
            Slight -> 1
            Notable -> 2
            Major -> 3
          newOrd = case dir of
            Toward -> fromEnum disp + steps
            Away -> fromEnum disp - steps
      in toEnum (max 0 (min 4 newOrd))

-- | Move NPCs to new locations
applyNpcMoves :: [(NpcId, LocationId, Text)] -> WorldState -> WorldState
applyNpcMoves moves state = foldr applyMove state moves
  where
    applyMove (npcId, locId, _) s = s
      { npcs = HM.adjust (\n -> n { npcLocation = Just locId }) npcId s.npcs }

-- | Record NPC secret reveals
applyNpcReveals :: [NpcReveal] -> WorldState -> WorldState
applyNpcReveals reveals state = foldr applyReveal state reveals
  where
    applyReveal rev s = s
      { npcs = HM.adjust (addSecret rev) rev.revealNpc s.npcs }
    addSecret rev npc =
      let secret = Secret { secretContent = rev.revealSecret, secretKnownBy = [] }
      in npc { npcKnows = secret : npc.npcKnows }

-- | Handle scene control directives
applySceneControl :: SceneControl -> WorldState -> WorldState
applySceneControl ctrl state = case ctrl of
  Continue -> state
  EndScene _ -> state { scene = Nothing }
  ShiftLocation locId _ -> case state.scene of
    Nothing -> state
    Just s -> state { scene = Just s { sceneLocation = locId } }
  TimeJump _ _ -> state  -- Time jumps don't directly modify state

-- | Create new rumors
applyNewRumors :: [NewRumor] -> WorldState -> WorldState
applyNewRumors newRumors state = foldr addRumor state newRumors
  where
    addRumor nr s =
      let rumorId = RumorId (T.toLower $ T.take 20 $ T.replace " " "-" nr.newRumorContent)
          rumor = Rumor
            { rumorId = rumorId
            , rumorContent = nr.newRumorContent
            , rumorSource = PlayerStarted
            , rumorTruthValue = nr.newRumorTruth
            , rumorSpread = nr.newRumorSpread
            }
      in s { rumors = rumor : s.rumors }

-- | Confirm rumor truth values
applyConfirmRumors :: [(RumorId, Text)] -> WorldState -> WorldState
applyConfirmRumors confirms state = state
  { rumors = map confirm state.rumors }
  where
    confirmMap = HM.fromList confirms
    confirm rumor = case HM.lookup rumor.rumorId confirmMap of
      Nothing -> rumor
      Just _ -> rumor { rumorTruthValue = TrueRumor }

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

-- | Apply compression output to world state
-- This is called after a scene ends to persist durable changes
applyCompression :: CompressionOutput -> WorldState -> WorldState
applyCompression output =
    applyWorldDeltas output.worldDeltas
  . applyExtractions output.extracted
  . applyDecay output.decay
  . recordSceneOutcome output.sceneOutcome

-- | Apply world deltas from compression
applyWorldDeltas :: WorldDeltas -> WorldState -> WorldState
applyWorldDeltas deltas state =
    applyFactionDeltas deltas.factionDeltas
  . applyNpcMemories deltas.npcMemories
  . applyLocationChanges deltas.locationChanges
  $ state

-- | Apply faction-level changes
applyFactionDeltas :: [FactionDelta] -> WorldState -> WorldState
applyFactionDeltas fdeltas state = foldr applyFDelta state fdeltas
  where
    applyFDelta fd s = s
      { factions = HM.adjust (updateFaction fd) fd.deltaFaction s.factions }
    updateFaction fd faction = faction
      { factionAttitude = case fd.deltaAttitudeChange of
          Nothing -> faction.factionAttitude
          Just (_, newAtt, _) -> newAtt
      , factionGoals = updateGoals fd.deltaGoalProgress faction.factionGoals
      , factionSecrets = map (\t -> Secret t []) fd.deltaNewSecrets ++ faction.factionSecrets
      }
    updateGoals progresses goals = map (updateGoal progresses) goals
    updateGoal progresses goal = case lookup goal.goalId progresses of
      Nothing -> goal
      Just newStatus -> goal { goalStatus = newStatus }

-- | Record NPC memories (stored as facts they know)
applyNpcMemories :: [(NpcId, Text)] -> WorldState -> WorldState
applyNpcMemories memories state = foldr applyMemory state memories
  where
    applyMemory (npcId, theMemory) s = s
      { npcs = HM.adjust (addMemory theMemory) npcId s.npcs }
    addMemory theMemory npc =
      let secret = Secret { secretContent = theMemory, secretKnownBy = [] }
      in npc { npcKnows = secret : npc.npcKnows }

-- | Apply location description changes
applyLocationChanges :: [(LocationId, Text)] -> WorldState -> WorldState
applyLocationChanges changes state = foldr applyChange state changes
  where
    applyChange (locId, newDesc) s = s
      { locations = HM.adjust (\l -> l { locationDescription = newDesc }) locId s.locations }

-- | Apply extractions (new threads, rumors, etc.)
applyExtractions :: Extractions -> WorldState -> WorldState
applyExtractions ext =
    applyNewThreads ext.extractedThreads
  . applyNewRumors ext.extractedRumors
  -- Promises, debts, insults, favors stored as NPC knowledge
  . applyPromises ext.extractedPromises
  . applyDebts ext.extractedDebts
  . applyInsults ext.extractedInsults
  . applyFavors ext.extractedFavors

-- | Store promises as NPC knowledge
applyPromises :: [(NpcId, Text)] -> WorldState -> WorldState
applyPromises promises state = foldr addPromise state promises
  where
    addPromise (npcId, promise) s = s
      { npcs = HM.adjust (addKnowledge ("PROMISE: " <> promise)) npcId s.npcs }
    addKnowledge content npc =
      let secret = Secret { secretContent = content, secretKnownBy = [] }
      in npc { npcKnows = secret : npc.npcKnows }

-- | Store debts as NPC knowledge
applyDebts :: [(NpcId, Text)] -> WorldState -> WorldState
applyDebts debts state = foldr addDebt state debts
  where
    addDebt (npcId, debt) s = s
      { npcs = HM.adjust (addKnowledge ("DEBT: " <> debt)) npcId s.npcs }
    addKnowledge content npc =
      let secret = Secret { secretContent = content, secretKnownBy = [] }
      in npc { npcKnows = secret : npc.npcKnows }

-- | Store insults as faction knowledge
applyInsults :: [(FactionId, Text)] -> WorldState -> WorldState
applyInsults insults state = foldr addInsult state insults
  where
    addInsult (fid, insult) s = s
      { factions = HM.adjust (addFact ("INSULT: " <> insult)) fid s.factions }
    addFact content faction = faction
      { factionKnownFacts = Fact content "scene" : faction.factionKnownFacts }

-- | Store favors as NPC knowledge
applyFavors :: [(NpcId, Text)] -> WorldState -> WorldState
applyFavors favors state = foldr addFavor state favors
  where
    addFavor (npcId, favor) s = s
      { npcs = HM.adjust (addKnowledge ("FAVOR: " <> favor)) npcId s.npcs }
    addKnowledge content npc =
      let secret = Secret { secretContent = content, secretKnownBy = [] }
      in npc { npcKnows = secret : npc.npcKnows }

-- | Apply decay (remove/reduce stale elements)
applyDecay :: Decay -> WorldState -> WorldState
applyDecay decay state = state
  { rumors = filter (not . isDecayed) state.rumors
  , threads = filter (not . isDecayedThread) state.threads
  , clocks = foldr decayClock state.clocks decay.decayClocks
  }
  where
    isDecayed rumor = rumor.rumorId `elem` decay.decayRumors
    isDecayedThread thread = thread.threadId `elem` decay.decayThreads
    decayClock (clockId, amount) clks =
      HM.adjust (\c -> c { clockFilled = max 0 (c.clockFilled - amount) }) clockId clks

-- | Record scene outcome in session history
recordSceneOutcome :: SceneOutcome -> WorldState -> WorldState
recordSceneOutcome outcome state = state
  { sessionHistory = state.sessionHistory Seq.|> SceneSummary
      { summaryText = outcome.outcomeSummary
      , summaryKeyBeats = outcome.outcomeKeyBeats
      }
  }
