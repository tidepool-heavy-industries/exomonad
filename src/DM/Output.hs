-- | DM Structured Output Types (Simplified for API grammar limits)
module DM.Output
  ( -- * Turn Output (Simplified)
    TurnOutput(..)
  , NarrativeConnector(..)
  , emptyTurnOutput
  , applyTurnOutput

    -- * Legacy types for compression (TODO: simplify these too)
  , RequestOutcomes(..)
  , NewThread(..)
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
-- TURN OUTPUT (Simplified for API grammar limits)
-- ══════════════════════════════════════════════════════════════

-- | Narrative connector - forces causal thinking
-- "And then" is lazy; these connectors demand causality
data NarrativeConnector
  = Therefore   -- This happened because of what came before
  | But         -- This happened despite what came before
  | Meanwhile   -- Parallel action (use sparingly)
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Simplified turn output that fits within API grammar limits
-- NPC speech happens via SpeakAsNPC tool invocation, not here
data TurnOutput = TurnOutput
  { narration :: Text                       -- Narrative prose for this turn
  , narrativeConnector :: Maybe NarrativeConnector  -- How this connects to previous beat
  , stressDelta :: Int                      -- Change in stress (-9 to +9)
  , coinDelta :: Int                        -- Change in coin
  , continueScene :: Bool                   -- True to continue, False to end scene
  , costDescription :: Maybe Text           -- If costly/setback, describe the cost for echoing
  , threatDescription :: Maybe Text         -- If unresolved threat, describe for echoing
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

emptyTurnOutput :: TurnOutput
emptyTurnOutput = TurnOutput
  { narration = ""
  , narrativeConnector = Nothing
  , stressDelta = 0
  , coinDelta = 0
  , continueScene = True
  , costDescription = Nothing
  , threatDescription = Nothing
  }

-- | Legacy type kept for compression (TODO: simplify)
data RequestOutcomes = RequestOutcomes
  { requestContext :: Text
  , requestPosition :: Position
  , requestEffect :: Effect
  , requestStakes :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

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

-- | Apply turn output to world state (simplified version)
-- Also tracks costs and threats for consequence echoing
applyTurnOutput :: TurnOutput -> WorldState -> WorldState
applyTurnOutput output state = state
  { player = (state.player)
      { stress = clamp 0 9 (state.player.stress + output.stressDelta)
      , coin = max 0 (state.player.coin + output.coinDelta)
      }
  , scene = if output.continueScene then state.scene else Nothing
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
