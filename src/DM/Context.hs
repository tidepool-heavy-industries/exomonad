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
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Foldable (toList)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Text.Ginger.GVal (ToGVal(..))
import Text.Ginger.GVal.Generic (genericToGVal)

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

-- | Build context for DM turn template from world state
-- Requires an active scene to be set
buildDMContext :: WorldState -> DMContext
buildDMContext world = case world.scene of
  Nothing -> error "buildDMContext: no active scene"
  Just activeScene ->
    let
      -- Get location from scene
      location = fromMaybe defaultLocation $
        HM.lookup activeScene.sceneLocation world.locations

      -- Enrich NPCs present in scene
      presentNpcs = map (enrichNpc world) activeScene.scenePresent

      -- Render scene beats as text
      sceneBeats = map renderBeat (toList activeScene.sceneBeats)

      -- Split clocks into visible and hidden
      allClocks = HM.elems world.clocks
      (visibleClocks, hiddenClocks) =
        foldr partitionClock ([], []) allClocks

      -- Get active threads (non-simmering)
      activeThreads = filter (\t -> t.threadTension /= Simmering) world.threads

      -- Get relevant rumors (spread beyond whisper)
      relevantRumors = filter (\r -> r.rumorSpread > Whisper) world.rumors

      -- Build faction summaries for factions in play
      factionsInPlay = map summarizeFaction (HM.elems world.factions)

      -- Calculate precarity (assume not hunted/recovering for now)
      precarity = calculatePrecarity world.player False False

      -- Build dice context
      diceCtx = buildDiceContext world

    in DMContext
      { ctxPlayer = world.player
      , ctxPrecarity = precarity
      , ctxDice = diceCtx
      , ctxLocation = location
      , ctxPresentNpcs = presentNpcs
      , ctxSceneBeats = sceneBeats
      , ctxStakes = let Stakes s = activeScene.sceneStakes in s
      , ctxVisibleClocks = visibleClocks
      , ctxHiddenClocks = hiddenClocks
      , ctxActiveThreads = activeThreads
      , ctxRelevantRumors = relevantRumors
      , ctxFactionsInPlay = factionsInPlay
      , ctxTone = world.tone
      , ctxSessionGoals = world.sessionGoals
      }
  where
    partitionClock clock (vis, hid)
      | clock.clockVisible = (clock : vis, hid)
      | otherwise = (vis, clock : hid)

    defaultLocation = Location
      { locationName = "Unknown"
      , locationDescription = "A mysterious place."
      , locationControlledBy = Nothing
      , locationFeatures = []
      }

-- | Build dice context from world state
buildDiceContext :: WorldState -> DiceContext
buildDiceContext world = DiceContext
  { dcPool = world.dicePool.poolDice
  , dcLockedOutcome = fmap pendingToLocked world.pendingOutcome
  }
  where
    pendingToLocked pending = case (pending.chosenDie, pending.chosenTier) of
      (Just die, Just tier) -> LockedOutcome
        { loContext = pending.outcomeContext
        , loPosition = pending.outcomePosition
        , loEffect = pending.outcomeEffect
        , loStakes = pending.outcomeStakes
        , loDieValue = die
        , loTier = tier
        }
      _ -> error "pendingToLocked: incomplete pending outcome"

-- | Render a scene beat as text for template display
renderBeat :: SceneBeat -> Text
renderBeat = \case
  PlayerAction action tags ->
    action <> if null tags then "" else " [" <> T.intercalate ", " (map (\(Tag t) -> t) tags) <> "]"
  NpcAction (NpcId npcId) action ->
    npcId <> ": " <> action
  EnvironmentShift desc ->
    "*" <> desc <> "*"
  Revelation secret ->
    "REVEALED: " <> secret.secretContent
  ClockTick (ClockId clockId) ticks ->
    "Clock '" <> clockId <> "' advanced by " <> T.pack (show ticks)

-- | Enrich an NPC with disposition label and current want
enrichNpc :: WorldState -> NpcId -> NpcWithDisposition
enrichNpc world npcId = case HM.lookup npcId world.npcs of
  Nothing -> NpcWithDisposition
    { nwdNpc = defaultNpc npcId
    , nwdDispositionLabel = "Unknown"
    , nwdCurrentWant = Nothing
    , nwdVoiceNotes = ""
    }
  Just npc -> NpcWithDisposition
    { nwdNpc = npc
    , nwdDispositionLabel = dispositionLabel npc.npcDisposition
    , nwdCurrentWant = mostUrgentWant npc.npcWants
    , nwdVoiceNotes = npc.npcVoiceNotes
    }
  where
    defaultNpc (NpcId name) = Npc
      { npcName = name
      , npcFaction = Nothing
      , npcDisposition = DispNeutral
      , npcWants = []
      , npcFears = []
      , npcKnows = []
      , npcLocation = Nothing
      , npcVoiceNotes = ""
      }

-- | Render disposition as human-readable label
dispositionLabel :: Disposition -> Text
dispositionLabel = \case
  DispHostile -> "Hostile"
  Suspicious -> "Suspicious"
  DispNeutral -> "Neutral"
  Friendly -> "Friendly"
  Loyal -> "Loyal"

-- | Find most urgent want (highest urgency first)
mostUrgentWant :: [Want] -> Maybe Text
mostUrgentWant wants = case sortOn (Down . urgencyOrd . (.wantUrgency)) wants of
  [] -> Nothing
  (w:_) -> Just w.wantDescription
  where
    urgencyOrd = \case
      Low -> 0 :: Int
      Medium -> 1
      High -> 2
      UrgencyDesperate -> 3

-- | Summarize a faction for template display
summarizeFaction :: Faction -> FactionSummary
summarizeFaction faction = FactionSummary
  { fsFactionName = faction.factionName
  , fsAttitudeLabel = attitudeLabel faction.factionAttitude
  , fsCurrentGoal = currentPursuingGoal faction.factionGoals
  }

-- | Render attitude as human-readable label
attitudeLabel :: Attitude -> Text
attitudeLabel = \case
  Hostile -> "Hostile"
  Wary -> "Wary"
  Neutral -> "Neutral"
  Favorable -> "Favorable"
  Allied -> "Allied"

-- | Find currently pursuing goal
currentPursuingGoal :: [Goal] -> Maybe Text
currentPursuingGoal goals = listToMaybe
  [g.goalDescription | g <- goals, g.goalStatus == Pursuing]

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

-- | Build context for scene compression template
buildCompressionContext :: ActiveScene -> WorldState -> CompressionContext
buildCompressionContext scene world = CompressionContext
  { ccSceneToCompress = scene
  , ccAllBeats = map renderBeat (toList scene.sceneBeats)
  , ccFactionStates = map summarizeFaction (HM.elems world.factions)
  , ccNpcStates = map (enrichNpc world) scene.scenePresent
  , ccActiveClocks = filter (not . clockComplete) (HM.elems world.clocks)
  , ccActiveThreads = world.threads
  }
  where
    clockComplete clock = clock.clockFilled >= clock.clockSegments

-- ══════════════════════════════════════════════════════════════
-- TOGVAL INSTANCES (for template rendering)
-- ══════════════════════════════════════════════════════════════

instance ToGVal m DMContext where toGVal = genericToGVal
instance ToGVal m DiceContext where toGVal = genericToGVal
instance ToGVal m LockedOutcome where toGVal = genericToGVal
instance ToGVal m Precarity where toGVal = genericToGVal
instance ToGVal m NpcWithDisposition where toGVal = genericToGVal
instance ToGVal m FactionSummary where toGVal = genericToGVal
instance ToGVal m CompressionContext where toGVal = genericToGVal
