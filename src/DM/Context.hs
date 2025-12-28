{-# LANGUAGE RecordWildCards #-}
-- | DM Template Context
module DM.Context
  ( -- * Turn Context
    DMContext(..)
  , buildDMContext

    -- * Mood Variant Context
  , MoodVariantContext(..)
  , buildMoodContext
  , emptyVariantContext

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
  -- Mood state machine - what phase of the loop we're in
  , ctxMood :: DMMood
  , ctxMoodLabel :: Text           -- Human-readable: "scene", "action", "aftermath", "downtime", "trauma"
  , ctxMoodVariant :: MoodVariantContext  -- Rich variant data for templates
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
  -- Consequence echoing (narrative continuity)
  , ctxRecentCosts :: [Text]            -- From last 3 costly/setback outcomes
  , ctxUnresolvedThreats :: [Text]      -- Complications not yet addressed
  , ctxPendingInterrupt :: Maybe ClockInterrupt  -- Clock triggered, awaiting handling
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Rich context for the current mood variant
-- This is what templates use to adapt their rendering
data MoodVariantContext = MoodVariantContext
  { -- Scene variants (Encounter, Opportunity, Discovery)
    mvcSceneType :: Maybe Text           -- "encounter", "opportunity", "discovery"
  , mvcSource :: Maybe Text              -- Encounter: who/what demands attention
  , mvcSceneUrgency :: Maybe Text        -- "low", "medium", "high", "critical"
  , mvcEscapable :: Maybe Bool           -- Can they walk away?
  , mvcOfferedBy :: Maybe Text           -- Opportunity: who's offering
  , mvcNature :: Maybe Text              -- Opportunity: what kind (job, intel, etc.)
  , mvcCatch :: Maybe Text               -- Opportunity: the catch
  , mvcWhat :: Maybe Text                -- Discovery: what was found
  , mvcImplications :: Maybe [Text]      -- Discovery: what it means
  -- Action variants
  , mvcPosition :: Maybe Text            -- "controlled", "risky", "desperate"
  , mvcThreat :: Maybe Text              -- What could go wrong
  , mvcOpportunity :: Maybe Text         -- What could be gained
  , mvcDomain :: Maybe Text              -- "infiltration", "social", "violence", "pursuit", "arcane"
  , mvcAdvantageSource :: Maybe Text     -- For controlled: why they have advantage
  , mvcWhyDesperate :: Maybe Text        -- For desperate: why it's so bad
  , mvcPotentialTrauma :: Maybe Bool     -- For desperate: could this break them?
  -- Aftermath variants
  , mvcOutcomeType :: Maybe Text         -- "clean", "costly", "setback", "disaster"
  , mvcWhatAchieved :: Maybe Text        -- What was accomplished
  , mvcWhatWentWrong :: Maybe Text       -- For setback/disaster
  , mvcCostsPaid :: Maybe [Text]         -- For costly outcomes
  , mvcNewComplications :: Maybe [Text]  -- Problems introduced
  , mvcImmediateDanger :: Maybe Bool     -- Is there active threat?
  , mvcEscapeRoute :: Maybe Text         -- Way out (for setback)
  -- Downtime variants
  , mvcDowntimeType :: Maybe Text        -- "recovery", "project", "entanglement"
  , mvcActivities :: Maybe [Text]        -- Recovery: available activities
  , mvcTimeAvailable :: Maybe Text       -- Recovery: how much time
  , mvcProjectName :: Maybe Text         -- Project: what they're working on
  , mvcProgress :: Maybe Int             -- Project: current progress
  , mvcRequired :: Maybe Int             -- Project: total needed
  , mvcEntanglementType :: Maybe Text    -- Entanglement: what kind
  , mvcEscapeOptions :: Maybe [Text]     -- Entanglement: ways out
  -- Trauma variants
  , mvcTraumaType :: Maybe Text
  , mvcWhatBroke :: Maybe Text
  , mvcAdrenaline :: Maybe Bool     -- Can push through for one more action?
  -- Bargain variants (out of dice)
  , mvcBargainContext :: Maybe Text      -- "the desperate chase", "holding the door"
  , mvcCanRetreat :: Maybe Bool          -- Can they "go home to rest"?
  , mvcRetreatDesc :: Maybe Text         -- "slip away to your garret"
  , mvcPassOutDesc :: Maybe Text         -- "collapse behind the coal bins"
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

-- | Build context for DM turn template from active scene and mood
--
-- The scene and mood are passed explicitly (from PhasePlaying pattern match).
-- This eliminates the need for Maybe checks - callers must have a valid scene.
buildDMContext :: ActiveScene -> DMMood -> WorldState -> DMContext
buildDMContext activeScene mood world =
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

      -- Build mood context (from passed mood, not world.mood)
      (moodLabel, moodVariant) = buildMoodContext mood

    in DMContext
      { ctxPlayer = world.player
      , ctxPrecarity = precarity
      , ctxMood = mood
      , ctxMoodLabel = moodLabel
      , ctxMoodVariant = moodVariant
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
      -- Consequence echoing
      , ctxRecentCosts = world.recentCosts
      , ctxUnresolvedThreats = world.unresolvedThreats
      , ctxPendingInterrupt = world.pendingInterrupt
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

-- | Build mood context from DMMood
-- Returns (human label, rich variant context)
buildMoodContext :: DMMood -> (Text, MoodVariantContext)
buildMoodContext mood = case mood of
  MoodScene variant -> ("scene", sceneVariantContext variant)
  MoodAction variant domain -> ("action", actionVariantContext variant domain)
  MoodAftermath variant -> ("aftermath", aftermathVariantContext variant)
  MoodDowntime variant -> ("downtime", downtimeVariantContext variant)
  MoodTrauma variant -> ("trauma", traumaVariantContext variant)
  MoodBargain variant -> ("bargain", bargainVariantContext variant)

-- | Empty variant context (all Nothing)
emptyVariantContext :: MoodVariantContext
emptyVariantContext = MoodVariantContext
  { -- Scene
    mvcSceneType = Nothing
  , mvcSource = Nothing
  , mvcSceneUrgency = Nothing
  , mvcEscapable = Nothing
  , mvcOfferedBy = Nothing
  , mvcNature = Nothing
  , mvcCatch = Nothing
  , mvcWhat = Nothing
  , mvcImplications = Nothing
  -- Action
  , mvcPosition = Nothing
  , mvcThreat = Nothing
  , mvcOpportunity = Nothing
  , mvcDomain = Nothing
  , mvcAdvantageSource = Nothing
  , mvcWhyDesperate = Nothing
  , mvcPotentialTrauma = Nothing
  -- Aftermath
  , mvcOutcomeType = Nothing
  , mvcWhatAchieved = Nothing
  , mvcWhatWentWrong = Nothing
  , mvcCostsPaid = Nothing
  , mvcNewComplications = Nothing
  , mvcImmediateDanger = Nothing
  , mvcEscapeRoute = Nothing
  -- Downtime
  , mvcDowntimeType = Nothing
  , mvcActivities = Nothing
  , mvcTimeAvailable = Nothing
  , mvcProjectName = Nothing
  , mvcProgress = Nothing
  , mvcRequired = Nothing
  , mvcEntanglementType = Nothing
  , mvcEscapeOptions = Nothing
  -- Trauma
  , mvcTraumaType = Nothing
  , mvcWhatBroke = Nothing
  , mvcAdrenaline = Nothing
  -- Bargain
  , mvcBargainContext = Nothing
  , mvcCanRetreat = Nothing
  , mvcRetreatDesc = Nothing
  , mvcPassOutDesc = Nothing
  }

-- | Extract scene variant context
sceneVariantContext :: SceneVariant -> MoodVariantContext
sceneVariantContext = \case
  Encounter{..} -> emptyVariantContext
    { mvcSceneType = Just "encounter"
    , mvcSource = Just svSource
    , mvcSceneUrgency = Just (urgencyLabel svUrgency)
    , mvcEscapable = Just svEscapable
    }
  Opportunity{..} -> emptyVariantContext
    { mvcSceneType = Just "opportunity"
    , mvcOfferedBy = fmap (\(NpcId nid) -> nid) svOfferedBy
    , mvcNature = Just svNature
    , mvcCatch = Just svCatch
    }
  Discovery{..} -> emptyVariantContext
    { mvcSceneType = Just "discovery"
    , mvcWhat = Just svWhat
    , mvcImplications = Just svImplications
    }
  where
    urgencyLabel = \case
      UrgencyLow -> "low"
      UrgencyMedium -> "medium"
      UrgencyHigh -> "high"
      UrgencyCritical -> "critical"

-- | Extract action variant context
actionVariantContext :: ActionVariant -> Maybe ActionDomain -> MoodVariantContext
actionVariantContext variant domain = base
  { mvcDomain = fmap domainLabel domain
  }
  where
    base = case variant of
      AvControlled{..} -> emptyVariantContext
        { mvcPosition = Just "controlled"
        , mvcAdvantageSource = Just avAdvantageSource
        , mvcOpportunity = Just avAdvantageSource  -- Controlled: advantage IS the opportunity
        , mvcThreat = Just avRiskIfFails
        }
      AvRisky{..} -> emptyVariantContext
        { mvcPosition = Just "risky"
        , mvcThreat = Just avThreat
        , mvcOpportunity = Just avOpportunity
        }
      AvDesperate{..} -> emptyVariantContext
        { mvcPosition = Just "desperate"
        , mvcWhyDesperate = Just avWhyDesperate
        , mvcThreat = Just avWhyDesperate  -- Desperate: the desperation IS the threat
        , mvcOpportunity = Just avStakes
        , mvcPotentialTrauma = Just avPotentialTrauma
        }

    domainLabel = \case
      DomainInfiltration -> "infiltration"
      DomainSocial -> "social"
      DomainViolence -> "violence"
      DomainPursuit -> "pursuit"
      DomainArcane -> "arcane"

-- | Extract aftermath variant context
aftermathVariantContext :: AftermathVariant -> MoodVariantContext
aftermathVariantContext = \case
  AmClean{..} -> emptyVariantContext
    { mvcOutcomeType = Just "clean"
    , mvcWhatAchieved = Just amWhatAchieved
    }
  AmCostly{..} -> emptyVariantContext
    { mvcOutcomeType = Just "costly"
    , mvcWhatAchieved = Just amWhatAchieved
    , mvcCostsPaid = Just amCostsPaid
    , mvcNewComplications = Just amNewComplications
    }
  AmSetback{..} -> emptyVariantContext
    { mvcOutcomeType = Just "setback"
    , mvcWhatWentWrong = Just amWhatWentWrong
    , mvcImmediateDanger = Just amImmediateDanger
    , mvcEscapeRoute = Just amEscapeRoute
    }
  AmDisaster{..} -> emptyVariantContext
    { mvcOutcomeType = Just "disaster"
    , mvcWhatWentWrong = Just amCatastrophe
    , mvcImmediateDanger = Just True
    , mvcPotentialTrauma = Just amTraumaRisk
    }

-- | Extract downtime variant context
downtimeVariantContext :: DowntimeVariant -> MoodVariantContext
downtimeVariantContext = \case
  Recovery{..} -> emptyVariantContext
    { mvcDowntimeType = Just "recovery"
    , mvcActivities = Just dvActivities
    , mvcTimeAvailable = Just dvTimeAvailable
    }
  Project{..} -> emptyVariantContext
    { mvcDowntimeType = Just "project"
    , mvcProjectName = Just dvProjectName
    , mvcProgress = Just dvProgress
    , mvcRequired = Just dvRequired
    }
  Entanglement{..} -> emptyVariantContext
    { mvcDowntimeType = Just "entanglement"
    , mvcEntanglementType = Just dvEntanglementType
    , mvcEscapeOptions = Just dvEscapeOptions
    }

-- | Extract trauma variant context
traumaVariantContext :: TraumaVariant -> MoodVariantContext
traumaVariantContext Breaking{..} = emptyVariantContext
  { mvcTraumaType = Just (T.pack $ show tvTraumaType)
  , mvcWhatBroke = Just tvWhatBroke
  , mvcAdrenaline = Just tvAdrenaline
  }

-- | Extract bargain variant context
bargainVariantContext :: BargainVariant -> MoodVariantContext
bargainVariantContext Bargaining{..} = emptyVariantContext
  { mvcBargainContext = Just bvWhatDrained
  , mvcCanRetreat = Just bvCanRetreat
  , mvcRetreatDesc = Just bvRetreatDesc
  , mvcPassOutDesc = Just bvPassOutDesc
  }

-- | Render a scene beat as text for template display
renderBeat :: SceneBeat -> Text
renderBeat = \case
  PlayerAction action tags ->
    "PLAYER: " <> action <> if null tags then "" else " [" <> T.intercalate ", " (map (\(Tag t) -> t) tags) <> "]"
  NpcAction (NpcId npcId) action ->
    npcId <> ": " <> action
  DMNarration narration ->
    "DM: " <> narration
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
instance ToGVal m MoodVariantContext where toGVal = genericToGVal
instance ToGVal m NpcWithDisposition where toGVal = genericToGVal
instance ToGVal m FactionSummary where toGVal = genericToGVal
instance ToGVal m CompressionContext where toGVal = genericToGVal
