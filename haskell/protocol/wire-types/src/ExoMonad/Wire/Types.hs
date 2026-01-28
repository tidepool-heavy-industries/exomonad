{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Wire format types for exomonad native GUI.
--
-- Defines UIState (server → client) and UserAction (client → server).
-- See PROTOCOL.md for WebSocket lifecycle.
module ExoMonad.Wire.Types
  ( -- * Server → Client
    UIState(..)
  , ChatMessage(..)
  , MessageRole(..)
  , TextInputConfig(..)
  , PhotoUploadConfig(..)
  , ChoiceOption(..)
  , ChoiceConfig(..)

    -- * DM-specific types
  , DMStats(..)
  , Precarity(..)
  , Clock(..)
  , ClockColor(..)
  , Position(..)
  , Effect(..)
  , OutcomeTier(..)
  , DieOption(..)
  , DicePool(..)
  , DMMood(..)
  , SceneVariant(..)
  , Urgency(..)
  , ActionVariant(..)
  , ActionDomain(..)
  , AftermathVariant(..)
  , DowntimeVariant(..)
  , BargainOption(..)
  , BargainCost(..)
  , CharCreationStep(..)
  , CharacterCreation(..)
  , Archetype(..)
  , Pronouns(..)
  , TarotCard(..)
  , TarotSpread(..)
  , HistoryEntry(..)

    -- * Client → Server
  , UserAction(..)
  ) where

import Data.Text (Text)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, withText, (.:), (.:?), (.!=))
import qualified Data.Aeson.Types as Aeson
import GHC.Generics (Generic)


-- ════════════════════════════════════════════════════════════════════════════
-- UIState (Server → Client)
-- ════════════════════════════════════════════════════════════════════════════

-- | UI state sent from server to client after each graph step.
data UIState = UIState
  { messages :: [ChatMessage]
  , textInput :: Maybe TextInputConfig
  , photoUpload :: Maybe PhotoUploadConfig
  , choices :: Maybe ChoiceConfig
  , graphNode :: Text
  , thinking :: Bool
  -- DM-specific fields
  , stats :: Maybe DMStats
  , clocks :: [Clock]
  , dicePool :: Maybe DicePool
  , mood :: Maybe DMMood
  , charCreation :: Maybe CharacterCreation
  , history :: [HistoryEntry]
  }
  deriving (Show, Eq, Generic)

instance ToJSON UIState where
  toJSON s = object
    [ "messages" .= s.messages
    , "textInput" .= s.textInput
    , "photoUpload" .= s.photoUpload
    , "choices" .= s.choices
    , "graphNode" .= s.graphNode
    , "thinking" .= s.thinking
    , "dmStats" .= s.stats
    , "dmClocks" .= s.clocks
    , "dmDicePool" .= s.dicePool
    , "dmMood" .= s.mood
    , "dmCharCreation" .= s.charCreation
    , "dmHistory" .= s.history
    ]

instance FromJSON UIState where
  parseJSON = withObject "UIState" $ \v ->
    UIState
      <$> v .: "messages"
      <*> v .:? "textInput"
      <*> v .:? "photoUpload"
      <*> v .:? "choices"
      <*> v .: "graphNode"
      <*> v .: "thinking"
      <*> v .:? "dmStats"
      <*> (v .:? "dmClocks" .!= [])
      <*> v .:? "dmDicePool"
      <*> v .:? "dmMood"
      <*> v .:? "dmCharCreation"
      <*> (v .:? "dmHistory" .!= [])

-- | Chat message in conversation history.
data ChatMessage = ChatMessage
  { role :: MessageRole
  , content :: Text
  , timestamp :: Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Message role.
data MessageRole = User | Assistant | System
  deriving (Show, Eq, Generic)

instance ToJSON MessageRole where
  toJSON User = "user"
  toJSON Assistant = "assistant"
  toJSON System = "system"

instance FromJSON MessageRole where
  parseJSON = withText "MessageRole" $ \case
    "user" -> pure User
    "assistant" -> pure Assistant
    "system" -> pure System
    t -> fail $ "Unknown role: " ++ show t

-- | Text input configuration.
data TextInputConfig = TextInputConfig
  { placeholder :: Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Photo upload configuration.
data PhotoUploadConfig = PhotoUploadConfig
  { prompt :: Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Choice option with rich metadata.
data ChoiceOption = ChoiceOption
  { index :: Int              -- ^ 0-based index for response
  , label :: Text             -- ^ Display label
  , description :: Maybe Text -- ^ Optional descriptive text
  , costs :: [Text]           -- ^ Cost tags e.g. ["2 Stress", "1 Heat"]
  , disabled :: Maybe Text    -- ^ Nothing = enabled, Just reason = disabled
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Choice configuration with multi-select support.
data ChoiceConfig = ChoiceConfig
  { prompt :: Text            -- ^ Prompt text shown above choices
  , options :: [ChoiceOption] -- ^ Available options
  , multiSelect :: Bool       -- ^ True = checkboxes, False = radio buttons
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- DM-Specific Types (Blades-inspired, v1 port)
-- ════════════════════════════════════════════════════════════════════════════

-- | Precarity level - drives narrative voice intensity.
-- Calculated as: stress + heat + (wanted * 2) + hunted bonus - recovering penalty
data Precarity
  = OperatingFromStrength  -- ^ score < 5: expansive, plant threats
  | RoomToManeuver         -- ^ 5-9: balanced tension
  | WallsClosingIn         -- ^ 10-14: urgent, compressed
  | HangingByThread        -- ^ >= 15: desperate, every word counts
  deriving (Show, Eq, Generic)

instance ToJSON Precarity where
  toJSON OperatingFromStrength = "operatingFromStrength"
  toJSON RoomToManeuver = "roomToManeuver"
  toJSON WallsClosingIn = "wallsClosingIn"
  toJSON HangingByThread = "hangingByThread"

instance FromJSON Precarity where
  parseJSON = withText "Precarity" $ \case
    "operatingFromStrength" -> pure OperatingFromStrength
    "roomToManeuver" -> pure RoomToManeuver
    "wallsClosingIn" -> pure WallsClosingIn
    "hangingByThread" -> pure HangingByThread
    t -> fail $ "Unknown precarity: " ++ show t

-- | Character stats for sidebar display.
data DMStats = DMStats
  { stress :: Int         -- ^ 0-9, trauma at 9
  , heat :: Int           -- ^ 0-9
  , coin :: Int
  , wantedLevel :: Int    -- ^ 0-4
  , trauma :: [Text]      -- ^ Freeform trauma names (LLM-determined)
  , precarity :: Precarity
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Clock color/type.
data ClockColor = ClockThreat | ClockOpportunity | ClockNeutral
  deriving (Show, Eq, Generic)

instance ToJSON ClockColor where
  toJSON ClockThreat = "threat"
  toJSON ClockOpportunity = "opportunity"
  toJSON ClockNeutral = "neutral"

instance FromJSON ClockColor where
  parseJSON = withText "ClockColor" $ \case
    "threat" -> pure ClockThreat
    "opportunity" -> pure ClockOpportunity
    "neutral" -> pure ClockNeutral
    t -> fail $ "Unknown clock color: " ++ show t

-- | Progress clock for tracking threats and opportunities.
data Clock = Clock
  { id :: Text
  , name :: Text
  , segments :: Int
  , filled :: Int
  , visible :: Bool       -- ^ False = hidden GM clock (revealed when triggered)
  , color :: ClockColor
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Action position (risk level).
data Position = Controlled | Risky | Desperate
  deriving (Show, Eq, Generic)

instance ToJSON Position where
  toJSON Controlled = "controlled"
  toJSON Risky = "risky"
  toJSON Desperate = "desperate"

instance FromJSON Position where
  parseJSON = withText "Position" $ \case
    "controlled" -> pure Controlled
    "risky" -> pure Risky
    "desperate" -> pure Desperate
    t -> fail $ "Unknown position: " ++ show t

-- | Action effect level.
data Effect = Limited | Standard | Great
  deriving (Show, Eq, Generic)

instance ToJSON Effect where
  toJSON Limited = "limited"
  toJSON Standard = "standard"
  toJSON Great = "great"

instance FromJSON Effect where
  parseJSON = withText "Effect" $ \case
    "limited" -> pure Limited
    "standard" -> pure Standard
    "great" -> pure Great
    t -> fail $ "Unknown effect: " ++ show t

-- | Outcome tier - calculated from die value and position.
data OutcomeTier
  = TierCritical   -- ^ 6 at any position
  | TierSuccess    -- ^ 4-5
  | TierPartial    -- ^ 2-3 (or 1-3 at Controlled)
  | TierBad        -- ^ 1 at Risky
  | TierDisaster   -- ^ 1 at Desperate
  deriving (Show, Eq, Generic)

instance ToJSON OutcomeTier where
  toJSON TierCritical = "critical"
  toJSON TierSuccess = "success"
  toJSON TierPartial = "partial"
  toJSON TierBad = "bad"
  toJSON TierDisaster = "disaster"

instance FromJSON OutcomeTier where
  parseJSON = withText "OutcomeTier" $ \case
    "critical" -> pure TierCritical
    "success" -> pure TierSuccess
    "partial" -> pure TierPartial
    "bad" -> pure TierBad
    "disaster" -> pure TierDisaster
    t -> fail $ "Unknown outcome tier: " ++ show t

-- | Single die option with LLM-generated preview.
-- All hints are generated at once when entering Action mood (precommitment).
data DieOption = DieOption
  { value :: Int           -- ^ 1-6
  , tier :: OutcomeTier    -- ^ Calculated from position
  , hint :: Text           -- ^ LLM-generated preview of this outcome
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Dice pool state. Pool depletes across session until bargain mode.
data DicePool = DicePool
  { dice :: [DieOption]        -- ^ Remaining dice (starts at 5, depletes)
  , position :: Position
  , effect :: Effect
  , context :: Text            -- ^ What they're attempting
  , pushAvailable :: Bool      -- ^ Can push yourself for +1d (costs 2 stress)
  , devilBargain :: Maybe Text -- ^ Offered bargain (accept for +1d)
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Scene urgency level.
data Urgency = UrgencyLow | UrgencyMedium | UrgencyHigh | UrgencyCritical
  deriving (Show, Eq, Generic)

instance ToJSON Urgency where
  toJSON UrgencyLow = "low"
  toJSON UrgencyMedium = "medium"
  toJSON UrgencyHigh = "high"
  toJSON UrgencyCritical = "critical"

instance FromJSON Urgency where
  parseJSON = withText "Urgency" $ \case
    "low" -> pure UrgencyLow
    "medium" -> pure UrgencyMedium
    "high" -> pure UrgencyHigh
    "critical" -> pure UrgencyCritical
    t -> fail $ "Unknown urgency: " ++ show t

-- | Scene variant - what kind of scene this is.
data SceneVariant
  = SvEncounter Urgency      -- ^ Someone/something demands attention
  | SvOpportunity Text       -- ^ Something offered (with a catch)
  | SvDiscovery Text         -- ^ Found something with implications
  deriving (Show, Eq, Generic)

instance ToJSON SceneVariant where
  toJSON (SvEncounter u) = object ["variant" .= ("encounter" :: Text), "urgency" .= u]
  toJSON (SvOpportunity t) = object ["variant" .= ("opportunity" :: Text), "catch" .= t]
  toJSON (SvDiscovery t) = object ["variant" .= ("discovery" :: Text), "implications" .= t]

instance FromJSON SceneVariant where
  parseJSON = withObject "SceneVariant" $ \v -> do
    var <- v .: "variant" :: Aeson.Parser Text
    case var of
      "encounter" -> SvEncounter <$> v .: "urgency"
      "opportunity" -> SvOpportunity <$> v .: "catch"
      "discovery" -> SvDiscovery <$> v .: "implications"
      _ -> fail $ "Unknown scene variant: " ++ show var

-- | Action domain overlay.
data ActionDomain = DomInfiltration | DomSocial | DomViolence | DomPursuit | DomArcane
  deriving (Show, Eq, Generic)

instance ToJSON ActionDomain where
  toJSON DomInfiltration = "infiltration"
  toJSON DomSocial = "social"
  toJSON DomViolence = "violence"
  toJSON DomPursuit = "pursuit"
  toJSON DomArcane = "arcane"

instance FromJSON ActionDomain where
  parseJSON = withText "ActionDomain" $ \case
    "infiltration" -> pure DomInfiltration
    "social" -> pure DomSocial
    "violence" -> pure DomViolence
    "pursuit" -> pure DomPursuit
    "arcane" -> pure DomArcane
    t -> fail $ "Unknown action domain: " ++ show t

-- | Action variant - risk level with context.
data ActionVariant
  = AvControlled Text Text   -- ^ Advantage, risk on failure
  | AvRisky Text Text        -- ^ Standard danger, standard stakes
  | AvDesperate Text Text    -- ^ Serious danger, severe consequences
  deriving (Show, Eq, Generic)

instance ToJSON ActionVariant where
  toJSON (AvControlled threat opp) = object
    ["variant" .= ("controlled" :: Text), "threat" .= threat, "opportunity" .= opp]
  toJSON (AvRisky threat opp) = object
    ["variant" .= ("risky" :: Text), "threat" .= threat, "opportunity" .= opp]
  toJSON (AvDesperate threat opp) = object
    ["variant" .= ("desperate" :: Text), "threat" .= threat, "opportunity" .= opp]

instance FromJSON ActionVariant where
  parseJSON = withObject "ActionVariant" $ \v -> do
    var <- v .: "variant" :: Aeson.Parser Text
    case var of
      "controlled" -> AvControlled <$> v .: "threat" <*> v .: "opportunity"
      "risky" -> AvRisky <$> v .: "threat" <*> v .: "opportunity"
      "desperate" -> AvDesperate <$> v .: "threat" <*> v .: "opportunity"
      _ -> fail $ "Unknown action variant: " ++ show var

-- | Aftermath variant - how the action resolved.
data AftermathVariant
  = AmClean                  -- ^ Just achieved goal
  | AmCostly Text            -- ^ Achieved goal but at cost
  | AmSetback Text           -- ^ Things went wrong (with escape route)
  | AmDisaster               -- ^ Catastrophic failure
  deriving (Show, Eq, Generic)

instance ToJSON AftermathVariant where
  toJSON AmClean = object ["variant" .= ("clean" :: Text)]
  toJSON (AmCostly cost) = object ["variant" .= ("costly" :: Text), "cost" .= cost]
  toJSON (AmSetback escape) = object ["variant" .= ("setback" :: Text), "escape" .= escape]
  toJSON AmDisaster = object ["variant" .= ("disaster" :: Text)]

instance FromJSON AftermathVariant where
  parseJSON = withObject "AftermathVariant" $ \v -> do
    var <- v .: "variant" :: Aeson.Parser Text
    case var of
      "clean" -> pure AmClean
      "costly" -> AmCostly <$> v .: "cost"
      "setback" -> AmSetback <$> v .: "escape"
      "disaster" -> pure AmDisaster
      _ -> fail $ "Unknown aftermath variant: " ++ show var

-- | Downtime variant - what kind of downtime activity.
data DowntimeVariant
  = DtRecovery [Text]        -- ^ Available activities
  | DtProject Text Int       -- ^ Long-term work, progress ticks
  | DtEntanglement Text      -- ^ Heat catches up
  deriving (Show, Eq, Generic)

instance ToJSON DowntimeVariant where
  toJSON (DtRecovery acts) = object ["variant" .= ("recovery" :: Text), "activities" .= acts]
  toJSON (DtProject name prog) = object
    ["variant" .= ("project" :: Text), "name" .= name, "progress" .= prog]
  toJSON (DtEntanglement desc) = object ["variant" .= ("entanglement" :: Text), "description" .= desc]

instance FromJSON DowntimeVariant where
  parseJSON = withObject "DowntimeVariant" $ \v -> do
    var <- v .: "variant" :: Aeson.Parser Text
    case var of
      "recovery" -> DtRecovery <$> v .: "activities"
      "project" -> DtProject <$> v .: "name" <*> v .: "progress"
      "entanglement" -> DtEntanglement <$> v .: "description"
      _ -> fail $ "Unknown downtime variant: " ++ show var

-- | Bargain cost types.
data BargainCost
  = CostStress Int
  | CostHeat Int
  | CostWanted
  | CostClockTick Text Int   -- ^ Clock name, segments to tick
  | CostFactionDebt Text     -- ^ Faction name
  | CostTrauma
  | CostItem Text            -- ^ Item lost
  deriving (Show, Eq, Generic)

instance ToJSON BargainCost where
  toJSON (CostStress n) = object ["type" .= ("stress" :: Text), "amount" .= n]
  toJSON (CostHeat n) = object ["type" .= ("heat" :: Text), "amount" .= n]
  toJSON CostWanted = object ["type" .= ("wanted" :: Text)]
  toJSON (CostClockTick name segs) = object
    ["type" .= ("clockTick" :: Text), "clock" .= name, "segments" .= segs]
  toJSON (CostFactionDebt faction) = object ["type" .= ("factionDebt" :: Text), "faction" .= faction]
  toJSON CostTrauma = object ["type" .= ("trauma" :: Text)]
  toJSON (CostItem item) = object ["type" .= ("item" :: Text), "item" .= item]

instance FromJSON BargainCost where
  parseJSON = withObject "BargainCost" $ \v -> do
    ty <- v .: "type" :: Aeson.Parser Text
    case ty of
      "stress" -> CostStress <$> v .: "amount"
      "heat" -> CostHeat <$> v .: "amount"
      "wanted" -> pure CostWanted
      "clockTick" -> CostClockTick <$> v .: "clock" <*> v .: "segments"
      "factionDebt" -> CostFactionDebt <$> v .: "faction"
      "trauma" -> pure CostTrauma
      "item" -> CostItem <$> v .: "item"
      _ -> fail $ "Unknown bargain cost type: " ++ show ty

-- | Bargain option - LLM-generated contextual deal.
data BargainOption = BargainOption
  { label :: Text
  , cost :: BargainCost
  , description :: Text    -- ^ LLM explains what this deal means
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Current game phase/mood with rich variants.
-- LLM controls transitions via tools (Engage, Resolve, etc.) - player cannot force.
data DMMood
  = MoodScene SceneVariant
  | MoodAction ActionVariant (Maybe ActionDomain)
  | MoodAftermath AftermathVariant
  | MoodDowntime DowntimeVariant
  | MoodTrauma                   -- ^ Full turn, LLM determines trauma type
  | MoodBargain [BargainOption]  -- ^ Out of dice, contextual deals offered
  deriving (Show, Eq, Generic)

instance ToJSON DMMood where
  toJSON (MoodScene sv) = object ["mood" .= ("scene" :: Text), "scene" .= sv]
  toJSON (MoodAction av dom) = object
    ["mood" .= ("action" :: Text), "action" .= av, "domain" .= dom]
  toJSON (MoodAftermath av) = object ["mood" .= ("aftermath" :: Text), "aftermath" .= av]
  toJSON (MoodDowntime dv) = object ["mood" .= ("downtime" :: Text), "downtime" .= dv]
  toJSON MoodTrauma = object ["mood" .= ("trauma" :: Text)]
  toJSON (MoodBargain opts) = object ["mood" .= ("bargain" :: Text), "options" .= opts]

instance FromJSON DMMood where
  parseJSON = withObject "DMMood" $ \v -> do
    m <- v .: "mood" :: Aeson.Parser Text
    case m of
      "scene" -> MoodScene <$> v .: "scene"
      "action" -> MoodAction <$> v .: "action" <*> v .:? "domain"
      "aftermath" -> MoodAftermath <$> v .: "aftermath"
      "downtime" -> MoodDowntime <$> v .: "downtime"
      "trauma" -> pure MoodTrauma
      "bargain" -> MoodBargain <$> v .: "options"
      _ -> fail $ "Unknown mood: " ++ show m

-- | Character archetype.
data Archetype = Cutter | Hound | Leech | Lurk | Slide | Spider | Whisper
  deriving (Show, Eq, Generic)

instance ToJSON Archetype where
  toJSON Cutter = "cutter"
  toJSON Hound = "hound"
  toJSON Leech = "leech"
  toJSON Lurk = "lurk"
  toJSON Slide = "slide"
  toJSON Spider = "spider"
  toJSON Whisper = "whisper"

instance FromJSON Archetype where
  parseJSON = withText "Archetype" $ \case
    "cutter" -> pure Cutter
    "hound" -> pure Hound
    "leech" -> pure Leech
    "lurk" -> pure Lurk
    "slide" -> pure Slide
    "spider" -> pure Spider
    "whisper" -> pure Whisper
    t -> fail $ "Unknown archetype: " ++ show t

-- | Character pronouns.
data Pronouns = HeHim | SheHer | TheyThem | CustomPronouns Text
  deriving (Show, Eq, Generic)

instance ToJSON Pronouns where
  toJSON HeHim = object ["type" .= ("heHim" :: Text)]
  toJSON SheHer = object ["type" .= ("sheHer" :: Text)]
  toJSON TheyThem = object ["type" .= ("theyThem" :: Text)]
  toJSON (CustomPronouns p) = object ["type" .= ("custom" :: Text), "pronouns" .= p]

instance FromJSON Pronouns where
  parseJSON = withObject "Pronouns" $ \v -> do
    ty <- v .: "type" :: Aeson.Parser Text
    case ty of
      "heHim" -> pure HeHim
      "sheHer" -> pure SheHer
      "theyThem" -> pure TheyThem
      "custom" -> CustomPronouns <$> v .: "pronouns"
      _ -> fail $ "Unknown pronouns type: " ++ show ty

-- | Tarot card for character creation spread.
data TarotCard = TarotCard
  { name :: Text           -- ^ Card name (e.g., "The Tower", "Three of Swords")
  , meaning :: Text        -- ^ Doskvol-flavored meaning
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Three-card tarot spread for character creation.
data TarotSpread = TarotSpread
  { past :: TarotCard      -- ^ What haunts them
  , present :: TarotCard   -- ^ What drives them now
  , future :: TarotCard    -- ^ What they're moving toward
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Character creation step (Tarot-based, v1 style).
data CharCreationStep
  = CCEnterName
  | CCChoosePronouns
  | CCChooseArchetype
  | CCEnterBackground       -- ^ Freeform "who are you in Doskvol?"
  | CCDrawTarot             -- ^ 3-card spread
  | CCConfirm               -- ^ Review and confirm
  deriving (Show, Eq, Generic)

instance ToJSON CharCreationStep where
  toJSON CCEnterName = object ["step" .= ("enterName" :: Text)]
  toJSON CCChoosePronouns = object ["step" .= ("choosePronouns" :: Text)]
  toJSON CCChooseArchetype = object ["step" .= ("chooseArchetype" :: Text)]
  toJSON CCEnterBackground = object ["step" .= ("enterBackground" :: Text)]
  toJSON CCDrawTarot = object ["step" .= ("drawTarot" :: Text)]
  toJSON CCConfirm = object ["step" .= ("confirm" :: Text)]

instance FromJSON CharCreationStep where
  parseJSON = withObject "CharCreationStep" $ \v -> do
    step <- v .: "step" :: Aeson.Parser Text
    case step of
      "enterName" -> pure CCEnterName
      "choosePronouns" -> pure CCChoosePronouns
      "chooseArchetype" -> pure CCChooseArchetype
      "enterBackground" -> pure CCEnterBackground
      "drawTarot" -> pure CCDrawTarot
      "confirm" -> pure CCConfirm
      _ -> fail $ "Unknown step: " ++ show step

-- | Character creation state (Tarot-based, v1 style).
data CharacterCreation = CharacterCreation
  { step :: CharCreationStep
  , name :: Maybe Text
  , pronouns :: Maybe Pronouns
  , archetype :: Maybe Archetype
  , background :: Maybe Text
  , tarotSpread :: Maybe TarotSpread
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | History entry for session log.
data HistoryEntry = HistoryEntry
  { timestamp :: Text
  , type_ :: Text           -- ^ "narration", "action", "roll", "clock"
  , summary :: Text
  , details :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON HistoryEntry where
  toJSON e = object
    [ "timestamp" .= e.timestamp
    , "type" .= e.type_
    , "summary" .= e.summary
    , "details" .= e.details
    ]

instance FromJSON HistoryEntry where
  parseJSON = withObject "HistoryEntry" $ \v ->
    HistoryEntry
      <$> v .: "timestamp"
      <*> v .: "type"
      <*> v .: "summary"
      <*> v .:? "details"


-- ════════════════════════════════════════════════════════════════════════════
-- UserAction (Client → Server)
-- ════════════════════════════════════════════════════════════════════════════

-- | User action sent from client to server.
data UserAction
  = TextAction Text           -- ^ Free-form text input
  | ChoiceAction Int          -- ^ Single selection (index)
  | MultiChoiceAction [Int]   -- ^ Multiple selections (indices)
  | PhotoAction Text Text     -- ^ Photo upload (base64 data, mimeType)
  deriving (Show, Eq, Generic)

instance ToJSON UserAction where
  toJSON (TextAction content_) = object
    [ "type" .= ("text" :: Text)
    , "content" .= content_
    ]
  toJSON (ChoiceAction idx) = object
    [ "type" .= ("choice" :: Text)
    , "index" .= idx
    ]
  toJSON (MultiChoiceAction idxs) = object
    [ "type" .= ("multiChoice" :: Text)
    , "indices" .= idxs
    ]
  toJSON (PhotoAction data_ mimeType_) = object
    [ "type" .= ("photo" :: Text)
    , "data" .= data_
    , "mimeType" .= mimeType_
    ]

instance FromJSON UserAction where
  parseJSON = withObject "UserAction" $ \v -> do
    ty <- v .: "type" :: Aeson.Parser Text
    case ty of
      "text" -> TextAction <$> v .: "content"
      "choice" -> ChoiceAction <$> v .: "index"
      "multiChoice" -> MultiChoiceAction <$> v .: "indices"
      "photo" -> PhotoAction <$> v .: "data" <*> v .: "mimeType"
      _ -> fail $ "Unknown action type: " ++ show ty
