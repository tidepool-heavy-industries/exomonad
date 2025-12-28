-- | Character creation flow for new games
--
-- Handles the initial setup when starting a fresh game:
-- 1. Collect player choices (name, pronouns, archetype)
-- 2. Draw tarot spread for story seeding
-- 3. Generate initial state via LLM
module DM.CharacterCreation
  ( -- * Types
    CharacterChoices(..)
  , Pronouns(..)
  , Archetype(..)
  , allArchetypes
  , pronounsText
  , archetypeName
  , archetypeDescription
    -- * Scenario generation
  , scenarioInitPrompt
  , ScenarioInit(..)
  , ClockInit(..)
  , TarotPosition(..)
  , ClockType(..)
  , parseScenarioInit
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..))
import Text.Ginger.GVal.Generic (genericToGVal)

import DM.Tarot (TarotCard, spreadDescription)

-- | Player's pronoun choice
data Pronouns = HeHim | SheHer | TheyThem | Custom Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Character archetypes (Blades in the Dark inspired)
data Archetype
  = Cutter      -- Violence and intimidation
  | Hound       -- Tracking and ranged combat
  | Leech       -- Alchemy and medicine
  | Lurk        -- Stealth and infiltration
  | Slide       -- Deception and social manipulation
  | Spider      -- Scheming and connections
  | Whisper     -- Arcane and ghost work
  deriving (Show, Eq, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | All available archetypes
allArchetypes :: [Archetype]
allArchetypes = [minBound .. maxBound]

-- | Get pronoun text for use in prompts
pronounsText :: Pronouns -> Text
pronounsText = \case
  HeHim -> "he/him"
  SheHer -> "she/her"
  TheyThem -> "they/them"
  Custom t -> t

-- | Get display name for archetype
archetypeName :: Archetype -> Text
archetypeName = \case
  Cutter -> "Cutter"
  Hound -> "Hound"
  Leech -> "Leech"
  Lurk -> "Lurk"
  Slide -> "Slide"
  Spider -> "Spider"
  Whisper -> "Whisper"

-- | Get description for archetype
archetypeDescription :: Archetype -> Text
archetypeDescription = \case
  Cutter -> "A dangerous fighter. You use violence and intimidation to get what you want."
  Hound -> "A deadly hunter. You track targets and eliminate them from a distance."
  Leech -> "A saboteur and tinkerer. You work with chemicals, bombs, and strange devices."
  Lurk -> "A stealthy infiltrator. You sneak into places and take things that aren't yours."
  Slide -> "A smooth talker. You manipulate people with lies, charm, and misdirection."
  Spider -> "A schemer and fixer. You work through contacts, leverage, and long-term plans."
  Whisper -> "An occult dabbler. You deal with ghosts, rituals, and things best left alone."

-- | Collected character choices
data CharacterChoices = CharacterChoices
  { ccName :: Text
  , ccPronouns :: Pronouns
  , ccArchetype :: Archetype
  , ccBackground :: Text          -- Freeform "who are you"
  , ccTarotSpread :: [TarotCard]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Which card position seeded a clock
data TarotPosition = TarotPast | TarotPresent | TarotFuture
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Clock type: threat (bad things) or goal (good things)
data ClockType = ThreatClock | GoalClock
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Clock initialization from scenario generation
data ClockInit = ClockInit
  { ciName :: Text              -- Clock name
  , ciSegments :: Int           -- Total segments (4, 6, or 8)
  , ciFilled :: Int             -- Starting filled segments
  , ciFromCard :: TarotPosition -- Which card seeded this
  , ciType :: ClockType         -- Threat or goal
  , ciConsequenceDesc :: Text   -- What happens when filled
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | LLM response for scenario initialization
data ScenarioInit = ScenarioInit
  { siFateNarration :: Text       -- Fate's interpretation of the spread
  , siStartingClocks :: [ClockInit]  -- Clocks seeded from tarot
  , siSceneNarration :: Text      -- Opening scene narration
  , siStartingStress :: Int       -- 0-4 typically
  , siStartingCoin :: Int         -- 0-4 typically
  , siStartingHeat :: Int         -- 0-2 typically
  , siStartingWanted :: Int       -- 0-1 typically
  , siStartingTrauma :: Maybe Text  -- Optional starting trauma
  , siSceneLocation :: Text       -- Where we start
  , siSceneStakes :: Text         -- What's at stake
  , siOpeningHook :: Text         -- The immediate situation
  , siSuggestedActions :: [Text]  -- Suggested player actions
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Parse LLM response into ScenarioInit
parseScenarioInit :: Value -> Either String ScenarioInit
parseScenarioInit v = case fromJSON v of
  Success s -> Right s
  Error e -> Left e

-- | Generate the scenario initialization prompt
scenarioInitPrompt :: CharacterChoices -> Text
scenarioInitPrompt choices = T.unlines
  [ "<identity>"
  , "You generate the opening of a Blades in the Dark style game."
  , "First speak as FATE - a noir oracle at a card table, smoke curling."
  , "Then drop into Doskvol as the DM with tight noir prose."
  , "</identity>"
  , ""
  , "<world>"
  , "Doskvol: a haunted industrial city of eternal darkness, gas-lamps, canal boats, and gang warfare."
  , "The sun has been dead for over a thousand years. Lightning barriers keep the ghosts at bay."
  , "The streets belong to criminals, nobles play deadly games, and the Spirit Wardens collect the dead."
  , "</world>"
  , ""
  , "<character>"
  , "Name: " <> choices.ccName
  , "Pronouns: " <> pronounsText choices.ccPronouns
  , "Archetype: " <> archetypeName choices.ccArchetype <> " - " <> archetypeDescription choices.ccArchetype
  , "Background: " <> choices.ccBackground
  , "</character>"
  , ""
  , "<spread>"
  , spreadDescription choices.ccTarotSpread
  , "</spread>"
  , ""
  , "<instructions>"
  , "FATE NARRATION (siFateNarration):"
  , "- Interpret each card in 1-2 sentences, tight and knowing"
  , "- Name the clock each card seeds (include in siStartingClocks)"
  , "- Past + Present cards = threat clocks (start 1-2 filled)"
  , "- Future card = goal clock (starts 0 filled)"
  , "- Voice: noir oracle, a little cruel, smoke and knowing"
  , ""
  , "CLOCKS (siStartingClocks):"
  , "- 3 clocks total: 2 threats (from past/present), 1 goal (from future)"
  , "- Each clock: 4-6 segments, threat clocks start 1-2 filled"
  , "- ciConsequenceDesc: what happens when clock fills"
  , ""
  , "SCENE NARRATION (siSceneNarration):"
  , "- Drop into Doskvol. Tight noir prose."
  , "- Establish character in a specific moment - grounding, not action"
  , "- ~100-150 words max"
  , "- Ground the player, then invite action"
  , ""
  , "STARTING STATS:"
  , "- Stress (0-4): Higher if troubled past"
  , "- Coin (0-4): Based on recent jobs/situation"
  , "- Heat (0-2): Higher if past suggests trouble"
  , "- Wanted (0-1): Only 1 if seriously hunted"
  , "- Trauma: Only if past card strongly suggests psychological damage"
  , "</instructions>"
  ]

-- ══════════════════════════════════════════════════════════════
-- ToGVal instances for Ginger templates
-- ══════════════════════════════════════════════════════════════

instance ToGVal m Pronouns where toGVal = genericToGVal
instance ToGVal m Archetype where toGVal = genericToGVal
instance ToGVal m CharacterChoices where toGVal = genericToGVal
instance ToGVal m TarotPosition where toGVal = genericToGVal
instance ToGVal m ClockType where toGVal = genericToGVal
instance ToGVal m ClockInit where toGVal = genericToGVal
instance ToGVal m ScenarioInit where toGVal = genericToGVal
