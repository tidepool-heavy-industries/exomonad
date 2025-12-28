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
  , ccTarotSpread :: [TarotCard]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | LLM response for scenario initialization
data ScenarioInit = ScenarioInit
  { siNarration :: Text         -- Opening narration
  , siStartingStress :: Int     -- 0-4 typically
  , siStartingCoin :: Int       -- 0-4 typically
  , siStartingHeat :: Int       -- 0-2 typically
  , siStartingWanted :: Int     -- 0-1 typically
  , siStartingTrauma :: Maybe Text  -- Optional starting trauma
  , siSceneLocation :: Text     -- Where we start
  , siSceneStakes :: Text       -- What's at stake
  , siOpeningHook :: Text       -- The immediate situation
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
  , "You are the Dungeon Master initializing a new Blades in the Dark game."
  , "Your job is to create a compelling opening scene based on the player's choices and the tarot spread drawn."
  , "</identity>"
  , ""
  , "<world>"
  , "Doskvol: a haunted industrial city of eternal darkness, gas-lamps, canal boats, and gang warfare."
  , "The sun has been dead for over a thousand years. Lightning barriers keep the ghosts at bay."
  , "The streets belong to criminals, nobles play deadly games, and the Spirit Wardens collect the dead."
  , "</world>"
  , ""
  , "<player_character>"
  , "Name: " <> choices.ccName
  , "Pronouns: " <> pronounsText choices.ccPronouns
  , "Archetype: " <> archetypeName choices.ccArchetype <> " - " <> archetypeDescription choices.ccArchetype
  , "</player_character>"
  , ""
  , "<fate>"
  , spreadDescription choices.ccTarotSpread
  , "</fate>"
  , ""
  , "<instructions>"
  , "Create an opening scenario that:"
  , "1. Honors the tarot spread's themes (past shapes backstory, present drives the opening scene, future hints at what's coming)"
  , "2. Fits the character's archetype and establishes them in a situation that uses their skills"
  , "3. Starts in media res - something is happening RIGHT NOW that demands attention"
  , "4. Sets appropriate starting stats based on the character's implied history"
  , ""
  , "Starting stat guidelines:"
  , "- Stress (0-4): Higher if troubled past, lower if things are going well"
  , "- Coin (0-4): Based on recent jobs/situation"
  , "- Heat (0-2): Higher if past card suggests trouble with law/gangs"
  , "- Wanted (0-1): Only 1 if seriously hunted"
  , "- Trauma: Only if past card strongly suggests psychological damage"
  , ""
  , "The opening narration should be 2-3 paragraphs, evocative and immediate."
  , "End with a moment that demands player response."
  , "</instructions>"
  ]

-- ══════════════════════════════════════════════════════════════
-- ToGVal instances for Ginger templates
-- ══════════════════════════════════════════════════════════════

instance ToGVal m Pronouns where toGVal = genericToGVal
instance ToGVal m Archetype where toGVal = genericToGVal
instance ToGVal m CharacterChoices where toGVal = genericToGVal
