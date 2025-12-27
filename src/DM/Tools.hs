-- | DM-Specific Tools (mid-turn capabilities)
module DM.Tools
  ( -- * Tools
    ThinkAsDM(..)
  , SpeakAsNPC(..)
  , AskPlayer(..)
  , Choose(..)
  
    -- * Tool Inputs/Outputs
  , ThinkInput(..)
  , SpeakInput(..)
  , AskInput(..)
  , AskResult(..)
  , ChooseInput(..)
  , ChooseResult(..)
  
    -- * Events
  , DMEvent(..)

    -- * Tool Registration
  , dmToolList
  , dmTools
  ) where

import DM.State
import Tidepool.Tool
import Tidepool.Effect (Emit, RequestInput, Random, emit, requestChoice, requestText, randomDouble)
import Tidepool.Schema (objectSchema, arraySchema, emptySchema, schemaToValue, SchemaType(..))
import Effectful (Eff, (:>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Aeson (Value, ToJSON, FromJSON)
import Data.List (find)
import GHC.Generics (Generic)

-- ══════════════════════════════════════════════════════════════
-- DM EVENTS
-- ══════════════════════════════════════════════════════════════

data DMEvent
  = DMThought Text
  | NPCSpoke NpcId Text
  | PlayerAsked Text
  | RandomChoice Text Int
  | ClockCompleted Text Text Consequence  -- clockId, clockName, consequence
  | SceneCompressed Text                   -- summary of what was compressed
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- ══════════════════════════════════════════════════════════════
-- THINK AS DM
-- ══════════════════════════════════════════════════════════════

data ThinkAsDM = ThinkAsDM
  deriving (Show, Eq, Generic)

data ThinkInput = ThinkInput { thought :: Text }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool ThinkAsDM DMEvent where
  type ToolInput ThinkAsDM = ThinkInput
  type ToolOutput ThinkAsDM = ()

  toolName = "think_as_dm"
  toolDescription = "Internal reasoning as the DM. Not visible to players."
  inputSchema = schemaToValue $ objectSchema
    [("thought", emptySchema TString)]
    ["thought"]

  executeTool input = emit (DMThought input.thought)

-- ══════════════════════════════════════════════════════════════
-- SPEAK AS NPC
-- ══════════════════════════════════════════════════════════════

data SpeakAsNPC = SpeakAsNPC
  deriving (Show, Eq, Generic)

data SpeakInput = SpeakInput
  { speakNpc :: NpcId
  , utterance :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool SpeakAsNPC DMEvent where
  type ToolInput SpeakAsNPC = SpeakInput
  type ToolOutput SpeakAsNPC = ()

  toolName = "speak_as_npc"
  toolDescription = "Voice a specific NPC character."
  inputSchema = schemaToValue $ objectSchema
    [ ("speakNpc", emptySchema TString)
    , ("utterance", emptySchema TString)
    ]
    ["speakNpc", "utterance"]

  executeTool input = emit (NPCSpoke input.speakNpc input.utterance)

-- ══════════════════════════════════════════════════════════════
-- ASK PLAYER
-- ══════════════════════════════════════════════════════════════

data AskPlayer = AskPlayer
  deriving (Show, Eq, Generic)

data AskInput = AskInput
  { question :: Text
  , choices :: Maybe [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AskResult = AskResult { playerResponse :: Text }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool AskPlayer DMEvent where
  type ToolInput AskPlayer = AskInput
  type ToolOutput AskPlayer = AskResult

  toolName = "ask_player"
  toolDescription = "Pause and ask the player a question."
  inputSchema = schemaToValue $ objectSchema
    [ ("question", emptySchema TString)
    , ("choices", arraySchema (emptySchema TString))
    ]
    ["question"]  -- choices is optional

  executeTool input = do
    emit (PlayerAsked input.question)
    response <- case input.choices of
      Nothing -> requestText input.question
      Just cs -> requestChoice input.question [(c, c) | c <- cs]
    return (AskResult response)

-- ══════════════════════════════════════════════════════════════
-- CHOOSE
-- ══════════════════════════════════════════════════════════════

data Choose = Choose
  deriving (Show, Eq, Generic)

data ChooseInput = ChooseInput
  { options :: [(Double, Text)]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ChooseResult = ChooseResult
  { chosenIndex :: Int
  , chosenLabel :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool Choose DMEvent where
  type ToolInput Choose = ChooseInput
  type ToolOutput Choose = ChooseResult

  toolName = "choose"
  toolDescription = "Make a weighted random choice. Options are [weight, label] pairs."
  inputSchema = schemaToValue $ objectSchema
    [("options", arraySchema $ arraySchema (emptySchema TNumber))]  -- [[weight, label], ...]
    ["options"]

  executeTool input = do
    let opts = input.options
        totalWeight = sum (map fst opts)
    roll <- randomDouble
    let target = roll * totalWeight
        (idx, label) = pickWeighted 0 target opts
    emit (RandomChoice label idx)
    return (ChooseResult idx label)
    where
      -- Walk through options, accumulating weight until we hit target
      pickWeighted :: Int -> Double -> [(Double, Text)] -> (Int, Text)
      pickWeighted idx target ((w, label) : rest)
        | target <= w = (idx, label)
        | otherwise = pickWeighted (idx + 1) (target - w) rest
      pickWeighted idx _ [] = (idx - 1, "")  -- Fallback (shouldn't happen)

-- ══════════════════════════════════════════════════════════════
-- TOOL REGISTRATION
-- ══════════════════════════════════════════════════════════════

-- | All DM tools as a type-safe list
dmToolList :: ToolList DMEvent '[ThinkAsDM, SpeakAsNPC, AskPlayer, Choose]
dmToolList = TCons (Proxy @ThinkAsDM)
           $ TCons (Proxy @SpeakAsNPC)
           $ TCons (Proxy @AskPlayer)
           $ TCons (Proxy @Choose)
           $ TNil

-- | All DM tools as JSON for API
dmTools :: [Value]
dmTools = toolListToJSON dmToolList
