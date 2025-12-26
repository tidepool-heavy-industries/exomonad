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
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Aeson (Value, ToJSON, FromJSON)
import GHC.Generics (Generic)

-- ══════════════════════════════════════════════════════════════
-- DM EVENTS
-- ══════════════════════════════════════════════════════════════

data DMEvent
  = DMThought Text
  | NPCSpoke NpcId Text
  | PlayerAsked Text
  | RandomChoice Text Int
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- THINK AS DM
-- ══════════════════════════════════════════════════════════════

data ThinkAsDM = ThinkAsDM
  deriving (Show, Eq, Generic)

data ThinkInput = ThinkInput { thought :: Text }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Tool ThinkAsDM where
  type ToolInput ThinkAsDM = ThinkInput
  type ToolOutput ThinkAsDM = ()
  
  toolName = "think_as_dm"
  toolDescription = "Internal reasoning as the DM. Not visible to players."
  inputSchema = error "TODO: ThinkAsDM inputSchema - JSON Schema for ThinkInput"
  
  executeTool input = error "TODO: ThinkAsDM executeTool - emit DMThought event"

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

instance Tool SpeakAsNPC where
  type ToolInput SpeakAsNPC = SpeakInput
  type ToolOutput SpeakAsNPC = ()
  
  toolName = "speak_as_npc"
  toolDescription = "Voice a specific NPC character."
  inputSchema = error "TODO: SpeakAsNPC inputSchema - JSON Schema for SpeakInput"
  
  executeTool input = error "TODO: SpeakAsNPC executeTool - emit NPCSpoke event"

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

instance Tool AskPlayer where
  type ToolInput AskPlayer = AskInput
  type ToolOutput AskPlayer = AskResult
  
  toolName = "ask_player"
  toolDescription = "Pause and ask the player a question."
  inputSchema = error "TODO: AskPlayer inputSchema - JSON Schema for AskInput"
  
  executeTool input = error "TODO: AskPlayer executeTool - use requestChoice/requestText from RequestInput effect"

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

instance Tool Choose where
  type ToolInput Choose = ChooseInput
  type ToolOutput Choose = ChooseResult
  
  toolName = "choose"
  toolDescription = "Make a weighted random choice."
  inputSchema = error "TODO: Choose inputSchema - JSON Schema for ChooseInput"
  
  executeTool input = error "TODO: Choose executeTool - use randomDouble from Random effect, emit RandomChoice"

-- ══════════════════════════════════════════════════════════════
-- TOOL REGISTRATION
-- ══════════════════════════════════════════════════════════════

-- | All DM tools as a type-safe list
dmToolList :: ToolList '[ThinkAsDM, SpeakAsNPC, AskPlayer, Choose]
dmToolList = TCons (Proxy @ThinkAsDM)
           $ TCons (Proxy @SpeakAsNPC)
           $ TCons (Proxy @AskPlayer)
           $ TCons (Proxy @Choose)
           $ TNil

-- | All DM tools as JSON for API
dmTools :: [Value]
dmTools = toolListToJSON dmToolList
