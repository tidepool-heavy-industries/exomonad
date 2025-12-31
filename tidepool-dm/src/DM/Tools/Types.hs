{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FieldSelectors #-}

-- | Tool input/output types for DM tools.
--
-- These types are defined in a separate module to support TH staging:
-- 'deriveHasJSONSchema' requires types to be in an already-compiled module.
--
-- All record fields have Haddock documentation that becomes JSON schema
-- descriptions, guiding the LLM on proper tool usage.
module DM.Tools.Types
  ( -- * Think Tool
    ThinkInput(..)

    -- * Speak Tool
  , SpeakInput(..)

    -- * Choose Tool
  , ChooseInput(..)
  , ChooseResult(..)

    -- * Spend Die Tool
  , SpendDieInput(..)
  , SpendDieResult(..)

    -- * Engage Tool (Scene → Action)
  , EngageInput(..)

    -- * Resolve Tool (Action → Aftermath)
  , ResolveInput(..)

    -- * Accept Tool (Aftermath → Scene)
  , AcceptInput(..)

    -- * Accept Bargain Tool
  , AcceptBargainInput(..)
  , AcceptBargainResult(..)

    -- * Retreat Tool
  , RetreatInput(..)

    -- * Pass Out Tool
  , PassOutInput(..)
  , PassOutResult(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import DM.State (Position, OutcomeTier, NpcId)

-- ══════════════════════════════════════════════════════════════════════════════
-- THINK TOOL
-- ══════════════════════════════════════════════════════════════════════════════

-- | Input for the think_as_dm tool.
data ThinkInput = ThinkInput
  { thought :: Text
    -- ^ Internal reasoning about the current situation. Not visible to players.
  }
  deriving (Show, Eq, Generic)

instance ToJSON ThinkInput
instance FromJSON ThinkInput

-- ══════════════════════════════════════════════════════════════════════════════
-- SPEAK TOOL
-- ══════════════════════════════════════════════════════════════════════════════

-- | Input for the speak_as_npc tool.
data SpeakInput = SpeakInput
  { speakNpc :: NpcId
    -- ^ The NPC's unique identifier.
  , utterance :: Text
    -- ^ What the NPC says, in their voice and manner.
  }
  deriving (Show, Eq, Generic)

instance ToJSON SpeakInput
instance FromJSON SpeakInput

-- ══════════════════════════════════════════════════════════════════════════════
-- CHOOSE TOOL
-- ══════════════════════════════════════════════════════════════════════════════

-- | Input for the choose tool (weighted random selection).
data ChooseInput = ChooseInput
  { options :: [(Double, Text)]
    -- ^ List of (weight, label) pairs. Higher weights are more likely.
  }
  deriving (Show, Eq, Generic)

instance ToJSON ChooseInput
instance FromJSON ChooseInput

-- | Result from the choose tool.
data ChooseResult = ChooseResult
  { chosenIndex :: Int
    -- ^ Zero-based index of the chosen option.
  , chosenLabel :: Text
    -- ^ Label of the chosen option.
  }
  deriving (Show, Eq, Generic)

instance ToJSON ChooseResult
instance FromJSON ChooseResult

-- ══════════════════════════════════════════════════════════════════════════════
-- SPEND DIE TOOL
-- ══════════════════════════════════════════════════════════════════════════════

-- | Input for the spend_die tool.
-- The LLM precommits to outcomes before the player chooses which die to spend.
data SpendDieInput = SpendDieInput
  { situation :: Text
    -- ^ Brief description of what's at stake.
  , position :: Position
    -- ^ Risk level: Controlled, Risky, or Desperate.
  , outcomes :: [(Int, Text, Text)]
    -- ^ Array parallel to dice pool. Each element is (dieValue, hint, narrative).
    -- outcomes[i] is for pool[i]. EACH die gets a unique outcome even if same value.
    -- Hint: 3-8 words shown during choice. Narrative: 1-3 sentences revealed after.
  }
  deriving (Show, Eq, Generic)

instance ToJSON SpendDieInput
instance FromJSON SpendDieInput

-- | Result from the spend_die tool.
data SpendDieResult = SpendDieResult
  { dieValue :: Int
    -- ^ The value of the die that was spent (1-6).
  , tier :: OutcomeTier
    -- ^ The outcome tier based on position and die value.
  , narrative :: Text
    -- ^ The precommitted narrative for this die, now revealed.
  }
  deriving (Show, Eq, Generic)

instance ToJSON SpendDieResult
instance FromJSON SpendDieResult

-- ══════════════════════════════════════════════════════════════════════════════
-- ENGAGE TOOL (Scene → Action)
-- ══════════════════════════════════════════════════════════════════════════════

-- | Input for the engage tool. Transitions from SCENE to ACTION mood.
data EngageInput = EngageInput
  { engageIntent :: Text
    -- ^ What the player is trying to accomplish.
  , engageApproach :: Text
    -- ^ How they're going about it.
  , engagePosition :: Text
    -- ^ Risk level: "controlled", "risky", or "desperate".
  , engageDomain :: Maybe Text
    -- ^ Action domain: "infiltration", "social", "violence", "pursuit", or "arcane".
  , engageThreat :: Text
    -- ^ What could go wrong if things turn bad.
  , engageOpportunity :: Text
    -- ^ What could be gained if things go well.
  }
  deriving (Show, Eq, Generic)

instance ToJSON EngageInput
instance FromJSON EngageInput

-- ══════════════════════════════════════════════════════════════════════════════
-- RESOLVE TOOL (Action → Aftermath)
-- ══════════════════════════════════════════════════════════════════════════════

-- | Input for the resolve tool. Transitions from ACTION to AFTERMATH mood.
data ResolveInput = ResolveInput
  { resolveOutcome :: Text
    -- ^ Type of outcome: "clean", "costly", "setback", or "disaster".
  , resolveWhat :: Text
    -- ^ What the player achieved or what happened.
  , resolveCosts :: [Text]
    -- ^ Costs paid (stress, harm, complications). May be empty for clean outcomes.
  , resolveComplications :: [Text]
    -- ^ New problems introduced. May be empty.
  }
  deriving (Show, Eq, Generic)

instance ToJSON ResolveInput
instance FromJSON ResolveInput

-- ══════════════════════════════════════════════════════════════════════════════
-- ACCEPT TOOL (Aftermath → Scene)
-- ══════════════════════════════════════════════════════════════════════════════

-- | Input for the accept tool. Transitions from AFTERMATH back to SCENE mood.
data AcceptInput = AcceptInput
  { acceptTransition :: Text
    -- ^ Brief narration of moving on from the aftermath.
  }
  deriving (Show, Eq, Generic)

instance ToJSON AcceptInput
instance FromJSON AcceptInput

-- ══════════════════════════════════════════════════════════════════════════════
-- ACCEPT BARGAIN TOOL
-- ══════════════════════════════════════════════════════════════════════════════

-- | Input for the accept_bargain tool. Player accepts a deal to refresh dice.
data AcceptBargainInput = AcceptBargainInput
  { bargainDescription :: Text
    -- ^ What the bargain entails, e.g. "Owe Bazso Baz a favor".
  , bargainCostType :: Text
    -- ^ Type of cost: "stress", "heat", "wanted", "clock", "faction", "trauma", or "item".
  , bargainCostAmount :: Int
    -- ^ Amount for stress/heat (1-4), or ticks for clock (1-4). Ignored for other types.
  , bargainCostTarget :: Maybe Text
    -- ^ Clock ID or Faction ID if applicable. Required for "clock" and "faction" types.
  , bargainDiceGained :: Int
    -- ^ Number of dice to add to pool (1-3).
  }
  deriving (Show, Eq, Generic)

instance ToJSON AcceptBargainInput
instance FromJSON AcceptBargainInput

-- | Result from the accept_bargain tool.
data AcceptBargainResult = AcceptBargainResult
  { newPoolSize :: Int
    -- ^ Total dice in pool after adding new dice.
  , costApplied :: Text
    -- ^ Human-readable description of the cost that was applied.
  }
  deriving (Show, Eq, Generic)

instance ToJSON AcceptBargainResult
instance FromJSON AcceptBargainResult

-- ══════════════════════════════════════════════════════════════════════════════
-- RETREAT TOOL
-- ══════════════════════════════════════════════════════════════════════════════

-- | Input for the retreat tool. Player chooses to leave and rest.
data RetreatInput = RetreatInput
  { retreatNarration :: Text
    -- ^ How the player slips away, e.g. "You vanish into the fog..."
  }
  deriving (Show, Eq, Generic)

instance ToJSON RetreatInput
instance FromJSON RetreatInput

-- ══════════════════════════════════════════════════════════════════════════════
-- PASS OUT TOOL
-- ══════════════════════════════════════════════════════════════════════════════

-- | Input for the pass_out tool. Involuntary collapse when no retreat possible.
data PassOutInput = PassOutInput
  { passOutNarration :: Text
    -- ^ How the player collapses, e.g. "Your legs buckle beneath you..."
  , clocksToAdvance :: [Text]
    -- ^ IDs of clocks to advance by 1 tick (threats progress while unconscious).
  , wakeUpLocation :: Text
    -- ^ Where the player comes to: captured, rescued, in the gutter, etc.
  }
  deriving (Show, Eq, Generic)

instance ToJSON PassOutInput
instance FromJSON PassOutInput

-- | Result from the pass_out tool.
data PassOutResult = PassOutResult
  { clocksAdvanced :: [Text]
    -- ^ IDs of clocks that were advanced.
  , wokeUpAt :: Text
    -- ^ Where the player woke up.
  }
  deriving (Show, Eq, Generic)

instance ToJSON PassOutResult
instance FromJSON PassOutResult
