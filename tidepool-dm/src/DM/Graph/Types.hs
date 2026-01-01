-- | Payload types for DM Graph transitions
--
-- These types carry data between graph nodes. Unlike the monolithic DMContext,
-- each transition carries exactly the data needed by the target node.
module DM.Graph.Types
  ( -- * Player Input (Entry)
    PlayerInput(..)

    -- * Scene Payloads
  , SceneSetup(..)
  , SceneResult(..)

    -- * Action Payloads
  , ActionSetup(..)
  , ActionResult(..)

    -- * Aftermath Payloads
  , AftermathSetup(..)
  , AftermathResult(..)

    -- * Bargain Payloads
  , BargainSetup(..)
  , BargainResult(..)

    -- * Trauma Payloads
  , TraumaSetup(..)

    -- * Downtime Payloads
  , DowntimeSetup(..)

    -- * Response (Exit)
  , Response(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import DM.State
  ( SceneVariant
  , ActionVariant
  , ActionDomain
  , AftermathVariant
  , BargainVariant
  , TraumaVariant
  , DowntimeVariant
  , OutcomeTier
  , DMMood
  )

-- ══════════════════════════════════════════════════════════════
-- ENTRY: Player Input
-- ══════════════════════════════════════════════════════════════

-- | Player's action text - the entry point to the graph
data PlayerInput = PlayerInput
  { piActionText :: Text
    -- ^ What the player said/did
  , piResumeMood :: Maybe DMMood
    -- ^ If resuming, what mood to continue from (Nothing = new scene)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- SCENE TRANSITIONS
-- ══════════════════════════════════════════════════════════════

-- | Setup for entering a scene node
data SceneSetup = SceneSetup
  { ssVariant :: SceneVariant
    -- ^ Which scene type (Encounter, Opportunity, Discovery)
  , ssPlayerAction :: Text
    -- ^ What the player just did
  , ssPreviousNarration :: Maybe Text
    -- ^ Narration from previous phase (for continuity)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Result from a scene node (before deciding next transition)
data SceneResult = SceneResult
  { srNarration :: Text
    -- ^ The DM's response
  , srEngageAction :: Maybe ActionSetup
    -- ^ If the LLM triggered engage, this is populated
  , srContinueScene :: Bool
    -- ^ Should we loop back to scene?
  , srEndScene :: Bool
    -- ^ Should we transition to downtime?
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- ACTION TRANSITIONS
-- ══════════════════════════════════════════════════════════════

-- | Setup for entering an action node (dice resolution)
data ActionSetup = ActionSetup
  { asVariant :: ActionVariant
    -- ^ Position: Controlled, Risky, or Desperate
  , asDomain :: Maybe ActionDomain
    -- ^ Domain overlay: Infiltration, Social, Violence, etc.
  , asPlayerAction :: Text
    -- ^ What the player is attempting
  , asContext :: Text
    -- ^ Context for the action (from scene)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Result from an action node
data ActionResult = ActionResult
  { arNarration :: Text
    -- ^ Pre-resolution narration
  , arResolvedTier :: Maybe OutcomeTier
    -- ^ If dice were resolved, the outcome tier
  , arTriggerBargain :: Bool
    -- ^ True if dice pool is empty → transition to Bargain
  , arAftermathSetup :: Maybe AftermathSetup
    -- ^ If resolved, setup for aftermath
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- AFTERMATH TRANSITIONS
-- ══════════════════════════════════════════════════════════════

-- | Setup for entering an aftermath node
data AftermathSetup = AftermathSetup
  { afVariant :: AftermathVariant
    -- ^ Outcome type: Clean, Costly, Setback, or Disaster
  , afOutcomeTier :: OutcomeTier
    -- ^ The dice outcome that led here
  , afActionContext :: Text
    -- ^ What action led to this
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Result from an aftermath node
data AftermathResult = AftermathResult
  { afrNarration :: Text
    -- ^ Consequence narration
  , afrTriggerTrauma :: Bool
    -- ^ True if stress >= 9 → transition to Trauma
  , afrContinueScene :: Bool
    -- ^ True if continuing in current scene
  , afrEndScene :: Bool
    -- ^ True if scene should end → transition to Downtime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- BARGAIN TRANSITIONS
-- ══════════════════════════════════════════════════════════════

-- | Setup for entering a bargain node (out of dice)
data BargainSetup = BargainSetup
  { bsVariant :: BargainVariant
    -- ^ Bargain context
  , bsPlayerAction :: Text
    -- ^ What the player was attempting when they ran out
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Result from a bargain node
data BargainResult = BargainResult
  { brNarration :: Text
    -- ^ Bargain resolution narration
  , brAcceptedDeal :: Bool
    -- ^ True if player accepted a deal → return to scene
  , brRetreated :: Bool
    -- ^ True if player retreated → transition to Downtime
  , brPassedOut :: Bool
    -- ^ True if player passed out → transition to Trauma
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- TRAUMA TRANSITIONS
-- ══════════════════════════════════════════════════════════════

-- | Setup for entering a trauma node (stress >= 9)
data TraumaSetup = TraumaSetup
  { tsVariant :: TraumaVariant
    -- ^ What broke and how
  , tsTrigger :: Text
    -- ^ What caused the break
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- DOWNTIME TRANSITIONS
-- ══════════════════════════════════════════════════════════════

-- | Setup for entering a downtime node
data DowntimeSetup = DowntimeSetup
  { dsVariant :: DowntimeVariant
    -- ^ Recovery, Project, or Entanglement
  , dsPreviousSceneSummary :: Text
    -- ^ Brief summary of what just happened
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- EXIT: Response
-- ══════════════════════════════════════════════════════════════

-- | Final response to the player (exit type)
--
-- Includes delta fields for GUI display of mechanical changes.
-- These track actual changes after clamping (not LLM's claimed values).
data Response = Response
  { rNarration :: Text
    -- ^ The DM's narrative response
  , rStressDelta :: Int
    -- ^ Actual stress change this turn (after clamping)
  , rCoinDelta :: Int
    -- ^ Actual coin change this turn
  , rHeatDelta :: Int
    -- ^ Actual heat change this turn
  , rSuggestedActions :: [Text]
    -- ^ Suggested next actions for quick-pick buttons
  , rSessionEnded :: Bool
    -- ^ True if the session is ending
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
