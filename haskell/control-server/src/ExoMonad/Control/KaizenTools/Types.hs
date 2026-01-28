{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ExoMonad.Control.KaizenTools.Types
  ( -- * Enums
    Impact(..)
  , FrictionType(..)
  , Effort(..)
    -- * Nested Types
  , Actionability(..)
    -- * Tool Args/Result
  , KaizenReportArgs(..)
  , KaizenReportResult(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import ExoMonad.Schema (deriveMCPTypeWith, deriveMCPEnum, defaultMCPOptions, (??), MCPOptions(..))
import ExoMonad.Control.TLTools.Types (Component(..))

-- ══════════════════════════════════════════════════════════════════════════════
-- ENUMS
-- ══════════════════════════════════════════════════════════════════════════════

-- | Impact level on workflow.
data Impact = ImpactLow | ImpactMedium | ImpactHigh | Blocker
  deriving stock (Show, Eq, Generic, Enum, Bounded)

$(deriveMCPEnum ''Impact)

-- | Type of friction encountered.
data FrictionType = UX | Performance | Documentation | Tooling | DX | Process
  deriving stock (Show, Eq, Generic, Enum, Bounded)

$(deriveMCPEnum ''FrictionType)

-- | Estimated effort to fix.
data Effort = Trivial | Small | Medium | Large | Unknown
  deriving stock (Show, Eq, Generic, Enum, Bounded)

$(deriveMCPEnum ''Effort)

-- ══════════════════════════════════════════════════════════════════════════════
-- NESTED TYPES
-- ══════════════════════════════════════════════════════════════════════════════

-- | Actionability: suggested fixes and workarounds.
data Actionability = Actionability
  { acSuggestedFix    :: Maybe Text
  , acWorkaround      :: Maybe Text
  , acEstimatedEffort :: Effort
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "ac" } ''Actionability
  [ 'acSuggestedFix    ?? "Suggested fix or improvement"
  , 'acWorkaround      ?? "Current workaround if any"
  , 'acEstimatedEffort ?? "Estimated effort to fix"
  ])

-- ══════════════════════════════════════════════════════════════════════════════
-- TOOL ARGS/RESULT
-- ══════════════════════════════════════════════════════════════════════════════

-- | Arguments for kaizen_report tool.
--
-- Structured interface for filing UX friction reports with component
-- awareness and actionability assessment.
data KaizenReportArgs = KaizenReportArgs
  { kraSummary       :: Text
  , kraComponent     :: Component
  , kraSubcomponent  :: Maybe Text
  , kraImpact        :: Impact
  , kraFrictionType  :: FrictionType
  , kraDescription   :: Text
  , kraActionability :: Actionability
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions { fieldPrefix = "kra" } ''KaizenReportArgs
  [ 'kraSummary       ?? "Concise summary of the friction or bug"
  , 'kraComponent     ?? "Which system component has the issue"
  , 'kraSubcomponent  ?? "More specific location within component"
  , 'kraImpact        ?? "Impact on your workflow"
  , 'kraFrictionType  ?? "Type of friction encountered"
  , 'kraDescription   ?? "Detailed description of what happened"
  , 'kraActionability ?? "Suggested fixes and workarounds"
  ])

-- | Result of kaizen_report tool.
data KaizenReportResult = KaizenReportResult
  { krrNumber  :: Int
  , krrUrl     :: Text
  , krrSuccess :: Bool
  , krrError   :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON KaizenReportResult where
  toJSON r = object
    [ "number"  .= krrNumber r
    , "url"     .= krrUrl r
    , "success" .= krrSuccess r
    , "error"   .= krrError r
    ]
