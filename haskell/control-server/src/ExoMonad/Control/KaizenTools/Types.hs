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
import ExoMonad.Schema (deriveMCPTypeWith, deriveMCPEnum, defaultMCPOptions, (??))
import ExoMonad.Control.TLTools.Types (Component(..))
import Language.Haskell.TH (mkName)

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
  { suggestedFix    :: Maybe Text
  , workaround      :: Maybe Text
  , estimatedEffort :: Effort
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions ''Actionability
  [ mkName "suggestedFix"    ?? "Suggested fix or improvement"
  , mkName "workaround"      ?? "Current workaround if any"
  , mkName "estimatedEffort" ?? "Estimated effort to fix"
  ])

-- ══════════════════════════════════════════════════════════════════════════════
-- TOOL ARGS/RESULT
-- ══════════════════════════════════════════════════════════════════════════════

-- | Arguments for kaizen_report tool.
--
-- Structured interface for filing UX friction reports with component
-- awareness and actionability assessment.
data KaizenReportArgs = KaizenReportArgs
  { summary       :: Text
  , component     :: Component
  , subcomponent  :: Maybe Text
  , impact        :: Impact
  , frictionType  :: FrictionType
  , description   :: Text
  , actionability :: Actionability
  }
  deriving stock (Show, Eq, Generic)

$(deriveMCPTypeWith defaultMCPOptions ''KaizenReportArgs
  [ mkName "summary"       ?? "Concise summary of the friction or bug"
  , mkName "component"     ?? "Which system component has the issue"
  , mkName "subcomponent"  ?? "More specific location within component"
  , mkName "impact"        ?? "Impact on your workflow"
  , mkName "frictionType"  ?? "Type of friction encountered"
  , mkName "description"   ?? "Detailed description of what happened"
  , mkName "actionability" ?? "Suggested fixes and workarounds"
  ])

-- | Result of kaizen_report tool.
data KaizenReportResult = KaizenReportResult
  { number  :: Int
  , url     :: Text
  , success :: Bool
  , error   :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON KaizenReportResult where
  toJSON r = object
    [ "number"  .= r.number
    , "url"     .= r.url
    , "success" .= r.success
    , "error"   .= r.error
    ]
