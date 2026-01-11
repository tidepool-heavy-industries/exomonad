{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FieldSelectors #-}

-- | Shared workflow types for TDD protocol.
-- Prefix convention: lowercase acronym of type name
module TypesFirstDev.Types.Shared
  ( PlannedTest(..)
  , Critique(..)
  , ImpactLevel(..)
  , ChangeEntry(..)
  , ChangeType(..)
  , NodeInfo(..)
  , CoverageReport(..)
  , ChildSpec(..)
  , InterfaceFile(..)
  , ClarificationRequest(..)
  , ClarificationType(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.StructuredOutput (StructuredOutput)

import TypesFirstDev.Types.Core (Criterion)

-- | Planned test from scaffold's test plan.
-- Prefix: pt
data PlannedTest = PlannedTest
  { ptCriterionId :: Text  -- ^ Which criterion this tests
  , ptName        :: Text  -- ^ Test function name
  , ptApproach    :: Text  -- ^ How the test will verify
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | TDD critique of implementation.
-- Prefix: cq
data Critique = Critique
  { cqFile        :: FilePath  -- ^ Path to file with issue
  , cqLine        :: Int       -- ^ Line number
  , cqIssue       :: Text      -- ^ Description of the issue
  , cqRequiredFix :: Text      -- ^ What fix is required
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Impact level of a merge for sibling rebasing.
data ImpactLevel
  = Trivial   -- ^ No semantic changes
  | Additive  -- ^ New exports added
  | Breaking  -- ^ API changes that may affect siblings
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Entry in changelog for a merge.
-- Prefix: ce
data ChangeEntry = ChangeEntry
  { ceSymbol :: Text        -- ^ Symbol that changed
  , ceType   :: ChangeType  -- ^ Kind of change
  , ceReason :: Text        -- ^ Why this change was made
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Type of change to a symbol.
data ChangeType
  = SignatureChange
  | NewExport
  | RemovedExport
  | BehaviorChange
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Node identification for routing.
-- Prefix: ni
data NodeInfo = NodeInfo
  { niId     :: Text  -- ^ Node identifier
  , niBranch :: Text  -- ^ Git branch for this node
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Coverage report from TDD review.
-- Prefix: cr
data CoverageReport = CoverageReport
  { crCriteriaWithTests :: [(Text, Text)]  -- ^ Pairs of (criterionId, testName)
  , crCriteriaMissing   :: [Text]          -- ^ Criterion IDs without tests
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Child specification for tree decomposition.
-- Prefix: cs
data ChildSpec = ChildSpec
  { csId                 :: Text        -- ^ Child identifier
  , csDescription        :: Text        -- ^ What the child implements
  , csAcceptanceCriteria :: [Criterion] -- ^ Criteria assigned to child
  , csTargetPath         :: FilePath    -- ^ Target path for child
  , csTestPath           :: FilePath    -- ^ Test path for child
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Interface file definition.
-- Prefix: if
data InterfaceFile = InterfaceFile
  { ifPath    :: FilePath  -- ^ Path to interface file
  , ifExports :: [Text]    -- ^ Exported symbols
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Why Scaffold is being re-invoked.
-- Carries diagnostic context from failed downstream nodes.
data ClarificationType
  = SpecAmbiguity       -- ^ Spec has contradictory or unclear requirements
  | BlockedDependency   -- ^ Missing symbol/module that should exist
  | MergeConflict       -- ^ Unresolvable conflict with sibling changes
  | InvalidScaffold     -- ^ Scaffold output missing required elements
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Request for scaffold clarification.
-- Prefix: cr
data ClarificationRequest = ClarificationRequest
  { crType     :: ClarificationType  -- ^ Category of issue
  , crDetails  :: Text               -- ^ What specifically went wrong
  , crQuestion :: Text               -- ^ What the LLM should address
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)
