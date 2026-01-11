{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Shared workflow types for V3 protocol.
-- Prefix convention: lowercase acronym of type name
module TypesFirstDev.V3.Types.Shared
  ( PlannedTest(..)
  , Critique(..)
  , ImpactLevel(..)
  , ChangeEntry(..)
  , ChangeType(..)
  , NodeInfo(..)
  , CoverageReport(..)
  , ChildSpec(..)
  , InterfaceFile(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.StructuredOutput (StructuredOutput)

import TypesFirstDev.V3.Types.Core (Criterion)

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
  { cqFile        :: FilePath
  , cqLine        :: Int
  , cqIssue       :: Text
  , cqRequiredFix :: Text
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
  { ceSymbol :: Text
  , ceType   :: ChangeType
  , ceReason :: Text
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
  { niId     :: Text
  , niBranch :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Coverage report from TDD review.
-- Prefix: cr
data CoverageReport = CoverageReport
  { crCriteriaWithTests :: [(Text, Text)]  -- ^ (criterionId, testName)
  , crCriteriaMissing   :: [Text]          -- ^ criterionIds without tests
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Child specification for tree decomposition.
-- Prefix: cs
data ChildSpec = ChildSpec
  { csId                 :: Text
  , csDescription        :: Text
  , csAcceptanceCriteria :: [Criterion]
  , csTargetPath         :: FilePath
  , csTestPath           :: FilePath
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Interface file definition.
-- Prefix: if
data InterfaceFile = InterfaceFile
  { ifPath    :: FilePath
  , ifExports :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)
