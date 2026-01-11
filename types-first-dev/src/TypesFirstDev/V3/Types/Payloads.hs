{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Cross-node communication payload types for V3 protocol.
-- Prefix convention: lowercase acronym of type name
module TypesFirstDev.V3.Types.Payloads
  ( InitWorkPayload(..)
  , TestsReadyPayload(..)
  , ImplResult(..)
  , TDDApproval(..)
  , MergeComplete(..)
  , MergeEvent(..)
  , Adaptation(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.StructuredOutput (StructuredOutput)

import TypesFirstDev.V3.Types.Shared (PlannedTest, ImpactLevel, ChangeEntry, CoverageReport)

-- | Scaffold output, input to TDD and Impl.
-- Prefix: iwp
data InitWorkPayload = InitWorkPayload
  { iwpScaffoldCommit :: Text
  , iwpInterfaceFile  :: FilePath
  , iwpContractSuite  :: FilePath
  , iwpTestPlan       :: [PlannedTest]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | TDD WriteTests output, input to ImplBarrier.
-- Prefix: trp
data TestsReadyPayload = TestsReadyPayload
  { trpCommit          :: Text
  , trpTestFiles       :: [FilePath]
  , trpPendingCriteria :: [Text]  -- ^ Criteria with tests now waiting
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Impl output, input to TDD ReviewImpl.
-- Prefix: ir
data ImplResult = ImplResult
  { irCommitHash  :: Text
  , irIterations  :: Int
  , irPassedTests :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | TDD Approved output, input to Merger.
-- Prefix: ta
data TDDApproval = TDDApproval
  { taSignOff        :: Text
  , taCoverageReport :: CoverageReport
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Merger output, broadcast to parent and siblings.
-- Prefix: mc
data MergeComplete = MergeComplete
  { mcCommit      :: Text
  , mcAuthor      :: Text
  , mcImpactLevel :: ImpactLevel
  , mcChanges     :: [ChangeEntry]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Broadcast payload for sibling notification (subset of MergeComplete).
-- Prefix: me
data MergeEvent = MergeEvent
  { meAuthor      :: Text
  , meImpactLevel :: ImpactLevel
  , meChanges     :: [ChangeEntry]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Rebaser adaptation record.
-- Prefix: ad
data Adaptation = Adaptation
  { adSymbol :: Text
  , adChange :: Text
  , adReason :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)
