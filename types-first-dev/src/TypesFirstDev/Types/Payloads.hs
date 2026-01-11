{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FieldSelectors #-}

-- | Cross-node communication payload types for TDD protocol.
-- Prefix convention: lowercase acronym of type name
module TypesFirstDev.Types.Payloads
  ( InitWorkPayload(..)
  , TestsReadyPayload(..)
  , ImplResult(..)
  , TDDApproval(..)
  , MergeComplete(..)
  , ChildFailure(..)
  , MergeEvent(..)
  , Adaptation(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.StructuredOutput (StructuredOutput)

import TypesFirstDev.Types.Shared (PlannedTest, ImpactLevel, ChangeEntry, CoverageReport)

-- | Scaffold output, input to TDD and Impl.
-- Prefix: iwp
data InitWorkPayload = InitWorkPayload
  { iwpScaffoldCommit :: Text           -- ^ Commit hash with scaffold
  , iwpInterfaceFile  :: FilePath       -- ^ Path to interface file
  , iwpContractSuite  :: FilePath       -- ^ Path to contract test suite
  , iwpTestPlan       :: [PlannedTest]  -- ^ Planned tests from scaffold
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | TDD WriteTests output, input to ImplBarrier.
-- Prefix: trp
data TestsReadyPayload = TestsReadyPayload
  { trpCommit          :: Text      -- ^ Commit hash with tests
  , trpTestFiles       :: [FilePath]  -- ^ Test file paths
  , trpPendingCriteria :: [Text]    -- ^ Criteria with tests now waiting
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Impl output, input to TDD ReviewImpl.
-- Prefix: ir
data ImplResult = ImplResult
  { irCommitHash  :: Text    -- ^ Commit hash with implementation
  , irIterations  :: Int     -- ^ Number of iterations taken
  , irPassedTests :: [Text]  -- ^ Test names that passed
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | TDD Approved output, input to Merger.
-- Prefix: ta
data TDDApproval = TDDApproval
  { taSignOff        :: Text            -- ^ TDD sign-off message
  , taCoverageReport :: CoverageReport  -- ^ Criteria coverage report
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Merger output, broadcast to parent and siblings.
-- Sum type: child either succeeded or failed.
-- Prefix: mc
data MergeComplete
  = MergeSuccess
      { mcCommit      :: Text          -- ^ Merge commit hash
      , mcAuthor      :: Text          -- ^ Who authored the changes
      , mcImpactLevel :: ImpactLevel   -- ^ Impact level of changes
      , mcChanges     :: [ChangeEntry] -- ^ List of changes made
      }
  | MergeFailed
      { mcFailure     :: ChildFailure  -- ^ Failure details
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Failure info from child execution.
-- Captures partial work for parent decision-making.
-- Prefix: cf
data ChildFailure = ChildFailure
  { cfReason        :: Text           -- ^ Why the child failed
  , cfBranch        :: Text           -- ^ Git branch where work happened
  , cfAttempts      :: Int            -- ^ Number of attempts before failure
  , cfPartialCommit :: Maybe Text     -- ^ Last commit hash (if any work was done)
  , cfFilesCreated  :: [FilePath]     -- ^ Files created before failure
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Broadcast payload for sibling notification (subset of MergeComplete).
-- Prefix: me
data MergeEvent = MergeEvent
  { meAuthor      :: Text          -- ^ Who authored the changes
  , meImpactLevel :: ImpactLevel   -- ^ Impact level of changes
  , meChanges     :: [ChangeEntry] -- ^ List of changes made
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Rebaser adaptation record.
-- Prefix: ad
data Adaptation = Adaptation
  { adSymbol :: Text  -- ^ Symbol that was adapted
  , adChange :: Text  -- ^ What change was made
  , adReason :: Text  -- ^ Why the adaptation was needed
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)
