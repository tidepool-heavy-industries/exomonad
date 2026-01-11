{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FieldSelectors #-}

-- | Node input/output types for TDD protocol.
-- Prefix convention: lowercase acronym of type name
module TypesFirstDev.Types.Nodes
  ( -- * Scaffold
    ScaffoldInput(..)
  , ScaffoldExit(..)
    -- * TDD WriteTests
  , TDDWriteTestsInput(..)
  , TDDWriteTestsExit(..)
    -- * TDD ReviewImpl
  , TDDReviewImplInput(..)
  , TDDReviewImplExit(..)
    -- * Impl
  , ImplInput(..)
  , ImplExit(..)
    -- * Merger
  , MergerInput(..)
  , MergerExit(..)
  , MergeRejectedReason(..)
    -- * Rebaser
  , RebaserInput(..)
  , RebaserExit(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.StructuredOutput (StructuredOutput)

import TypesFirstDev.Types.Core (Spec, ParentContext)
import TypesFirstDev.Types.Shared
  ( PlannedTest, Critique, NodeInfo, CoverageReport
  , ChildSpec, InterfaceFile, ImpactLevel, ChangeEntry
  )
import TypesFirstDev.Types.Payloads
  ( InitWorkPayload, TestsReadyPayload, ImplResult
  , TDDApproval, MergeComplete, MergeEvent, Adaptation
  )

-- ════════════════════════════════════════════════════════════════════════════
-- SCAFFOLD
-- ════════════════════════════════════════════════════════════════════════════

-- | Scaffold node input.
-- Prefix: si
data ScaffoldInput = ScaffoldInput
  { siSpec          :: Spec                  -- ^ Work specification
  , siParentContext :: Maybe ParentContext   -- ^ Context from parent if child
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Scaffold node output (oneOf).
-- Prefix: se
data ScaffoldExit
  = ScaffoldInitWork
      { seCommit      :: Text
      , seInterface   :: FilePath
      , seContract    :: FilePath
      , seTestPlan    :: [PlannedTest]
      , seChildSpecs  :: Maybe [ChildSpec]
      , seInterfaces  :: Maybe [InterfaceFile]
      }
  | ScaffoldClarificationNeeded
      { seQuestion           :: Text
      , seAmbiguityReference :: Text
      , seSpecSentence       :: Text
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- TDD WRITETESTS
-- ════════════════════════════════════════════════════════════════════════════

-- | TDDWriteTests node input.
-- Prefix: twi
data TDDWriteTestsInput = TDDWriteTestsInput
  { twiSpec     :: Spec             -- ^ Work specification
  , twiScaffold :: InitWorkPayload  -- ^ Scaffold output
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | TDDWriteTests node output (oneOf).
-- Prefix: twe
data TDDWriteTestsExit
  = TDDTestsReady
      { tweTestsCommit      :: Text
      , tweTestFiles        :: [FilePath]
      , twePendingCriteria  :: [Text]
      }
  | TDDInvalidScaffold
      { tweMissingType      :: Text
      , tweExpectedLocation :: FilePath
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- TDD REVIEWIMPL
-- ════════════════════════════════════════════════════════════════════════════

-- | TDDReviewImpl node input.
-- Prefix: tri
data TDDReviewImplInput = TDDReviewImplInput
  { triSpec       :: Spec             -- ^ Work specification
  , triScaffold   :: InitWorkPayload  -- ^ Scaffold output
  , triImplResult :: ImplResult       -- ^ Implementation result to review
  , triDiff       :: Text             -- ^ Diff of implementation changes
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | TDDReviewImpl node output (oneOf).
-- Prefix: tre
data TDDReviewImplExit
  = TDDApproved
      { treSignOff        :: Text
      , treCoverageReport :: CoverageReport
      }
  | TDDMoreTests
      { treCritiques       :: [Critique]
      , treAdditionalTests :: [PlannedTest]
      }
  | TDDReject
      { treReason          :: Text
      , treMissingCriteria :: [Text]
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- IMPL
-- ════════════════════════════════════════════════════════════════════════════

-- | Impl node input.
-- Prefix: ii
data ImplInput = ImplInput
  { iiSpec         :: Spec                   -- ^ Work specification
  , iiScaffold     :: InitWorkPayload        -- ^ Scaffold output
  , iiTestsReady   :: TestsReadyPayload      -- ^ TDD test readiness payload
  , iiChildMerges  :: Maybe [MergeComplete]  -- ^ Child merge results if any
  , iiAttemptCount :: Int                    -- ^ Current attempt number
  , iiCritiqueList :: Maybe [Critique]       -- ^ TDD critiques to address
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Impl node output (oneOf).
-- Prefix: ie
data ImplExit
  = ImplTestsPassed
      { ieCommitHash  :: Text
      , ieIterations  :: Int
      , iePassedTests :: [Text]
      }
  | ImplRequestRetry
      { ieDiagnosis    :: Text
      , ieStrategyFrom :: Text
      , ieStrategyTo   :: Text
      , ieFailingTests :: [Text]
      }
  | ImplBlockedDependency
      { ieMissingSymbol     :: Text
      , ieExpectedImportPath :: FilePath
      }
  | ImplSpecAmbiguity
      { ieSpecSentence       :: Text
      , ieContradictionTrace :: Text
      , ieQuestion           :: Text
      }
  | ImplStuck
      { ieStuckDiagnosis      :: Text
      , ieStuckRecommendation :: Text
      , ieStuckAttempts       :: Int
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- MERGER
-- ════════════════════════════════════════════════════════════════════════════

-- | Merger node input.
-- Prefix: mi
data MergerInput = MergerInput
  { miParentNode    :: NodeInfo     -- ^ Parent node info
  , miChildNode     :: NodeInfo     -- ^ Child node to merge
  , miTddApproval   :: TDDApproval  -- ^ TDD approval from child
  , miContractSuite :: FilePath     -- ^ Contract suite path
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Merger node output (oneOf).
-- Prefix: mex
data MergerExit
  = MergerComplete
      { mexCommit      :: Text
      , mexAuthor      :: Text
      , mexImpactLevel :: ImpactLevel
      , mexChanges     :: [ChangeEntry]
      }
  | MergerRejected
      { mexReason       :: MergeRejectedReason
      , mexDetails      :: Text
      , mexFailingTests :: [Text]
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Reason for merge rejection.
data MergeRejectedReason
  = ContractViolation
  | BuildFailure
  | IntegrationFailure
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- ════════════════════════════════════════════════════════════════════════════
-- REBASER
-- ════════════════════════════════════════════════════════════════════════════

-- | Rebaser node input.
-- Prefix: ri
data RebaserInput = RebaserInput
  { riNode          :: NodeInfo   -- ^ Node being rebased
  , riParentBranch  :: Text       -- ^ Parent branch name
  , riNewParentHead :: Text       -- ^ New parent HEAD commit
  , riMergeEvent    :: MergeEvent -- ^ Merge event triggering rebase
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)

-- | Rebaser node output (oneOf).
-- Prefix: re
data RebaserExit
  = RebaserClean
      { reNewBase :: Text
      }
  | RebaserAdapted
      { reAdaptedBase :: Text
      , reAdaptations :: [Adaptation]
      }
  | RebaserConflict
      { reConflictFile    :: FilePath
      , reOurChange       :: Text
      , reTheirChange     :: Text
      , reWhyUnresolvable :: Text
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, StructuredOutput)
