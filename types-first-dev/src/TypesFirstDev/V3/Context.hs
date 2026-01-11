{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Template context types for V3 graph nodes.
--
-- These types are what Jinja templates receive for rendering.
-- Must be in separate module from Templates.hs for TH staging.
--
-- NOTE: Context field names match template variables.
-- The ToGVal instances map prefixed type fields to unprefixed template access.
-- e.g., sDescription -> description, irCommitHash -> commitHash
module TypesFirstDev.V3.Context
  ( -- * Scaffold Context
    ScaffoldTemplateCtx(..)

    -- * TDD WriteTests Context
  , TDDWriteTestsTemplateCtx(..)

    -- * TDD ReviewImpl Context
  , TDDReviewImplTemplateCtx(..)

    -- * Impl Context
  , ImplTemplateCtx(..)

    -- * Merger Context
  , MergerTemplateCtx(..)

    -- * Rebaser Context
  , RebaserTemplateCtx(..)
  ) where

import Control.Monad.Writer (Writer)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..), dict, list)
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

import TypesFirstDev.V3.Types

-- | Type alias for the ginger monad
type GingerM = Run SourcePos (Writer Text) Text

-- ════════════════════════════════════════════════════════════════════════════
-- SCAFFOLD CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Context for scaffold template.
data ScaffoldTemplateCtx = ScaffoldTemplateCtx
  { spec          :: Spec
  , parentContext :: Maybe ParentContext
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance ToGVal GingerM ScaffoldTemplateCtx where
  toGVal ctx = dict
    [ ("spec", toGVal ctx.spec)
    , ("parentContext", toGVal ctx.parentContext)
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- TDD WRITETESTS CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Context for TDD write tests template.
data TDDWriteTestsTemplateCtx = TDDWriteTestsTemplateCtx
  { spec            :: Spec
  , scaffold        :: InitWorkPayload
  , coveredCriteria :: [Text]  -- From TDDMem
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance ToGVal GingerM TDDWriteTestsTemplateCtx where
  toGVal ctx = dict
    [ ("spec", toGVal ctx.spec)
    , ("scaffold", toGVal ctx.scaffold)
    , ("coveredCriteria", list (toGVal <$> ctx.coveredCriteria))
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- TDD REVIEWIMPL CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Context for TDD review impl template.
data TDDReviewImplTemplateCtx = TDDReviewImplTemplateCtx
  { spec       :: Spec
  , scaffold   :: InitWorkPayload
  , implResult :: ImplResult
  , diff       :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance ToGVal GingerM TDDReviewImplTemplateCtx where
  toGVal ctx = dict
    [ ("spec", toGVal ctx.spec)
    , ("scaffold", toGVal ctx.scaffold)
    , ("implResult", toGVal ctx.implResult)
    , ("diff", toGVal ctx.diff)
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- IMPL CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Context for impl template.
data ImplTemplateCtx = ImplTemplateCtx
  { spec         :: Spec
  , scaffold     :: InitWorkPayload
  , testsReady   :: TestsReadyPayload
  , childMerges  :: Maybe [MergeComplete]
  , attemptCount :: Int
  , critiqueList :: Maybe [Critique]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance ToGVal GingerM ImplTemplateCtx where
  toGVal ctx = dict
    [ ("spec", toGVal ctx.spec)
    , ("scaffold", toGVal ctx.scaffold)
    , ("testsReady", toGVal ctx.testsReady)
    , ("childMerges", toGVal ctx.childMerges)
    , ("attemptCount", toGVal ctx.attemptCount)
    , ("critiqueList", toGVal ctx.critiqueList)
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- MERGER CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Context for merger template.
data MergerTemplateCtx = MergerTemplateCtx
  { parentNode    :: NodeInfo
  , childNode     :: NodeInfo
  , tddApproval   :: TDDApproval
  , contractSuite :: FilePath
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance ToGVal GingerM MergerTemplateCtx where
  toGVal ctx = dict
    [ ("parentNode", toGVal ctx.parentNode)
    , ("childNode", toGVal ctx.childNode)
    , ("tddApproval", toGVal ctx.tddApproval)
    , ("contractSuite", toGVal ctx.contractSuite)
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- REBASER CONTEXT
-- ════════════════════════════════════════════════════════════════════════════

-- | Context for rebaser template.
data RebaserTemplateCtx = RebaserTemplateCtx
  { node          :: NodeInfo
  , parentBranch  :: Text
  , newParentHead :: Text
  , mergeEvent    :: MergeEvent
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance ToGVal GingerM RebaserTemplateCtx where
  toGVal ctx = dict
    [ ("node", toGVal ctx.node)
    , ("parentBranch", toGVal ctx.parentBranch)
    , ("newParentHead", toGVal ctx.newParentHead)
    , ("mergeEvent", toGVal ctx.mergeEvent)
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- ToGVal instances for domain types
-- Maps prefixed field names (sId) to template access (id)
-- ════════════════════════════════════════════════════════════════════════════

instance ToGVal GingerM Spec where
  toGVal s = dict
    [ ("id", toGVal s.sId)
    , ("description", toGVal s.sDescription)
    , ("acceptanceCriteria", list (toGVal <$> s.sAcceptanceCriteria))
    , ("targetPath", toGVal s.sTargetPath)
    , ("testPath", toGVal s.sTestPath)
    , ("complexityConstraints", toGVal s.sComplexityConstraints)
    ]

instance ToGVal GingerM Constraints where
  toGVal c = dict
    [ ("time", toGVal c.cTime)
    , ("space", toGVal c.cSpace)
    ]

instance ToGVal GingerM Criterion where
  toGVal c = dict
    [ ("id", toGVal c.critId)
    , ("text", toGVal c.critText)
    ]

instance ToGVal GingerM ParentContext where
  toGVal p = dict
    [ ("interface", toGVal p.pcInterface)
    , ("assignedCriteria", toGVal p.pcAssignedCriteria)
    ]

instance ToGVal GingerM InitWorkPayload where
  toGVal p = dict
    [ ("scaffoldCommit", toGVal p.iwpScaffoldCommit)
    , ("interfaceFile", toGVal p.iwpInterfaceFile)
    , ("contractSuite", toGVal p.iwpContractSuite)
    , ("testPlan", list (toGVal <$> p.iwpTestPlan))
    ]

instance ToGVal GingerM PlannedTest where
  toGVal t = dict
    [ ("criterionId", toGVal t.ptCriterionId)
    , ("name", toGVal t.ptName)
    , ("approach", toGVal t.ptApproach)
    ]

instance ToGVal GingerM TestsReadyPayload where
  toGVal p = dict
    [ ("commit", toGVal p.trpCommit)
    , ("testFiles", list (toGVal <$> p.trpTestFiles))
    , ("pendingCriteria", list (toGVal <$> p.trpPendingCriteria))
    ]

instance ToGVal GingerM ImplResult where
  toGVal r = dict
    [ ("commitHash", toGVal r.irCommitHash)
    , ("iterations", toGVal r.irIterations)
    , ("passedTests", list (toGVal <$> r.irPassedTests))
    ]

instance ToGVal GingerM Critique where
  toGVal c = dict
    [ ("file", toGVal c.cqFile)
    , ("line", toGVal c.cqLine)
    , ("issue", toGVal c.cqIssue)
    , ("requiredFix", toGVal c.cqRequiredFix)
    ]

instance ToGVal GingerM MergeComplete where
  toGVal m = dict
    [ ("commit", toGVal m.mcCommit)
    , ("author", toGVal m.mcAuthor)
    , ("impactLevel", toGVal (T.pack (show m.mcImpactLevel)))
    , ("changes", list (toGVal <$> m.mcChanges))
    ]

instance ToGVal GingerM ChangeEntry where
  toGVal c = dict
    [ ("symbol", toGVal c.ceSymbol)
    , ("type", toGVal (T.pack (show c.ceType)))
    , ("reason", toGVal c.ceReason)
    ]

instance ToGVal GingerM NodeInfo where
  toGVal n = dict
    [ ("id", toGVal n.niId)
    , ("branch", toGVal n.niBranch)
    ]

instance ToGVal GingerM TDDApproval where
  toGVal a = dict
    [ ("signOff", toGVal a.taSignOff)
    , ("coverageReport", toGVal a.taCoverageReport)
    ]

instance ToGVal GingerM CoverageReport where
  toGVal r = dict
    [ ("criteriaWithTests", list (toGVal <$> r.crCriteriaWithTests))
    , ("criteriaMissing", list (toGVal <$> r.crCriteriaMissing))
    ]

instance ToGVal GingerM MergeEvent where
  toGVal e = dict
    [ ("author", toGVal e.meAuthor)
    , ("impactLevel", toGVal (T.pack (show e.meImpactLevel)))
    , ("changes", list (toGVal <$> e.meChanges))
    ]
