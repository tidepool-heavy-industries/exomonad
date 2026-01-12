{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Template context for the single Work node.
--
-- The context includes:
-- - Spec and depth info (from WorkInput)
-- - CompletedChild (injected by effect when resuming from AwaitNext)
-- - PendingCount (injected by effect, number of children still running)
module TypesFirstDev.WorkContext
  ( -- * Template Context
    WorkTemplateCtx(..)
    -- * Completed Child Info
  , CompletedChildCtx(..)
  , ChildOutcome(..)
  , mkCompletedChild
  ) where

import Control.Monad.Writer (Writer)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..), dict, list)
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

import TypesFirstDev.Types.Core (Spec(..), Criterion(..), Constraints(..))
import TypesFirstDev.Types.Work (ChildSpec(..))

-- | Type alias for the ginger monad
type GingerM = Run SourcePos (Writer Text) Text

--------------------------------------------------------------------------------
-- Completed Child Info
--------------------------------------------------------------------------------

-- | Outcome of a child execution - success, failure, or plan revision needed.
data ChildOutcome
  = ChildSuccess { coCommitHash :: Text }
  | ChildFailure { coErrorMessage :: Text, coErrorDetails :: Maybe Text }
  | ChildPlanRevision
      { cprIssue :: Text
      , cprDiscovery :: Text
      , cprProposedChange :: Text
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToGVal GingerM ChildOutcome where
  -- All fields present in all cases (null for non-applicable) for typed template compatibility
  toGVal (ChildSuccess hash) = dict
    [ ("success", toGVal True)
    , ("failed", toGVal False)
    , ("needsPlanRevision", toGVal False)
    , ("commitHash", toGVal hash)
    , ("errorMessage", toGVal (Nothing :: Maybe Text))
    , ("errorDetails", toGVal (Nothing :: Maybe Text))
    , ("revisionIssue", toGVal (Nothing :: Maybe Text))
    , ("revisionDiscovery", toGVal (Nothing :: Maybe Text))
    , ("revisionProposedChange", toGVal (Nothing :: Maybe Text))
    ]
  toGVal (ChildFailure msg details) = dict
    [ ("success", toGVal False)
    , ("failed", toGVal True)
    , ("needsPlanRevision", toGVal False)
    , ("commitHash", toGVal (Nothing :: Maybe Text))
    , ("errorMessage", toGVal msg)
    , ("errorDetails", toGVal details)
    , ("revisionIssue", toGVal (Nothing :: Maybe Text))
    , ("revisionDiscovery", toGVal (Nothing :: Maybe Text))
    , ("revisionProposedChange", toGVal (Nothing :: Maybe Text))
    ]
  toGVal (ChildPlanRevision issue discovery proposed) = dict
    [ ("success", toGVal False)
    , ("failed", toGVal False)
    , ("needsPlanRevision", toGVal True)
    , ("commitHash", toGVal (Nothing :: Maybe Text))
    , ("errorMessage", toGVal (Nothing :: Maybe Text))
    , ("errorDetails", toGVal (Nothing :: Maybe Text))
    , ("revisionIssue", toGVal issue)
    , ("revisionDiscovery", toGVal discovery)
    , ("revisionProposedChange", toGVal proposed)
    ]

-- | Information about a child that just completed.
--
-- Flattened record for typed template compatibility.
-- All fields present; use success/failed/needsPlanRevision to know which are valid.
data CompletedChildCtx = CompletedChildCtx
  { cccDirective :: Text        -- ^ The original directive given to this child
  , cccSuccess :: Bool          -- ^ True if child succeeded
  , cccFailed :: Bool           -- ^ True if child failed (unexpected error)
  , cccNeedsPlanRevision :: Bool  -- ^ True if child needs plan revision
  , cccCommitHash :: Maybe Text   -- ^ Present if success
  , cccErrorMessage :: Maybe Text -- ^ Present if failed
  , cccErrorDetails :: Maybe Text -- ^ Optional additional error info
  , cccRevisionIssue :: Maybe Text        -- ^ Present if plan revision needed
  , cccRevisionDiscovery :: Maybe Text    -- ^ Present if plan revision needed
  , cccRevisionProposedChange :: Maybe Text -- ^ Present if plan revision needed
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Smart constructor from ChildOutcome
mkCompletedChild :: Text -> ChildOutcome -> CompletedChildCtx
mkCompletedChild directive outcome = case outcome of
  ChildSuccess hash -> CompletedChildCtx
    { cccDirective = directive
    , cccSuccess = True
    , cccFailed = False
    , cccNeedsPlanRevision = False
    , cccCommitHash = Just hash
    , cccErrorMessage = Nothing
    , cccErrorDetails = Nothing
    , cccRevisionIssue = Nothing
    , cccRevisionDiscovery = Nothing
    , cccRevisionProposedChange = Nothing
    }
  ChildFailure msg details -> CompletedChildCtx
    { cccDirective = directive
    , cccSuccess = False
    , cccFailed = True
    , cccNeedsPlanRevision = False
    , cccCommitHash = Nothing
    , cccErrorMessage = Just msg
    , cccErrorDetails = details
    , cccRevisionIssue = Nothing
    , cccRevisionDiscovery = Nothing
    , cccRevisionProposedChange = Nothing
    }
  ChildPlanRevision issue discovery proposed -> CompletedChildCtx
    { cccDirective = directive
    , cccSuccess = False
    , cccFailed = False
    , cccNeedsPlanRevision = True
    , cccCommitHash = Nothing
    , cccErrorMessage = Nothing
    , cccErrorDetails = Nothing
    , cccRevisionIssue = Just issue
    , cccRevisionDiscovery = Just discovery
    , cccRevisionProposedChange = Just proposed
    }

instance ToGVal GingerM CompletedChildCtx where
  toGVal c = dict
    [ ("directive", toGVal c.cccDirective)
    , ("success", toGVal c.cccSuccess)
    , ("failed", toGVal c.cccFailed)
    , ("needsPlanRevision", toGVal c.cccNeedsPlanRevision)
    , ("commitHash", toGVal c.cccCommitHash)
    , ("errorMessage", toGVal c.cccErrorMessage)
    , ("errorDetails", toGVal c.cccErrorDetails)
    , ("revisionIssue", toGVal c.cccRevisionIssue)
    , ("revisionDiscovery", toGVal c.cccRevisionDiscovery)
    , ("revisionProposedChange", toGVal c.cccRevisionProposedChange)
    ]

--------------------------------------------------------------------------------
-- Work Template Context
--------------------------------------------------------------------------------

-- | Context for the Work template.
--
-- The template uses this to render context-appropriate prompts:
-- - Fresh start: no completedChild, no pendingCount
-- - After Spawn: pendingCount > 0, no completedChild yet
-- - After AwaitNext: completedChild populated, pendingCount updated
-- - Near completion: pendingCount = 0 after processing all children
data WorkTemplateCtx = WorkTemplateCtx
  { spec              :: Spec                   -- ^ What we're building
  , depth             :: Int                    -- ^ Current recursion depth
  , maxDepth          :: Int                    -- ^ Maximum allowed depth
  , completedChild    :: Maybe CompletedChildCtx -- ^ Just completed child (if resuming from AwaitNext)
  , pendingCount      :: Int                    -- ^ Number of children still running
  , allCompleted      :: [CompletedChildCtx]    -- ^ All children completed so far
  -- Derived fields for template convenience
  , isRoot            :: Bool                   -- ^ depth == 0
  , atMaxDepth        :: Bool                   -- ^ depth >= maxDepth
  , hasCompletedChild :: Bool                   -- ^ completedChild /= Nothing
  , hasPendingChildren :: Bool                  -- ^ pendingCount > 0
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance ToGVal GingerM WorkTemplateCtx where
  toGVal ctx = dict
    [ ("spec", toGVal ctx.spec)
    , ("depth", toGVal ctx.depth)
    , ("maxDepth", toGVal ctx.maxDepth)
    , ("completedChild", toGVal ctx.completedChild)
    , ("pendingCount", toGVal ctx.pendingCount)
    , ("allCompleted", list (toGVal <$> ctx.allCompleted))
    , ("isRoot", toGVal ctx.isRoot)
    , ("atMaxDepth", toGVal ctx.atMaxDepth)
    , ("hasCompletedChild", toGVal ctx.hasCompletedChild)
    , ("hasPendingChildren", toGVal ctx.hasPendingChildren)
    ]

--------------------------------------------------------------------------------
-- ToGVal instances for imported types
--------------------------------------------------------------------------------

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
    [ ("time", toGVal c.cnTime)
    , ("space", toGVal c.cnSpace)
    ]

instance ToGVal GingerM Criterion where
  toGVal c = dict
    [ ("id", toGVal c.cId)
    , ("text", toGVal c.cText)
    ]

instance ToGVal GingerM ChildSpec where
  toGVal cs = dict
    [ ("directive", toGVal cs.csDirective)
    , ("boundary", toGVal cs.csBoundary)
    ]
