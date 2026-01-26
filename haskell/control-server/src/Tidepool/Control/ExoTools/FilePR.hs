{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.ExoTools.FilePR
  ( FilePRGraph(..)
  , filePRHandlers
  , filePRLogic
  , FilePRArgs(..)
  , FilePRResult(..)
  , PRInfo(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), object, withObject)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo)
import Tidepool.Effects.GitHub (GitHub, Repo(..), PRCreateSpec(..), PRUrl(..), PullRequest(..), PRFilter(..), Issue(..), getIssue, createPR, listPullRequests, defaultPRFilter)
import qualified Data.Text as T
import Data.Maybe (listToMaybe)
import Tidepool.Role (Role(..))
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef, MCPRoleHint)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, emptySchema, SchemaType(..), describeField)

import Tidepool.Control.ExoTools.Internal (parseIssueNumber, slugify, formatPRBody)

-- | Arguments for file_pr tool.
-- Issue number and title are inferred from the branch - agent provides context.
data FilePRArgs = FilePRArgs
  { fpaTesting     :: Text        -- ^ Required: How was this tested?
  , fpaCompromises :: Maybe Text  -- ^ Optional: Tradeoffs or shortcuts taken
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema FilePRArgs where
  jsonSchema = objectSchema
    [ ("testing", describeField "testing" "How was this tested? What scenarios were verified?" (emptySchema TString))
    , ("compromises", describeField "compromises" "Any tradeoffs, shortcuts, or known limitations?" (emptySchema TString))
    ]
    ["testing"]  -- testing is required

instance FromJSON FilePRArgs where
  parseJSON = withObject "FilePRArgs" $ \v ->
    FilePRArgs <$> v .: "testing" <*> v .:? "compromises"

instance ToJSON FilePRArgs where
  toJSON args = object
    [ "testing" .= fpaTesting args
    , "compromises" .= fpaCompromises args
    ]

-- | PR Info for file_pr result.
data PRInfo = PRInfo
  { priNumber :: Int
  , priUrl    :: Text
  , priStatus :: Text
  , priTitle  :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Result of file_pr tool.
-- Returns PR info for either an existing or newly created PR.
data FilePRResult = FilePRResult
  { fprPr :: Maybe PRInfo      -- ^ PR info (existing or newly created)
  , fprCreated :: Bool         -- ^ True if we created a new PR, False if existing found
  , fprError :: Maybe Text     -- ^ Error message if operation failed
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON FilePRResult where
  toJSON res = object
    [ "pr" .= fprPr res
    , "created" .= fprCreated res
    , "error" .= fprError res
    ]

instance FromJSON FilePRResult where
  parseJSON = withObject "FilePRResult" $ \v ->
    FilePRResult
      <$> v .:? "pr"
      <*> v .: "created"
      <*> v .:? "error"

-- | Graph definition for file_pr tool.
data FilePRGraph mode = FilePRGraph
  { fpEntry :: mode :- EntryNode FilePRArgs
      :@ MCPExport
      :@ MCPToolDef '("file_pr", "File a pull request for current issue. Idempotent: returns existing PR if one exists.")
      :@ MCPRoleHint 'Dev

  , fpRun :: mode :- LogicNode
      :@ Input FilePRArgs
      :@ UsesEffects '[Git, GitHub, Goto Exit FilePRResult]

  , fpExit :: mode :- ExitNode FilePRResult
  }
  deriving Generic

-- | Handlers for file_pr graph.
filePRHandlers
  :: (Member Git es, Member GitHub es)
  => FilePRGraph (AsHandler es)
filePRHandlers = FilePRGraph
  { fpEntry = ()
  , fpRun = filePRLogic
  , fpExit = ()
  }

-- | Core logic for file_pr.
-- Issue number and title inferred from branch. Agent provides testing/compromises.
-- Idempotent: checks for existing PR first, returns it if found.
filePRLogic
  :: (Member Git es, Member GitHub es)
  => FilePRArgs
  -> Eff es (GotoChoice '[To Exit FilePRResult])
filePRLogic args = do
  -- 1. Get Worktree/Git info
  mWt <- getWorktreeInfo

  -- 2. Determine Issue Number from branch
  let mIssueNum = case mWt of
        Just wt -> parseIssueNumber wt.wiBranch
        Nothing -> Nothing

  case mIssueNum of
    Nothing ->
      pure $ gotoExit $ FilePRResult Nothing False (Just "Not on an issue branch. file_pr requires gh-{num}/* branch naming.")
    Just num -> do
      -- 3. Get Issue Info
      -- TODO: Configurable repo
      let repo = Repo "tidepool/tidepool"
      mIssue <- getIssue repo num False
      case mIssue of
        Nothing ->
          pure $ gotoExit $ FilePRResult Nothing False (Just $ "Issue #" <> T.pack (show num) <> " not found.")
        Just issue -> do
          -- 4. Check if PR already exists (idempotent)
          let searchStr = "[gh-" <> T.pack (show num) <> "]"
              filt = defaultPRFilter { pfSearch = Just searchStr, pfLimit = Just 1 }

          existingPrs <- listPullRequests repo filt
          case listToMaybe existingPrs of
            Just pr -> do
              -- PR already exists - return it (idempotent behavior)
              let info = PRInfo
                    { priNumber = pr.prNumber
                    , priUrl = pr.prUrl
                    , priStatus = T.pack (show pr.prState)
                    , priTitle = pr.prTitle
                    }
              pure $ gotoExit $ FilePRResult (Just info) False Nothing

            Nothing -> do
              -- 5. Prepare PR Spec
              let headBranch = case mWt of
                    Just wt -> wt.wiBranch
                    Nothing -> "gh-" <> T.pack (show num) <> "/" <> slugify issue.issueTitle

              let title = "[" <> "gh-" <> T.pack (show num) <> "] " <> issue.issueTitle
                  body = formatPRBody issue args.fpaTesting args.fpaCompromises
                  spec = PRCreateSpec
                    { prcsRepo = repo
                    , prcsHead = headBranch
                    , prcsBase = "main"
                    , prcsTitle = title
                    , prcsBody = body
                    }

              -- 6. Create PR
              PRUrl url <- createPR spec
              -- Note: We don't have the PR number from createPR, so we use 0
              -- In practice, the URL is what matters
              let info = PRInfo
                    { priNumber = 0
                    , priUrl = url
                    , priStatus = "OPEN"
                    , priTitle = title
                    }
              pure $ gotoExit $ FilePRResult (Just info) True Nothing
