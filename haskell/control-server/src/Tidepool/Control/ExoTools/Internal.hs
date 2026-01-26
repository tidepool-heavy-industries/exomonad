{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Tidepool.Control.ExoTools.Internal
  ( ExoStatusResult(..)
  , getDevelopmentContext
  , parseIssueNumber
  , slugify
  , formatPRBody
  , extractIssueNumber
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Freer (Eff, Member)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Char (isAlphaNum, isSpace)
import Data.List (find, sortOn)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Text.Parsec.Pos (SourcePos)

import Tidepool.Control.ExoTools.FilePR.Context (PRBodyContext(..), PRDepContext(..))
import Tidepool.Graph.Template (TypedTemplate, typedTemplateFile, runTypedTemplate)

import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo, getDirtyFiles)
import Tidepool.Effects.GitHub
  ( GitHub, PullRequest(..), Issue(..), listPullRequests, getIssue
  , Repo(..), PRFilter(..), defaultPRFilter, IssueState(..), listIssues, defaultIssueFilter, IssueFilter(..)
  )

-- | Brief issue info for summaries.
data IssueBrief = IssueBrief
  { ibNumber :: Int
  , ibTitle :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON IssueBrief where
  toJSON b = object
    [ "number" .= ibNumber b
    , "title" .= ibTitle b
    ]

-- | Sprint summary info.
data SprintSummary = SprintSummary
  { ssTotalOpen :: Int
  , ssOpenIssues :: [IssueBrief]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SprintSummary where
  toJSON s = object
    [ "total_open" .= ssTotalOpen s
    , "open_issues" .= ssOpenIssues s
    ]

-- | Result of exo_status tool.
data ExoStatusResult = ExoStatusResult
  { esrIssue :: Maybe Issue
  , esrWorktree :: Maybe WorktreeInfo
  , esrDirtyFiles :: [FilePath]
  , esrPR :: Maybe PullRequest
  , esrSprintSummary :: Maybe SprintSummary
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ExoStatusResult where
  toJSON res = object
    [ "issue" .= esrIssue res
    , "worktree" .= esrWorktree res
    , "dirty_files" .= esrDirtyFiles res
    , "pr" .= esrPR res
    , "sprint_summary" .= esrSprintSummary res
    ]

getDevelopmentContext
  :: (Member Git es, Member GitHub es)
  => Maybe Text
  -> Eff es ExoStatusResult
getDevelopmentContext maybeIssueId = do
  -- 1. Get Worktree/Git info
  mWt <- getWorktreeInfo
  dirtyFiles <- getDirtyFiles

  -- TODO: Configurable repo
  let repo = Repo "tidepool/tidepool"

  -- 2. Determine Issue Number
  let branchIssueNum = case mWt of
        Just wt -> parseIssueNumber wt.wiBranch
        Nothing -> Nothing
      targetIssueNum = (maybeIssueId >>= parseNum) <|> branchIssueNum
      parseNum t = case (reads (T.unpack t) :: [(Int, String)]) of
        [(n, "")] -> Just n
        _ -> Nothing

  -- 3. Get Issue Info
  mIssue <- case targetIssueNum of
    Just num -> getIssue repo num False
    Nothing -> pure Nothing

  -- 4. Get PR Info
  mPR <- case mWt of
    Just wt -> do
      let filt = defaultPRFilter { pfBase = Just "main", pfLimit = Just 100 }
      prs <- listPullRequests repo filt
      pure $ find (\pr -> pr.prHeadRefName == wt.wiBranch) prs
    Nothing -> pure Nothing

  -- 5. Get Sprint Summary if no issue context
  mSprintSummary <- case mIssue of
    Just _ -> pure Nothing
    Nothing -> do
      allIssues <- listIssues repo (defaultIssueFilter { ifState = Just IssueOpen })
      let toIssueBrief i = IssueBrief
            { ibNumber = i.issueNumber
            , ibTitle = i.issueTitle
            }

      pure $ Just SprintSummary
        { ssTotalOpen = length allIssues
        , ssOpenIssues = map toIssueBrief $ take 10 allIssues
        }

  pure ExoStatusResult
    { esrIssue = mIssue
    , esrWorktree = mWt
    , esrDirtyFiles = dirtyFiles
    , esrPR = mPR
    , esrSprintSummary = mSprintSummary
    }

-- | Parse issue number from branch name (gh-{num}/* convention)
parseIssueNumber :: Text -> Maybe Int
parseIssueNumber branch =
  if "gh-" `T.isPrefixOf` branch
  then
    let content = T.drop 3 branch
        (numStr, _) = T.break (== '/') content
    in case (reads (T.unpack numStr) :: [(Int, String)]) of
         [(n, "")] -> Just n
         _ -> Nothing
  else Nothing

-- | Slugify a title for use in branch/directory names.
-- Returns "untitled" if the input produces no valid slug.
slugify :: Text -> Text
slugify title =
  let cleaned = T.filter (\c -> isAlphaNum c || isSpace c) title
      parts = filter (not . T.null) $ T.words cleaned
  in if null parts
     then "untitled"
     else T.intercalate "-" $ map T.toLower parts

-- | Compiled PR body template.
prBodyTemplate :: TypedTemplate PRBodyContext SourcePos
prBodyTemplate = $(typedTemplateFile ''PRBodyContext "templates/hook/pr-body.jinja")

-- | Format PR body from issue info with testing and compromises sections.
formatPRBody :: Issue -> Text -> Maybe Text -> Text
formatPRBody issue testing compromises = runTypedTemplate ctx prBodyTemplate
  where
    ctx = PRBodyContext
      { issue_number = T.pack (show issue.issueNumber)
      , description = Just issue.issueBody
      , testing = testing
      , compromises = compromises
      , dependencies = [] -- TODO: Parse from body if needed
      , dependents = []
      }

-- | Extract issue number from PR title.
-- Pattern: [gh-XXX]
extractIssueNumber :: Text -> Maybe Int
extractIssueNumber title =
  let (_, rest) = T.breakOn "[gh-" title
  in if T.null rest
     then Nothing
     else
       let (numStr, _) = T.break (== ']') (T.drop 4 rest)
       in case (reads (T.unpack numStr) :: [(Int, String)]) of
            [(n, "")] -> Just n
            _ -> Nothing
