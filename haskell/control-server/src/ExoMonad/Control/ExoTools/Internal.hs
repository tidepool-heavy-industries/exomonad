{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ExoMonad.Control.ExoTools.Internal
  ( ExoStatusResult (..),
    getDevelopmentContext,
    parseIssueNumber,
    slugify,
    formatPRBody,
    extractIssueNumber,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Freer (Eff, Member)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Control.ExoTools.FilePR.Context (PRBodyContext (..))
import ExoMonad.Effects.Git (Git, WorktreeInfo (..), getDirtyFiles, getWorktreeInfo)
import ExoMonad.Effects.GitHub
  ( GitHub,
    Issue (..),
    IssueFilter (..),
    IssueState (..),
    PRFilter (..),
    PullRequest (..),
    defaultIssueFilter,
    defaultPRFilter,
    defaultRepo,
    getIssue,
    listIssues,
    listPullRequests,
  )
import ExoMonad.Graph.Template (TypedTemplate, runTypedTemplate, typedTemplateFile)
import GHC.Generics (Generic)
import Text.Parsec.Pos (SourcePos)
import Text.Read (readMaybe)

-- | Brief issue info for summaries.
data IssueBrief = IssueBrief
  { number :: Int,
    title :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON IssueBrief where
  toJSON b =
    object
      [ "number" .= b.number,
        "title" .= b.title
      ]

-- | Sprint summary info.
data SprintSummary = SprintSummary
  { totalOpen :: Int,
    openIssues :: [IssueBrief]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SprintSummary where
  toJSON s =
    object
      [ "total_open" .= s.totalOpen,
        "open_issues" .= s.openIssues
      ]

-- | Result of exo_status tool.
data ExoStatusResult = ExoStatusResult
  { issue :: Maybe Issue,
    worktree :: Maybe WorktreeInfo,
    dirtyFiles :: [FilePath],
    pr :: Maybe PullRequest,
    sprintSummary :: Maybe SprintSummary
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ExoStatusResult where
  toJSON res =
    object
      [ "issue" .= res.issue,
        "worktree" .= res.worktree,
        "dirty_files" .= res.dirtyFiles,
        "pr" .= res.pr,
        "sprint_summary" .= res.sprintSummary
      ]

getDevelopmentContext ::
  (Member Git es, Member GitHub es) =>
  Maybe Text ->
  Eff es ExoStatusResult
getDevelopmentContext maybeIssueId = do
  -- 1. Get Worktree/Git info
  mWt <- getWorktreeInfo
  dirtyFiles <- getDirtyFiles

  let repo = defaultRepo

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
    Just num -> do
      result <- getIssue repo num False
      pure $ case result of
        Left _err -> Nothing
        Right mi -> mi
    Nothing -> pure Nothing

  -- 4. Get PR Info
  mPR <- case mWt of
    Just wt -> do
      let filt = defaultPRFilter {pfBase = Just "main", pfLimit = Just 100}
      prsResult <- listPullRequests repo filt
      let prs = case prsResult of
            Left _err -> []
            Right ps -> ps
      pure $ find (\pr -> pr.prHeadRefName == wt.wiBranch) prs
    Nothing -> pure Nothing

  -- 5. Get Sprint Summary if no issue context
  mSprintSummary <- case mIssue of
    Just _ -> pure Nothing
    Nothing -> do
      allIssuesResult <- listIssues repo (defaultIssueFilter {ifState = Just IssueOpen})
      let allIssues = case allIssuesResult of
            Left _err -> []
            Right is -> is
          toIssueBrief i =
            IssueBrief
              { number = i.issueNumber,
                title = i.issueTitle
              }

      pure $
        Just
          SprintSummary
            { totalOpen = length allIssues,
              openIssues = map toIssueBrief $ take 10 allIssues
            }

  pure
    ExoStatusResult
      { issue = mIssue,
        worktree = mWt,
        dirtyFiles = dirtyFiles,
        pr = mPR,
        sprintSummary = mSprintSummary
      }

-- | Parse issue number from branch name (gh-{num}/* convention)
parseIssueNumber :: Text -> Maybe Int
parseIssueNumber branch =
  if "gh-" `T.isPrefixOf` T.toLower branch
    then
      let content = T.drop 3 branch
          (numStr, rest) = T.span isDigit content
       in if T.null numStr
            then Nothing
            else case T.uncons rest of
              Nothing -> readMaybe (T.unpack numStr)
              Just (c, _) | c == '/' || c == '-' -> readMaybe (T.unpack numStr)
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
    ctx =
      PRBodyContext
        { issue_number = T.pack (show issue.issueNumber),
          description = Just issue.issueBody,
          testing = testing,
          compromises = compromises,
          dependencies = [], -- TODO: Parse from body if needed
          dependents = []
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
