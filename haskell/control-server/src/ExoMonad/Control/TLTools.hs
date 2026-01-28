{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Team Lead tools logic.
module ExoMonad.Control.TLTools
  ( tlCreateIssueLogic
  , TLCreateIssueArgs(..)
  , TLCreateIssueResult(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import ExoMonad.Effects.GitHub
  ( GitHub, CreateIssueInput(..), createIssue, defaultCreateIssueInput, Repo(..)
  )
import ExoMonad.Effects.Env (Env)
import ExoMonad.Control.TLTools.Types
import ExoMonad.Control.GHTools (getRepo)

-- ════════════════════════════════════════════════════════════════════════════
-- TL CREATE ISSUE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for tl_create_issue.
tlCreateIssueLogic
  :: (Member GitHub es, Member Env es)
  => TLCreateIssueArgs
  -> Eff es TLCreateIssueResult
tlCreateIssueLogic args = do
  -- Use default repo or env repo
  repo <- getRepo Nothing

  let body = formatIssueBody args
      labels = constructLabels args
      assignees = maybe [] (.assignees) args.assignment
      input = (defaultCreateIssueInput repo args.title)
        { ciiBody      = body
        , ciiLabels    = labels
        , ciiAssignees = assignees
        }

  res <- createIssue input
  case res of
    Left err -> pure $ TLCreateIssueResult
      { number  = 0
      , url     = ""
      , success = False
      , error   = Just (T.pack $ show err)
      }
    Right num -> pure $ TLCreateIssueResult
      { number  = num
      , url     = "https://github.com/" <> repo.unRepo <> "/issues/" <> T.pack (show num)
      , success = True
      , error   = Nothing
      }

-- | Format the issue body from nested structured fields.
formatIssueBody :: TLCreateIssueArgs -> Text
formatIssueBody args = T.unlines $ filter (not . T.null)
  [ "**Category:** " <> T.pack (show args.classification.category)
  , "**Priority:** " <> T.pack (show args.classification.priority)
  , maybe "" (\s -> "**Severity:** " <> T.pack (show s)) args.classification.severity
  , if null args.classification.components
      then ""
      else "**Components:** " <> T.intercalate ", " (map (T.pack . show) args.classification.components)
  , ""
  , "## Description"
  , args.content.description
  , ""
  , maybe "" (\r -> "## Reproduction Steps\n" <> r <> "\n") args.content.reproduction
  , if null args.content.acceptanceCriteria
      then ""
      else "## Acceptance Criteria\n" <> formatCriteria args.content.acceptanceCriteria
  ]

-- | Format acceptance criteria as GitHub markdown checklist.
formatCriteria :: [Criterion] -> Text
formatCriteria = T.unlines . map formatCriterion
  where
    formatCriterion c =
      "- [ ] " <> c.item <> if c.testable then " *(testable)*" else ""

-- | Construct labels from classification fields.
constructLabels :: TLCreateIssueArgs -> [Text]
constructLabels args =
  [ T.toLower (T.pack $ show args.classification.category)
  , "priority:" <> T.toLower (T.pack $ show args.classification.priority)
  ]
  ++ map (T.toLower . T.pack . show) args.classification.components
  ++ fromMaybe [] (args.assignment >>= (.labels))
