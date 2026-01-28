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
      assignees = maybe [] asAssignees args.tliaAssignment
      input = (defaultCreateIssueInput repo args.tliaTitle)
        { ciiBody      = body
        , ciiLabels    = labels
        , ciiAssignees = assignees
        }

  res <- createIssue input
  case res of
    Left err -> pure $ TLCreateIssueResult
      { tlcrNumber  = 0
      , tlcrUrl     = ""
      , tlcrSuccess = False
      , tlcrError   = Just (T.pack $ show err)
      }
    Right num -> pure $ TLCreateIssueResult
      { tlcrNumber  = num
      , tlcrUrl     = "https://github.com/" <> repo.unRepo <> "/issues/" <> T.pack (show num)
      , tlcrSuccess = True
      , tlcrError   = Nothing
      }

-- | Format the issue body from nested structured fields.
formatIssueBody :: TLCreateIssueArgs -> Text
formatIssueBody args = T.unlines $ filter (not . T.null)
  [ "**Category:** " <> T.pack (show args.tliaClassification.clCategory)
  , "**Priority:** " <> T.pack (show args.tliaClassification.clPriority)
  , maybe "" (\s -> "**Severity:** " <> T.pack (show s)) args.tliaClassification.clSeverity
  , if null args.tliaClassification.clComponents
      then ""
      else "**Components:** " <> T.intercalate ", " (map (T.pack . show) args.tliaClassification.clComponents)
  , ""
  , "## Description"
  , args.tliaContent.icDescription
  , ""
  , maybe "" (\r -> "## Reproduction Steps\n" <> r <> "\n") args.tliaContent.icReproduction
  , if null args.tliaContent.icAcceptanceCriteria
      then ""
      else "## Acceptance Criteria\n" <> formatCriteria args.tliaContent.icAcceptanceCriteria
  ]

-- | Format acceptance criteria as GitHub markdown checklist.
formatCriteria :: [Criterion] -> Text
formatCriteria = T.unlines . map formatCriterion
  where
    formatCriterion c =
      "- [ ] " <> c.crItem <> if c.crTestable then " *(testable)*" else ""

-- | Construct labels from classification fields.
constructLabels :: TLCreateIssueArgs -> [Text]
constructLabels args =
  [ T.toLower (T.pack $ show args.tliaClassification.clCategory)
  , "priority:" <> T.toLower (T.pack $ show args.tliaClassification.clPriority)
  ]
  ++ map (T.toLower . T.pack . show) args.tliaClassification.clComponents
  ++ fromMaybe [] (args.tliaAssignment >>= asLabels)
