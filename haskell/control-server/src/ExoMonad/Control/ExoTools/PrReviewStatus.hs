{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.ExoTools.PrReviewStatus
  ( PrReviewStatusGraph (..),
    prReviewStatusHandlers,
    prReviewStatusLogic,
    PrReviewStatusArgs (..),
    PrReviewStatusResult (..),
    AuthorFeedback (..),
    FeedbackSummary (..),
  )
where

import Control.Monad.Freer (Eff, Member)
import Data.List (partition)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day (ModifiedJulianDay), UTCTime (..))
import ExoMonad.Control.ExoTools.PrReviewStatus.Types
import ExoMonad.Effects.GitHub (Author (..), GitHub, PullRequest (..), Review (..), ReviewComment (..), ReviewState (..), defaultRepo, getPullRequest, getPullRequestReviews)
import ExoMonad.Graph.Generic (AsHandler, type (:-))
import ExoMonad.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import ExoMonad.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import ExoMonad.Graph.Types (Exit, Input, MCPExport, MCPToolDef, UsesEffects, type (:@))
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for pr_review_status tool.
data PrReviewStatusGraph mode = PrReviewStatusGraph
  { prsEntry ::
      mode
        :- EntryNode PrReviewStatusArgs
        :@ MCPExport
        :@ MCPToolDef '("pr_review_status", "Get comprehensive PR feedback: Copilot vs human comments, pending vs resolved, with summary counts."),
    prsRun ::
      mode
        :- LogicNode
        :@ Input PrReviewStatusArgs
        :@ UsesEffects '[GitHub, Goto Exit PrReviewStatusResult],
    prsExit :: mode :- ExitNode PrReviewStatusResult
  }
  deriving (Generic)

-- | Handlers for pr_review_status graph.
prReviewStatusHandlers ::
  (Member GitHub es) =>
  PrReviewStatusGraph (AsHandler es)
prReviewStatusHandlers =
  PrReviewStatusGraph
    { prsEntry = (),
      prsRun = prReviewStatusLogic,
      prsExit = ()
    }

-- ════════════════════════════════════════════════════════════════════════════
-- LOGIC
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert a PR-level Review to a ReviewComment.
-- PR-level reviews have no file path or line number.
reviewToComment :: Review -> ReviewComment
reviewToComment (Review author body state) =
  let Author login _ = author
      -- Intentionally normalize review timestamps to the Unix epoch.
      -- This tool does not use timestamps for sorting or grouping, so a fixed value
      -- is sufficient. If timestamps become semantically important, pass through
      -- the real created-at time instead of this placeholder.
      createdAt = UTCTime (ModifiedJulianDay 0) 0
      -- Treat APPROVED and DISMISSED as resolved.
      isResolved = state `elem` [ReviewApproved, ReviewDismissed]
   in ReviewComment login body Nothing Nothing state createdAt isResolved

-- | Check if an author is Copilot or a bot.
isCopilotAuthor :: Text -> Bool
isCopilotAuthor author =
  author == "copilot" || author == "github-actions[bot]" || "copilot" `T.isInfixOf` T.toLower author

-- | Check if a comment is resolved.
isCommentResolved :: ReviewComment -> Bool
isCommentResolved c = c.rcIsResolved

-- | Extract author from comment.
commentAuthor :: ReviewComment -> Text
commentAuthor c = c.rcAuthor

-- | Partition comments by author type and resolution status.
partitionComments :: [ReviewComment] -> (AuthorFeedback, AuthorFeedback)
partitionComments comments =
  let (copilotComments, humanComments) = partition (isCopilotAuthor . commentAuthor) comments
      (copilotPending, copilotResolved) = partition (not . isCommentResolved) copilotComments
      (humanPending, humanResolved) = partition (not . isCommentResolved) humanComments
   in ( AuthorFeedback copilotPending copilotResolved,
        AuthorFeedback humanPending humanResolved
      )

-- | Build summary counts from feedback.
buildSummary :: AuthorFeedback -> AuthorFeedback -> FeedbackSummary
buildSummary copilot humans =
  FeedbackSummary
    { copilotPending = length (copilot.pending),
      copilotResolved = length (copilot.resolved),
      humanPending = length (humans.pending),
      humanResolved = length (humans.resolved)
    }

-- | Core logic for pr_review_status.
-- Fetches both inline review comments and PR-level reviews, then consolidates them.
prReviewStatusLogic ::
  (Member GitHub es) =>
  PrReviewStatusArgs ->
  Eff es (GotoChoice '[To Exit PrReviewStatusResult])
prReviewStatusLogic args = do
  let repo = defaultRepo

  -- Fetch inline review comments (code-specific feedback)
  inlineCommentsResult <- getPullRequestReviews repo args.prNumber
  let inlineComments = case inlineCommentsResult of
        Left _err -> []
        Right cs -> cs

  -- Fetch PR-level reviews (general feedback on the PR)
  maybePrResult <- getPullRequest repo args.prNumber True
  let prLevelReviews = case maybePrResult of
        Left _err -> []
        Right Nothing -> []
        Right (Just pr) -> map reviewToComment pr.prReviews

  -- Combine all feedback
  let allComments = inlineComments ++ prLevelReviews

  -- Partition by author type and resolution status
  let (copilotFeedback, humanFeedback) = partitionComments allComments
      summary = buildSummary copilotFeedback humanFeedback

  pure $
    gotoExit
      PrReviewStatusResult
        { prNumber = args.prNumber,
          copilot = copilotFeedback,
          humans = humanFeedback,
          summary = summary
        }
