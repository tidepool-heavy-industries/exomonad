{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.ExoTools.PrReviewStatus
  ( PrReviewStatusGraph(..)
  , prReviewStatusHandlers
  , prReviewStatusLogic
  , PrReviewStatusArgs(..)
  , PrReviewStatusResult(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime(..), Day(..))
import GHC.Generics (Generic)

import Tidepool.Effects.GitHub
  ( GitHub
  , Repo(..)
  , PullRequest(..)
  , ReviewComment(..)
  , ReviewState(..)
  , Review(..)
  , Author(..)
  , getPullRequestReviews
  , getPullRequest
  )
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, emptySchema, SchemaType(..), describeField)

-- | Arguments for pr_review_status tool.
data PrReviewStatusArgs = PrReviewStatusArgs
  { prsaPrNumber :: Int
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema PrReviewStatusArgs where
  jsonSchema = objectSchema
    [ ("pr_number", describeField "pr_number" "Pull Request number." (emptySchema TInteger))
    ]
    ["pr_number"]

instance FromJSON PrReviewStatusArgs where
  parseJSON = withObject "PrReviewStatusArgs" $ \v ->
    PrReviewStatusArgs <$> v .: "pr_number"

instance ToJSON PrReviewStatusArgs where
  toJSON args = object ["pr_number" .= prsaPrNumber args]

-- | Feedback from a single author category (Copilot or human).
data AuthorFeedback = AuthorFeedback
  { afPending :: [ReviewComment]
  , afResolved :: [ReviewComment]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON AuthorFeedback where
  toJSON af = object
    [ "pending" .= afPending af
    , "resolved" .= afResolved af
    ]

instance FromJSON AuthorFeedback where
  parseJSON = withObject "AuthorFeedback" $ \v ->
    AuthorFeedback
      <$> v .: "pending"
      <*> v .: "resolved"

-- | Summary counts for quick triage.
data FeedbackSummary = FeedbackSummary
  { fsCopilotPending :: Int
  , fsCopilotResolved :: Int
  , fsHumanPending :: Int
  , fsHumanResolved :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON FeedbackSummary where
  toJSON fs = object
    [ "copilot_pending" .= fsCopilotPending fs
    , "copilot_resolved" .= fsCopilotResolved fs
    , "human_pending" .= fsHumanPending fs
    , "human_resolved" .= fsHumanResolved fs
    ]

instance FromJSON FeedbackSummary where
  parseJSON = withObject "FeedbackSummary" $ \v ->
    FeedbackSummary
      <$> v .: "copilot_pending"
      <*> v .: "copilot_resolved"
      <*> v .: "human_pending"
      <*> v .: "human_resolved"

-- | Result of pr_review_status tool.
-- Returns comprehensive feedback grouped by author type and status.
data PrReviewStatusResult = PrReviewStatusResult
  { prsrPrNumber :: Int
  , prsrCopilot :: AuthorFeedback
  , prsrHumans :: AuthorFeedback
  , prsrSummary :: FeedbackSummary
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PrReviewStatusResult where
  toJSON res = object
    [ "pr_number" .= prsrPrNumber res
    , "copilot" .= prsrCopilot res
    , "humans" .= prsrHumans res
    , "summary" .= prsrSummary res
    ]

instance FromJSON PrReviewStatusResult where
  parseJSON = withObject "PrReviewStatusResult" $ \v ->
    PrReviewStatusResult
      <$> v .: "pr_number"
      <*> v .: "copilot"
      <*> v .: "humans"
      <*> v .: "summary"

-- | Graph definition for pr_review_status tool.
data PrReviewStatusGraph mode = PrReviewStatusGraph
  { prsEntry :: mode :- EntryNode PrReviewStatusArgs
      :@ MCPExport
      :@ MCPToolDef '("pr_review_status", "Get comprehensive PR feedback: Copilot vs human comments, pending vs resolved, with summary counts.")

  , prsRun :: mode :- LogicNode
      :@ Input PrReviewStatusArgs
      :@ UsesEffects '[GitHub, Goto Exit PrReviewStatusResult]

  , prsExit :: mode :- ExitNode PrReviewStatusResult
  }
  deriving Generic

-- | Handlers for pr_review_status graph.
prReviewStatusHandlers
  :: (Member GitHub es)
  => PrReviewStatusGraph (AsHandler es)
prReviewStatusHandlers = PrReviewStatusGraph
  { prsEntry = ()
  , prsRun = prReviewStatusLogic
  , prsExit = ()
  }

-- | Convert a PR-level Review to a ReviewComment.
-- PR-level reviews have no file path or line number.
reviewToComment :: Review -> ReviewComment
reviewToComment (Review author body state) =
  let Author login _ = author
      -- Use epoch as placeholder timestamp since Review type doesn't include createdAt
      -- This is fine for our purposes since we're not using timestamp for grouping
      createdAt = UTCTime (ModifiedJulianDay 0) 0
  in ReviewComment login body Nothing Nothing state createdAt

-- | Check if an author is Copilot or a bot.
isCopilotAuthor :: Text -> Bool
isCopilotAuthor author =
  author == "copilot" || author == "github-actions[bot]" || "copilot" `T.isInfixOf` T.toLower author

-- | Check if a comment is resolved.
-- For now, we use a heuristic: comments with state other than Commented/Pending are considered resolved.
isCommentResolved :: ReviewComment -> Bool
isCommentResolved (ReviewComment _ _ _ _ state _) =
  case state of
    ReviewCommented -> False
    ReviewPending -> False
    _ -> True

-- | Extract author from comment.
commentAuthor :: ReviewComment -> Text
commentAuthor (ReviewComment author _ _ _ _ _) = author

-- | Partition comments by author type and resolution status.
partitionComments :: [ReviewComment] -> (AuthorFeedback, AuthorFeedback)
partitionComments comments =
  let (copilotComments, humanComments) = partitionBy (isCopilotAuthor . commentAuthor) comments
      (copilotPending, copilotResolved) = partitionBy (not . isCommentResolved) copilotComments
      (humanPending, humanResolved) = partitionBy (not . isCommentResolved) humanComments
  in ( AuthorFeedback copilotPending copilotResolved
     , AuthorFeedback humanPending humanResolved
     )
  where
    partitionBy predicate xs = foldr (\x (ts, fs) -> if predicate x then (x:ts, fs) else (ts, x:fs)) ([], []) xs

-- | Build summary counts from feedback.
buildSummary :: AuthorFeedback -> AuthorFeedback -> FeedbackSummary
buildSummary copilot humans =
  FeedbackSummary
    { fsCopilotPending = length (afPending copilot)
    , fsCopilotResolved = length (afResolved copilot)
    , fsHumanPending = length (afPending humans)
    , fsHumanResolved = length (afResolved humans)
    }

-- | Core logic for pr_review_status.
-- Fetches both inline review comments and PR-level reviews, then consolidates them.
prReviewStatusLogic
  :: (Member GitHub es)
  => PrReviewStatusArgs
  -> Eff es (GotoChoice '[To Exit PrReviewStatusResult])
prReviewStatusLogic args = do
  let repo = Repo "tidepool-heavy-industries/tidepool"

  -- Fetch inline review comments (code-specific feedback)
  inlineComments <- getPullRequestReviews repo args.prsaPrNumber

  -- Fetch PR-level reviews (general feedback on the PR)
  maybePr <- getPullRequest repo args.prsaPrNumber True
  let prLevelReviews = case maybePr of
        Nothing -> []
        Just (PullRequest _ _ _ _ _ _ _ _ _ _ _ _ reviews) -> map reviewToComment reviews

  -- Combine all feedback
  let allComments = inlineComments ++ prLevelReviews

  -- Partition by author type and resolution status
  let (copilotFeedback, humanFeedback) = partitionComments allComments
      summary = buildSummary copilotFeedback humanFeedback

  pure $ gotoExit PrReviewStatusResult
    { prsrPrNumber = args.prsaPrNumber
    , prsrCopilot = copilotFeedback
    , prsrHumans = humanFeedback
    , prsrSummary = summary
    }
