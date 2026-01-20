{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tidepool.Control.ExoTools.Internal
  ( ExoStatusResult(..)
  , getDevelopmentContext
  , parseBeadId
  , slugify
  , formatPRBody
  , extractBeadId
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Freer (Eff, Member)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Char (isAlphaNum, isSpace)
import Data.List (find, sortOn, take)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Effects.BD
  ( BD
  , BeadInfo(..)
  , BeadStatus(..)
  , DependencyInfo(..)
  , getBead
  , listBeads
  , defaultListBeadsInput
  )
import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo, getDirtyFiles)
import Tidepool.Effects.GitHub (GitHub, PullRequest(..), listPullRequests, Repo(..), PRFilter(..), defaultPRFilter)

-- | Brief bead info for summaries.
data BeadBrief = BeadBrief
  { bbId :: Text
  , bbTitle :: Text
  , bbPriority :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON BeadBrief where
  toJSON b = object
    [ "id" .= bbId b
    , "title" .= bbTitle b
    , "priority" .= bbPriority b
    ]

-- | Sprint summary info.
data SprintSummary = SprintSummary
  { ssTotalOpen :: Int
  , ssReady :: [BeadBrief]
  , ssBlocked :: Int
  , ssInProgress :: Int
  , ssRecentlyClosed :: [BeadBrief]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SprintSummary where
  toJSON s = object
    [ "total_open" .= ssTotalOpen s
    , "ready" .= ssReady s
    , "blocked" .= ssBlocked s
    , "in_progress" .= ssInProgress s
    , "recently_closed" .= ssRecentlyClosed s
    ]

-- | Result of exo_status tool.
data ExoStatusResult = ExoStatusResult
  { esrBead :: Maybe BeadInfo
  , esrWorktree :: Maybe WorktreeInfo
  , esrDirtyFiles :: [FilePath]
  , esrPR :: Maybe PullRequest
  , esrSprintSummary :: Maybe SprintSummary
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ExoStatusResult where
  toJSON res = object
    [ "bead" .= esrBead res
    , "worktree" .= esrWorktree res
    , "dirty_files" .= esrDirtyFiles res
    , "pr" .= esrPR res
    , "sprint_summary" .= esrSprintSummary res
    ]

getDevelopmentContext
  :: (Member BD es, Member Git es, Member GitHub es)
  => Maybe Text
  -> Eff es ExoStatusResult
getDevelopmentContext maybeBeadId = do
  -- 1. Get Worktree/Git info
  mWt <- getWorktreeInfo
  dirtyFiles <- getDirtyFiles

  -- 2. Determine Bead ID
  let branchBeadId = case mWt of
        Just wt -> parseBeadId wt.wiBranch
        Nothing -> Nothing
      targetBeadId = maybeBeadId <|> branchBeadId

  -- 3. Get Bead Info
  mBead <- case targetBeadId of
    Just bid -> getBead bid
    Nothing -> pure Nothing

  -- 4. Get PR Info
  mPR <- case mWt of
    Just wt -> do
      let repo = Repo "tidepool-heavy-industries/tidepool"
          filt = defaultPRFilter { pfBase = Just "main", pfLimit = Just 100 }
      prs <- listPullRequests repo filt
      pure $ find (\pr -> pr.prHeadRefName == wt.wiBranch) prs
    Nothing -> pure Nothing

  -- 5. Get Sprint Summary if no bead context
  mSprintSummary <- case mBead of
    Just _ -> pure Nothing
    Nothing -> do
      allBeads <- listBeads defaultListBeadsInput
      let openStatuses = [StatusOpen, StatusInProgress, StatusHooked, StatusBlocked]
          isOpen b = b.biStatus `elem` openStatuses
          isReady b = b.biStatus == StatusOpen && all (\d -> d.diStatus == StatusClosed) b.biDependencies
          isClosed b = b.biStatus == StatusClosed
          
          toBeadBrief b = BeadBrief
            { bbId = b.biId
            , bbTitle = b.biTitle
            , bbPriority = b.biPriority
            }

          readyBeads = sortOn (.biPriority) $ filter isReady allBeads
          recentlyClosedBeads = sortOn (Down . (.biClosedAt)) $ filter isClosed allBeads

      pure $ Just SprintSummary
        { ssTotalOpen = length $ filter isOpen allBeads
        , ssReady = map toBeadBrief $ take 5 readyBeads
        , ssBlocked = length $ filter (\b -> b.biStatus == StatusBlocked) allBeads
        , ssInProgress = length $ filter (\b -> b.biStatus == StatusInProgress) allBeads
        , ssRecentlyClosed = map toBeadBrief $ take 3 recentlyClosedBeads
        }

  pure ExoStatusResult
    { esrBead = mBead
    , esrWorktree = mWt
    , esrDirtyFiles = dirtyFiles
    , esrPR = mPR
    , esrSprintSummary = mSprintSummary
    }

-- | Parse bead ID from branch name (bd-{id}/* convention)
parseBeadId :: Text -> Maybe Text
parseBeadId branch =
  if "bd-" `T.isPrefixOf` branch
  then
    let content = T.drop 3 branch
        (beadId, rest) = T.break (== '/') content
    in if T.null beadId || T.null (T.drop 1 rest) -- Must have / and something after
       then Nothing
       else Just $ "tidepool-" <> beadId
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

-- | Format PR body from bead info.
formatPRBody :: BeadInfo -> Text
formatPRBody bead = T.unlines $
  [ "Closes " <> bead.biId
  , ""
  , "## Description"
  , fromMaybe "(No description)" bead.biDescription
  , ""
  ] ++
  (if null bead.biDependencies
   then []
   else ["## Dependencies", ""] ++ map formatDep bead.biDependencies ++ [""]) ++
  (if null bead.biDependents
   then []
   else ["## Dependents", ""] ++ map formatDep bead.biDependents ++ [""])
  where
    formatDep dep = "  → " <> dep.diId <> ": " <> dep.diTitle

-- | Extract bead ID from PR title.
-- Pattern: [tidepool-XXX]
extractBeadId :: Text -> Maybe Text
extractBeadId title =
  let (_, rest) = T.breakOn "[tidepool-" title
  in if T.null rest
     then Nothing
     else
       let (idPart, _) = T.break (== ']') (T.drop 1 rest)
       in Just idPart
