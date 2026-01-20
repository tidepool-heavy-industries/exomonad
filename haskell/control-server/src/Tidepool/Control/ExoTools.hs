{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | External Orchestration tools (Tier 1) as Graph DSL nodes.
--
-- Includes tools for interacting with beads (BD), git, and GitHub.
module Tidepool.Control.ExoTools
  ( -- * Exo Status
    ExoStatusGraph(..)
  , exoStatusHandlers
  , exoStatusLogic
  , ExoStatusArgs(..)
  , ExoStatusResult(..)

    -- * Exo Complete
  , ExoCompleteGraph(..)
  , exoCompleteHandlers
  , exoCompleteLogic
  , ExoCompleteArgs(..)
  , ExoCompleteResult(..)

    -- * Exo Reconstitute
  , ExoReconstituteGraph(..)
  , exoReconstituteHandlers
  , exoReconstituteLogic
  , ExoReconstituteArgs(..)
  , ExoReconstituteResult

    -- * Pre Commit Check
  , PreCommitCheckGraph(..)
  , preCommitCheckHandlers
  , preCommitCheckLogic
  , PreCommitCheckArgs(..)
  , PreCommitCheckResult(..)

    -- * Spawn Agents
  , SpawnAgentsGraph(..)
  , spawnAgentsHandlers
  , spawnAgentsLogic
  , SpawnAgentsArgs(..)
  , SpawnAgentsResult(..)

    -- * File PR
  , FilePRGraph(..)
  , filePRHandlers
  , filePRLogic
  , FilePRArgs(..)
  , FilePRResult(..)

    -- * Pr Review Status
  , PrReviewStatusGraph(..)
  , prReviewStatusHandlers
  , prReviewStatusLogic
  , PrReviewStatusArgs(..)
  , PrReviewStatusResult(..)

    -- * Helpers
  , parseBeadId
  , slugify
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM)
import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), object, withObject)
import Data.Char (isAlphaNum, isSpace)
import Data.Either (partitionEithers)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.FilePath ((</>))

import Tidepool.Effects.BD (BD, BeadInfo(..), BeadStatus(..), DependencyInfo(..), getBead, closeBead, sync)
import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo, getDirtyFiles)
import Tidepool.Effects.GitHub (GitHub, PullRequest(..), listPullRequests, PRFilter(..), Repo(..), PRCreateSpec(..), PRUrl(..), createPR, ReviewComment(..), getPullRequestReviews)
import Tidepool.Effects.Justfile (Justfile, runRecipe, JustResult(..))
import Tidepool.Effects.Worktree (Worktree, WorktreeSpec(..), WorktreePath(..), createWorktree)
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, arraySchema, emptySchema, SchemaType(..), describeField)

-- ════════════════════════════════════════════════════════════════════════════
-- EXO-STATUS GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for exo_status tool.
data ExoStatusArgs = ExoStatusArgs
  { esaBeadId :: Maybe Text  -- ^ Optional bead ID. If not provided, inferred from branch.
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema ExoStatusArgs where
  jsonSchema = objectSchema
    [ ("bead_id", describeField "bead_id" "Optional bead ID (e.g. tidepool-huj). If omitted, inferred from branch name." (emptySchema TString))
    ]
    []

instance FromJSON ExoStatusArgs where
  parseJSON = withObject "ExoStatusArgs" $ \v ->
    ExoStatusArgs <$> v .:? "bead_id"

instance ToJSON ExoStatusArgs where
  toJSON args = object ["bead_id" .= esaBeadId args]

-- | Result of exo_status tool.
data ExoStatusResult = ExoStatusResult
  { esrBead :: Maybe BeadInfo
  , esrWorktree :: Maybe WorktreeInfo
  , esrDirtyFiles :: [FilePath]
  , esrPR :: Maybe PullRequest
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ExoStatusResult where
  toJSON res = object
    [ "bead" .= esrBead res
    , "worktree" .= esrWorktree res
    , "dirty_files" .= esrDirtyFiles res
    , "pr" .= esrPR res
    ]

-- | Graph definition for exo_status tool.
data ExoStatusGraph mode = ExoStatusGraph
  { esEntry :: mode :- EntryNode ExoStatusArgs
      :@ MCPExport
      :@ MCPToolDef '("exo_status", "Get current development context: bead details, git status, and PR info.")

  , esRun :: mode :- LogicNode
      :@ Input ExoStatusArgs
      :@ UsesEffects '[BD, Git, GitHub, Goto Exit ExoStatusResult]

  , esExit :: mode :- ExitNode ExoStatusResult
  }
  deriving Generic

-- | Handlers for exo_status graph.
exoStatusHandlers
  :: (Member BD es, Member Git es, Member GitHub es)
  => ExoStatusGraph (AsHandler es)
exoStatusHandlers = ExoStatusGraph
  { esEntry = ()
  , esRun = exoStatusLogic
  , esExit = ()
  }

-- | Core logic for exo_status.
exoStatusLogic
  :: (Member BD es, Member Git es, Member GitHub es)
  => ExoStatusArgs
  -> Eff es (GotoChoice '[To Exit ExoStatusResult])
exoStatusLogic args = do
  result <- getDevelopmentContext args.esaBeadId
  pure $ gotoExit result

-- ════════════════════════════════════════════════════════════════════════════
-- EXO-COMPLETE GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for exo_complete tool.
data ExoCompleteArgs = ExoCompleteArgs
  { ecaBeadId :: Maybe Text  -- ^ Optional bead ID. If not provided, inferred from branch.
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema ExoCompleteArgs where
  jsonSchema = objectSchema
    [ ("bead_id", describeField "bead_id" "Optional bead ID (e.g. tidepool-huj). If omitted, inferred from branch name." (emptySchema TString))
    ]
    []

instance FromJSON ExoCompleteArgs where
  parseJSON = withObject "ExoCompleteArgs" $ \v ->
    ExoCompleteArgs <$> v .:? "bead_id"

instance ToJSON ExoCompleteArgs where
  toJSON args = object
    [ "bead_id" .= ecaBeadId args
    ]

-- | Result of exo_complete tool.
data ExoCompleteResult = ExoCompleteResult
  { ecrBeadId :: Text
  , ecrStatus :: BeadStatus
  , ecrMessage :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ExoCompleteResult where
  toJSON res = object
    [ "bead_id" .= ecrBeadId res
    , "status" .= ecrStatus res
    , "message" .= ecrMessage res
    ]

-- | Graph definition for exo_complete tool.
data ExoCompleteGraph mode = ExoCompleteGraph
  { ecEntry :: mode :- EntryNode ExoCompleteArgs
      :@ MCPExport
      :@ MCPToolDef '("exo_complete", "Complete work on a bead: close the bead and verify git state.")

  , ecRun :: mode :- LogicNode
      :@ Input ExoCompleteArgs
      :@ UsesEffects '[BD, Git, Goto Exit ExoCompleteResult]

  , ecExit :: mode :- ExitNode ExoCompleteResult
  }
  deriving Generic

-- | Handlers for exo_complete graph.
exoCompleteHandlers
  :: (Member BD es, Member Git es)
  => ExoCompleteGraph (AsHandler es)
exoCompleteHandlers = ExoCompleteGraph
  { ecEntry = ()
  , ecRun = exoCompleteLogic
  , ecExit = ()
  }

-- | Core logic for exo_complete.
exoCompleteLogic
  :: (Member BD es, Member Git es)
  => ExoCompleteArgs
  -> Eff es (GotoChoice '[To Exit ExoCompleteResult])
exoCompleteLogic args = do
  -- 1. Get Worktree/Git info
  mWt <- getWorktreeInfo
  dirtyFiles <- getDirtyFiles

  -- 2. Determine Bead ID
  let branchBeadId = case mWt of
        Just wt -> parseBeadId wt.wiBranch
        Nothing -> Nothing
      mTargetBeadId = ecaBeadId args <|> branchBeadId

  case mTargetBeadId of
    Nothing ->
      pure $ gotoExit ExoCompleteResult
        { ecrBeadId = ""
        , ecrStatus = StatusOpen
        , ecrMessage = "Could not determine bead ID. Please provide bead_id argument."
        }
    Just bid -> do
      -- 3. Get Bead Info to verify existence
      mBead <- getBead bid
      case mBead of
        Nothing ->
          pure $ gotoExit ExoCompleteResult
            { ecrBeadId = bid
            , ecrStatus = StatusOpen
            , ecrMessage = "Bead " <> bid <> " not found."
            }
        Just bead | bead.biStatus == StatusClosed ->
          pure $ gotoExit ExoCompleteResult
            { ecrBeadId = bid
            , ecrStatus = StatusClosed
            , ecrMessage = "Bead " <> bid <> " is already closed."
            }
        Just bead -> do
          -- 4. Check for dirty files
          if not (null dirtyFiles)
            then pure $ gotoExit ExoCompleteResult
              { ecrBeadId = bid
              , ecrStatus = bead.biStatus
              , ecrMessage = "Warning: You have uncommitted changes. Please commit before completing: " <> T.pack (show dirtyFiles)
              }
            else do
              -- 5. Close the bead
              closeBead bid
              pure $ gotoExit ExoCompleteResult
                { ecrBeadId = bid
                , ecrStatus = StatusClosed
                , ecrMessage = "Bead " <> bid <> " has been closed. Don't forget to push and create a PR if you haven't already!"
                }

-- ════════════════════════════════════════════════════════════════════════════
-- EXO-RECONSTITUTE GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for exo_reconstitute tool.
data ExoReconstituteArgs = ExoReconstituteArgs
  { eraBeadId :: Maybe Text  -- ^ Optional bead ID.
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema ExoReconstituteArgs where
  jsonSchema = objectSchema
    [ ("bead_id", describeField "bead_id" "Optional bead ID (e.g. tidepool-huj). If omitted, inferred from branch name." (emptySchema TString))
    ]
    []

instance FromJSON ExoReconstituteArgs where
  parseJSON = withObject "ExoReconstituteArgs" $ \v ->
    ExoReconstituteArgs <$> v .:? "bead_id"

instance ToJSON ExoReconstituteArgs where
  toJSON args = object ["bead_id" .= eraBeadId args]

-- | Result of exo_reconstitute tool.
--
-- Same as ExoStatusResult for now.
type ExoReconstituteResult = ExoStatusResult

-- | Graph definition for exo_reconstitute tool.
data ExoReconstituteGraph mode = ExoReconstituteGraph
  { erEntry :: mode :- EntryNode ExoReconstituteArgs
      :@ MCPExport
      :@ MCPToolDef '("exo_reconstitute", "Synchronize beads from main and refresh development context.")

  , erRun :: mode :- LogicNode
      :@ Input ExoReconstituteArgs
      :@ UsesEffects '[BD, Git, GitHub, Goto Exit ExoReconstituteResult]

  , erExit :: mode :- ExitNode ExoReconstituteResult
  }
  deriving Generic

-- | Handlers for exo_reconstitute graph.
exoReconstituteHandlers
  :: (Member BD es, Member Git es, Member GitHub es)
  => ExoReconstituteGraph (AsHandler es)
exoReconstituteHandlers = ExoReconstituteGraph
  { erEntry = ()
  , erRun = exoReconstituteLogic
  , erExit = ()
  }

-- | Core logic for exo_reconstitute.
exoReconstituteLogic
  :: (Member BD es, Member Git es, Member GitHub es)
  => ExoReconstituteArgs
  -> Eff es (GotoChoice '[To Exit ExoReconstituteResult])
exoReconstituteLogic args = do
  -- 1. Run BD Sync (Primary difference from exo_status)
  sync

  -- 2. Determine context
  result <- getDevelopmentContext args.eraBeadId
  pure $ gotoExit result

-- ════════════════════════════════════════════════════════════════════════════
-- PRE-COMMIT-CHECK GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for pre_commit_check tool.
data PreCommitCheckArgs = PreCommitCheckArgs
  { pccaRecipe :: Maybe Text  -- ^ Optional just recipe name.
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema PreCommitCheckArgs where
  jsonSchema = objectSchema
    [ ("recipe", describeField "recipe" "Optional just recipe to run. Defaults to 'pre-commit-fast'." (emptySchema TString))
    ]
    []

instance FromJSON PreCommitCheckArgs where
  parseJSON = withObject "PreCommitCheckArgs" $ \v ->
    PreCommitCheckArgs <$> v .:? "recipe"

instance ToJSON PreCommitCheckArgs where
  toJSON args = object ["recipe" .= pccaRecipe args]

-- | Result of pre_commit_check tool.
data PreCommitCheckResult = PreCommitCheckResult
  { pccrSuccess :: Bool
  , pccrStdout  :: Text
  , pccrStderr  :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PreCommitCheckResult where
  toJSON res = object
    [ "success" .= pccrSuccess res
    , "stdout"  .= pccrStdout res
    , "stderr"  .= pccrStderr res
    ]

instance FromJSON PreCommitCheckResult where
  parseJSON = withObject "PreCommitCheckResult" $ \v ->
    PreCommitCheckResult
      <$> v .: "success"
      <*> v .: "stdout"
      <*> v .: "stderr"

-- | Graph definition for pre_commit_check tool.
data PreCommitCheckGraph mode = PreCommitCheckGraph
  { pccEntry :: mode :- EntryNode PreCommitCheckArgs
      :@ MCPExport
      :@ MCPToolDef '("pre_commit_check", "Run pre-commit quality checks using 'just'.")

  , pccRun :: mode :- LogicNode
      :@ Input PreCommitCheckArgs
      :@ UsesEffects '[Justfile, Goto Exit PreCommitCheckResult]

  , pccExit :: mode :- ExitNode PreCommitCheckResult
  }
  deriving Generic

-- | Handlers for pre_commit_check graph.
preCommitCheckHandlers
  :: Member Justfile es
  => PreCommitCheckGraph (AsHandler es)
preCommitCheckHandlers = PreCommitCheckGraph
  { pccEntry = ()
  , pccRun = preCommitCheckLogic
  , pccExit = ()
  }

-- | Core logic for pre_commit_check.
preCommitCheckLogic
  :: Member Justfile es
  => PreCommitCheckArgs
  -> Eff es (GotoChoice '[To Exit PreCommitCheckResult])
preCommitCheckLogic args = do
  let recipe = fromMaybe "pre-commit-fast" args.pccaRecipe
  res <- runRecipe recipe []
  let success = res.exitCode == 0
  pure $ gotoExit PreCommitCheckResult
    { pccrSuccess = success
    , pccrStdout  = res.stdout
    , pccrStderr  = res.stderr
    }

-- ════════════════════════════════════════════════════════════════════════════
-- SPAWN-AGENTS GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for spawn_agents tool.
data SpawnAgentsArgs = SpawnAgentsArgs
  { saaBeadIds :: [Text]  -- ^ List of short form bead IDs (e.g. "wzi", "1b2").
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema SpawnAgentsArgs where
  jsonSchema = objectSchema
    [ ("bead_ids", describeField "bead_ids" "List of short-form bead IDs to spawn worktrees for." (arraySchema (emptySchema TString)))
    ]
    ["bead_ids"]

instance FromJSON SpawnAgentsArgs where
  parseJSON = withObject "SpawnAgentsArgs" $ \v ->
    SpawnAgentsArgs <$> v .: "bead_ids"

instance ToJSON SpawnAgentsArgs where
  toJSON args = object ["bead_ids" .= saaBeadIds args]

-- | Result of spawn_agents tool.
data SpawnAgentsResult = SpawnAgentsResult
  { sarWorktrees :: [(Text, FilePath)]
  , sarBootstrap :: [Text]
  , sarFailed    :: [(Text, Text)]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SpawnAgentsResult where
  toJSON res = object
    [ "worktrees" .= sarWorktrees res
    , "bootstrap" .= sarBootstrap res
    , "failed"     .= sarFailed res
    ]

-- | Graph definition for spawn_agents tool.
data SpawnAgentsGraph mode = SpawnAgentsGraph
  { saEntry :: mode :- EntryNode SpawnAgentsArgs
      :@ MCPExport
      :@ MCPToolDef '("spawn_agents", "Create worktrees and branches for parallel agent dispatch.")

  , saRun :: mode :- LogicNode
      :@ Input SpawnAgentsArgs
      :@ UsesEffects '[BD, Git, Worktree, Goto Exit SpawnAgentsResult]

  , saExit :: mode :- ExitNode SpawnAgentsResult
  }
  deriving Generic

-- | Handlers for spawn_agents graph.
spawnAgentsHandlers
  :: (Member BD es, Member Git es, Member Worktree es)
  => SpawnAgentsGraph (AsHandler es)
spawnAgentsHandlers = SpawnAgentsGraph
  { saEntry = ()
  , saRun = spawnAgentsLogic
  , saExit = ()
  }

-- | Core logic for spawn_agents.
spawnAgentsLogic
  :: (Member BD es, Member Git es, Member Worktree es)
  => SpawnAgentsArgs
  -> Eff es (GotoChoice '[To Exit SpawnAgentsResult])
spawnAgentsLogic args = do
  mWtInfo <- getWorktreeInfo
  let repoRoot = maybe "." (\wi -> wi.wiRepoRoot) mWtInfo

  results <- forM args.saaBeadIds $ \shortId -> do
    -- Normalize bead ID (ensure it starts with tidepool-)
    let fullId = if "tidepool-" `T.isPrefixOf` shortId
                 then shortId
                 else "tidepool-" <> shortId
    
    mBead <- getBead fullId
    case mBead of
      Nothing -> pure $ Left (shortId, "Bead not found: " <> fullId)
      Just bead -> do
        if bead.biStatus == StatusBlocked
          then pure $ Left (shortId, "Bead is blocked: " <> fullId)
          else do
            let slug = slugify bead.biTitle
                branchName = "bd-" <> shortId <> "/" <> slug
                -- Resolve absolute path based on repo root
                targetPath = repoRoot </> ".worktrees" </> "tidepool" </> "bd-" <> T.unpack shortId <> "-" <> T.unpack slug
                spec = WorktreeSpec
                  { wsBaseName = "bd-" <> shortId
                  , wsFromBranch = Just "origin/main"
                  , wsBranchName = Just branchName
                  , wsPath = Just targetPath
                  }
            
            res <- createWorktree spec
            case res of
              Left err -> pure $ Left (shortId, T.pack (show err))
              Right (WorktreePath path) -> pure $ Right (shortId, path)

  let (failed, succeeded) = partitionEithers results
      worktrees = succeeded
      bootstrap = map (\(_, path) -> "cd " <> T.pack path <> " && ./scripts/bead-context") succeeded

  pure $ gotoExit $ SpawnAgentsResult
    { sarWorktrees = worktrees
    , sarBootstrap = bootstrap
    , sarFailed    = failed
    }

-- ════════════════════════════════════════════════════════════════════════════
-- PR-REVIEW-STATUS GRAPH
-- ════════════════════════════════════════════════════════════════════════════

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

-- | Result of pr_review_status tool.
-- Returns comments grouped by author.
data PrReviewStatusResult = PrReviewStatusResult
  { prsrComments :: Map Text [ReviewComment]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PrReviewStatusResult where
  toJSON res = toJSON (prsrComments res)

instance FromJSON PrReviewStatusResult where
  parseJSON v = PrReviewStatusResult <$> parseJSON v

-- | Graph definition for pr_review_status tool.
data PrReviewStatusGraph mode = PrReviewStatusGraph
  { prsEntry :: mode :- EntryNode PrReviewStatusArgs
      :@ MCPExport
      :@ MCPToolDef '("pr_review_status", "Get PR review comments grouped by author (especially Copilot).")

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

-- | Core logic for pr_review_status.
prReviewStatusLogic
  :: (Member GitHub es)
  => PrReviewStatusArgs
  -> Eff es (GotoChoice '[To Exit PrReviewStatusResult])
prReviewStatusLogic args = do
  let repo = Repo "tidepool-heavy-industries/tidepool"
  comments <- getPullRequestReviews repo args.prsaPrNumber
  
  -- Group by author
  let grouped = foldr (\c acc -> Map.insertWith (++) c.rcAuthor [c] acc) Map.empty comments
  
  pure $ gotoExit PrReviewStatusResult
    { prsrComments = grouped
    }


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Helper to gather common development context.
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
      prs <- listPullRequests repo (PRFilter Nothing (Just "main") (Just 100))
      pure $ find (\pr -> pr.prHeadRefName == wt.wiBranch) prs
    Nothing -> pure Nothing

  pure ExoStatusResult
    { esrBead = mBead
    , esrWorktree = mWt
    , esrDirtyFiles = dirtyFiles
    , esrPR = mPR
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

-- ════════════════════════════════════════════════════════════════════════════
-- FILE-PR GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for file_pr tool.
data FilePRArgs = FilePRArgs
  { fpaBeadId :: Maybe Text  -- ^ Optional bead ID. If not provided, inferred from branch.
  , fpaTitle  :: Maybe Text  -- ^ Optional PR title. If not provided, derived from bead.
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema FilePRArgs where
  jsonSchema = objectSchema
    [ ("bead_id", describeField "bead_id" "Optional bead ID (e.g. tidepool-huj). If omitted, inferred from branch name." (emptySchema TString))
    , ("title", describeField "title" "Optional PR title. If omitted, derived from bead title." (emptySchema TString))
    ]
    []

instance FromJSON FilePRArgs where
  parseJSON = withObject "FilePRArgs" $ \v ->
    FilePRArgs <$> v .:? "bead_id" <*> v .:? "title"

instance ToJSON FilePRArgs where
  toJSON args = object
    [ "bead_id" .= fpaBeadId args
    , "title" .= fpaTitle args
    ]

-- | Result of file_pr tool.
data FilePRResult = FilePRResult
  { fprUrl :: Maybe Text
  , fprError :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON FilePRResult where
  toJSON res = object
    [ "url" .= fprUrl res
    , "error" .= fprError res
    ]

instance FromJSON FilePRResult where
  parseJSON = withObject "FilePRResult" $ \v ->
    FilePRResult
      <$> v .:? "url"
      <*> v .:? "error"

-- | Graph definition for file_pr tool.
data FilePRGraph mode = FilePRGraph
  { fpEntry :: mode :- EntryNode FilePRArgs
      :@ MCPExport
      :@ MCPToolDef '("file_pr", "File a pull request with full bead context in the body.")

  , fpRun :: mode :- LogicNode
      :@ Input FilePRArgs
      :@ UsesEffects '[BD, Git, GitHub, Goto Exit FilePRResult]

  , fpExit :: mode :- ExitNode FilePRResult
  }
  deriving Generic

-- | Handlers for file_pr graph.
filePRHandlers
  :: (Member BD es, Member Git es, Member GitHub es)
  => FilePRGraph (AsHandler es)
filePRHandlers = FilePRGraph
  { fpEntry = ()
  , fpRun = filePRLogic
  , fpExit = ()
  }

-- | Core logic for file_pr.
filePRLogic
  :: (Member BD es, Member Git es, Member GitHub es)
  => FilePRArgs
  -> Eff es (GotoChoice '[To Exit FilePRResult])
filePRLogic args = do
  -- 1. Get Worktree/Git info
  mWt <- getWorktreeInfo

  -- 2. Determine Bead ID
  let branchBeadId = case mWt of
        Just wt -> parseBeadId wt.wiBranch
        Nothing -> Nothing
      mTargetBeadId = args.fpaBeadId <|> branchBeadId

  case mTargetBeadId of
    Nothing ->
      pure $ gotoExit $ FilePRResult Nothing (Just "Could not determine bead ID. Please provide bead_id argument.")
    Just bid -> do
      -- 3. Get Bead Info
      mBead <- getBead bid
      case mBead of
        Nothing ->
          pure $ gotoExit $ FilePRResult Nothing (Just $ "Bead " <> bid <> " not found.")
        Just bead -> do
          -- 4. Prepare PR Spec
          -- Derive branch name if worktree info unavailable
          let derivedBranch = "bd-" <> bid <> "/" <> slugify bead.biTitle
              headBranch = case mWt of
                Just wt -> wt.wiBranch
                Nothing -> derivedBranch

          let title = fromMaybe ("[" <> bid <> "] " <> bead.biTitle) args.fpaTitle
              body = formatPRBody bead
              repo = Repo "tidepool-heavy-industries/tidepool"
              spec = PRCreateSpec
                { prcsRepo = repo
                , prcsHead = headBranch
                , prcsBase = "main"
                , prcsTitle = title
                , prcsBody = body
                }

          -- 5. Create PR
          PRUrl url <- createPR spec
          pure $ gotoExit $ FilePRResult (Just url) Nothing

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
