{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.ExoTools.SpawnAgents
  ( SpawnAgentsGraph(..)
  , spawnAgentsHandlers
  , spawnAgentsLogic
  , SpawnAgentsArgs(..)
  , SpawnAgentsResult(..)
  ) where

import Control.Monad (forM)
import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.FilePath ((</>))

import Tidepool.Effects.BD (BD, BeadInfo(..), BeadStatus(..), DependencyInfo(..), getBead)
import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo)
import Tidepool.Effects.Worktree (Worktree, WorktreeSpec(..), WorktreePath(..), createWorktree)
import Tidepool.Effects.FileSystem (FileSystem, createDirectory, writeFileText, copyFile, createSymlink)
import Tidepool.Effects.Zellij (Zellij, TabConfig(..), TabId(..), checkZellijEnv, newTab)
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, arraySchema, emptySchema, SchemaType(..), describeField)

import Tidepool.Control.ExoTools.Internal (slugify)

-- | Build markdown context for a bead.
buildBeadContext :: BeadInfo -> Text -> Text
buildBeadContext bead branchName = T.unlines
  [ "# Task: " <> bead.biTitle
  , ""
  , "**ID:** " <> bead.biId
  , "**Status:** " <> statusToText bead.biStatus
  , "**Priority:** " <> T.pack (show bead.biPriority)
  , "**Branch:** " <> branchName
  , ""
  , "## Description"
  , ""
  , fromMaybe "(no description)" bead.biDescription
  , ""
  , "## Dependencies"
  , ""
  , if null bead.biDependencies
      then "None"
      else T.unlines $ map formatDep bead.biDependencies
  , ""
  , "## Workflow"
  , ""
  , "1. Implement changes"
  , "2. Commit: [" <> bead.biId <> "] <description>"
  , "3. Push: git push -u origin " <> branchName
  , "4. File PR: gh pr create --title \"[" <> bead.biId <> "] " <> bead.biTitle <> "\""
  ]
  where
    statusToText = \case
      StatusOpen -> "open"
      StatusInProgress -> "in_progress"
      StatusClosed -> "closed"
      StatusHooked -> "hooked"
      StatusBlocked -> "blocked"

    formatDep (DependencyInfo { diId = depId, diTitle = depTitle, diStatus = depStatus }) =
      "- " <> depId <> ": " <> depTitle <> " (" <> statusToText depStatus <> ")"

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
  { sarWorktrees :: [(Text, FilePath)]  -- ^ Successfully created worktrees: (shortId, path)
  , sarTabs      :: [(Text, Text)]      -- ^ Successfully launched tabs: (shortId, tabId)
  , sarFailed    :: [(Text, Text)]      -- ^ Failed operations: (shortId, reason)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SpawnAgentsResult where
  toJSON res = object
    [ "worktrees" .= sarWorktrees res
    , "tabs"      .= sarTabs res
    , "failed"    .= sarFailed res
    ]

-- | Graph definition for spawn_agents tool.
data SpawnAgentsGraph mode = SpawnAgentsGraph
  { saEntry :: mode :- EntryNode SpawnAgentsArgs
      :@ MCPExport
      :@ MCPToolDef '("spawn_agents", "Create worktrees and branches for parallel agent dispatch.")

  , saRun :: mode :- LogicNode
      :@ Input SpawnAgentsArgs
      :@ UsesEffects '[BD, Git, Worktree, FileSystem, Zellij, Goto Exit SpawnAgentsResult]

  , saExit :: mode :- ExitNode SpawnAgentsResult
  }
  deriving Generic

-- | Handlers for spawn_agents graph.
spawnAgentsHandlers
  :: (Member BD es, Member Git es, Member Worktree es, Member FileSystem es, Member Zellij es)
  => SpawnAgentsGraph (AsHandler es)
spawnAgentsHandlers = SpawnAgentsGraph
  { saEntry = ()
  , saRun = spawnAgentsLogic
  , saExit = ()
  }

-- | Core logic for spawn_agents.
spawnAgentsLogic
  :: (Member BD es, Member Git es, Member Worktree es, Member FileSystem es, Member Zellij es)
  => SpawnAgentsArgs
  -> Eff es (GotoChoice '[To Exit SpawnAgentsResult])
spawnAgentsLogic args = do
  -- 1. Check Zellij environment (fail if not in Zellij)
  mZellijSession <- checkZellijEnv
  case mZellijSession of
    Nothing -> pure $ gotoExit $ SpawnAgentsResult
      { sarWorktrees = []
      , sarTabs = []
      , sarFailed = [("*", "Not running in Zellij session")]
      }
    Just _ -> do
      -- Get repo root for path resolution
      mWtInfo <- getWorktreeInfo
      let repoRoot = maybe "." (\wi -> wi.wiRepoRoot) mWtInfo

      -- 2. Process each bead
      results <- forM args.saaBeadIds $ \shortId -> do
        processBead repoRoot shortId

      -- Partition results
      let (failed, succeeded) = partitionEithers results
          worktrees = [(sid, path) | (sid, path, _) <- succeeded]
          tabs = [(sid, T.unpack tabId) | (sid, _, TabId tabId) <- succeeded]

      pure $ gotoExit $ SpawnAgentsResult
        { sarWorktrees = worktrees
        , sarTabs = [(sid, T.pack t) | (sid, t) <- tabs]
        , sarFailed = failed
        }


-- | Process a single bead: create worktree, bootstrap .tidepool/, write context, launch tab.
processBead
  :: (Member BD es, Member Worktree es, Member FileSystem es, Member Zellij es)
  => FilePath                                          -- ^ Repo root
  -> Text                                              -- ^ Short bead ID
  -> Eff es (Either (Text, Text) (Text, FilePath, TabId))  -- ^ Left (id, error) or Right (id, path, tabId)
processBead repoRoot shortId = do
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
              targetPath = repoRoot </> ".worktrees" </> "tidepool" </> "bd-" <> T.unpack shortId <> "-" <> T.unpack slug
              spec = WorktreeSpec
                { wsBaseName = "bd-" <> shortId
                , wsFromBranch = Just "origin/main"
                , wsBranchName = Just branchName
                , wsPath = Just targetPath
                }

          -- a. Create worktree
          wtRes <- createWorktree spec
          case wtRes of
            Left err -> pure $ Left (shortId, T.pack (show err))
            Right (WorktreePath path) -> do
              -- b. Bootstrap .tidepool/ directory structure
              bootstrapRes <- bootstrapTidepool repoRoot path
              case bootstrapRes of
                Left errMsg -> pure $ Left (shortId, "Worktree created but bootstrap failed: " <> errMsg)
                Right () -> do
                  -- c. Write bead context
                  let context = buildBeadContext bead branchName
                  contextRes <- writeBeadContext path context
                  case contextRes of
                    Left errMsg -> pure $ Left (shortId, "Worktree created but context write failed: " <> errMsg)
                    Right () -> do
                      -- d. Launch Zellij tab
                      let tabConfig = TabConfig
                            { tcName = shortId
                            , tcLayout = ".zellij/worktree.kdl"
                            , tcCwd = path
                            , tcEnv = [("SUBAGENT_CMD", "claude")]
                            }
                      tabRes <- newTab tabConfig
                      case tabRes of
                        Left err -> pure $ Left (shortId, "Worktree created but tab launch failed: " <> T.pack (show err))
                        Right tabId -> pure $ Right (shortId, path, tabId)


-- | Bootstrap .tidepool/ directory structure in a worktree.
bootstrapTidepool
  :: (Member FileSystem es)
  => FilePath  -- ^ Repo root (for symlink source)
  -> FilePath  -- ^ Worktree path
  -> Eff es (Either Text ())
bootstrapTidepool repoRoot worktreePath = do
  -- Create .tidepool directories
  let tidepoolDir = worktreePath </> ".tidepool"
      socketsDir = tidepoolDir </> "sockets"
      logsDir = tidepoolDir </> "logs"

  dirRes1 <- createDirectory socketsDir
  case dirRes1 of
    Left err -> pure $ Left $ T.pack (show err)
    Right () -> do
      dirRes2 <- createDirectory logsDir
      case dirRes2 of
        Left err -> pure $ Left $ T.pack (show err)
        Right () -> do
          -- Copy subagent process-compose template
          let srcTemplate = repoRoot </> ".tidepool" </> "templates" </> "subagent-pc.yaml"
              dstConfig = worktreePath </> "process-compose.yaml"
          copyRes <- copyFile srcTemplate dstConfig
          case copyRes of
            Left err -> pure $ Left $ T.pack (show err)
            Right () -> do
              -- Symlink .env from repo root
              let srcEnv = repoRoot </> ".env"
                  dstEnv = worktreePath </> ".env"
              symlinkRes <- createSymlink srcEnv dstEnv
              case symlinkRes of
                Left err -> pure $ Left $ T.pack (show err)
                Right () -> pure $ Right ()


-- | Write bead context to .claude/context/bead.md.
writeBeadContext
  :: (Member FileSystem es)
  => FilePath  -- ^ Worktree path
  -> Text      -- ^ Context content
  -> Eff es (Either Text ())
writeBeadContext worktreePath context = do
  let contextDir = worktreePath </> ".claude" </> "context"
      contextFile = contextDir </> "bead.md"

  dirRes <- createDirectory contextDir
  case dirRes of
    Left err -> pure $ Left $ T.pack (show err)
    Right () -> do
      writeRes <- writeFileText contextFile context
      case writeRes of
        Left err -> pure $ Left $ T.pack (show err)
        Right () -> pure $ Right ()
