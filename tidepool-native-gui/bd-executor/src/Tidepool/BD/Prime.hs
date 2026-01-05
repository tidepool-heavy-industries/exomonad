-- | Urchin Prime - Context generation for Claude Code sessions.
--
-- Aggregates worktree info, git status, and bead state into
-- focused context for agent sessions.
module Tidepool.BD.Prime
  ( -- * Types
    PrimeContext(..)

    -- * Context Gathering
  , gatherContext
  , gatherContextFromCwd

    -- * Rendering
  , renderPrime
  , renderPrimeJSON
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (ToJSON(..), object, (.=), encode, decode)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortOn)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Tidepool.Effects.BD (BeadInfo(..), BeadStatus(..), BeadType(..), DependencyInfo(..))
import Tidepool.BD.Prime.Worktree (WorktreeInfo(..), detectWorktree)
import Tidepool.BD.Executor (BDConfig(..), defaultBDConfig, bdShow)


-- | Aggregated context for urchin prime output.
data PrimeContext = PrimeContext
  { pcWorktree      :: WorktreeInfo
    -- ^ Current worktree/repo info
  , pcDirtyFiles    :: [FilePath]
    -- ^ Uncommitted changes
  , pcRecentCommits :: [Text]
    -- ^ Last 5 commit subjects
  , pcInProgress    :: [BeadInfo]
    -- ^ Tasks with status=in_progress
  , pcReady         :: [BeadInfo]
    -- ^ Open, unblocked tasks
  , pcHooked        :: [BeadInfo]
    -- ^ Hooked tasks (propulsion priority)
  , pcEpicContext   :: Maybe BeadInfo
    -- ^ Epic matching worktree name (if any)
  }
  deriving (Show, Generic)

instance ToJSON PrimeContext where
  toJSON ctx = object
    [ "worktree" .= object
        [ "name"   .= ctx.pcWorktree.wiName
        , "path"   .= ctx.pcWorktree.wiPath
        , "branch" .= ctx.pcWorktree.wiBranch
        , "repo_root" .= ctx.pcWorktree.wiRepoRoot
        , "is_worktree" .= ctx.pcWorktree.wiIsWorktree
        ]
    , "git" .= object
        [ "dirty_count" .= length ctx.pcDirtyFiles
        , "dirty_files" .= ctx.pcDirtyFiles
        , "recent_commits" .= ctx.pcRecentCommits
        ]
    , "in_progress" .= ctx.pcInProgress
    , "ready" .= ctx.pcReady
    , "hooked" .= ctx.pcHooked
    , "epic" .= ctx.pcEpicContext
    ]


-- | Gather context from current working directory.
gatherContextFromCwd :: IO (Maybe PrimeContext)
gatherContextFromCwd = do
  mWorktree <- detectWorktree
  case mWorktree of
    Nothing -> pure Nothing
    Just wt -> Just <$> gatherContext defaultBDConfig wt


-- | Gather full prime context for a worktree.
gatherContext :: BDConfig -> WorktreeInfo -> IO PrimeContext
gatherContext config worktree = do
  -- Git info
  dirtyFiles <- getGitDirtyFiles
  recentCommits <- getGitRecentCommits 5

  -- BD queries (via CLI)
  inProgress <- bdListByStatus config "in_progress"
  hooked <- bdListByStatus config "hooked"
  allOpen <- bdListByStatus config "open"

  -- Filter open to only unblocked (no open dependencies)
  let ready = filter isUnblocked allOpen

  -- Find epic matching worktree name
  epics <- bdListByType config "epic"
  let epic = findMatchingEpic (wiName worktree) epics

  pure PrimeContext
    { pcWorktree = worktree
    , pcDirtyFiles = dirtyFiles
    , pcRecentCommits = recentCommits
    , pcInProgress = sortByPriority inProgress
    , pcReady = sortByPriority ready
    , pcHooked = sortByPriority hooked
    , pcEpicContext = epic
    }


-- | Render context as markdown.
renderPrime :: PrimeContext -> Text
renderPrime ctx = T.unlines $ concat
  [ header
  , [""]
  , gitSection
  , [""]
  , propulsionSection
  , [""]
  , inProgressSection
  , [""]
  , readySection
  , epicSection
  ]
  where
    wt = ctx.pcWorktree

    header =
      [ "# Urchin Prime: " <> wt.wiName <> " / " <> wt.wiBranch
      ]

    gitSection =
      [ "## Git Status"
      , "- **Branch**: " <> wt.wiBranch
      , "- **Dirty files**: " <> T.pack (show $ length ctx.pcDirtyFiles)
      ] ++
      case ctx.pcRecentCommits of
        [] -> []
        (recent:_) -> ["- **Recent**: " <> recent]

    propulsionSection =
      if null ctx.pcHooked
        then
          [ "## Propulsion"
          , ""
          , "No hooked work. Review **In Progress** or **Ready** below."
          ]
        else
          [ "## Propulsion"
          , ""
          , "**HOOKED WORK** - proceed immediately:"
          ] ++ map renderBead ctx.pcHooked

    inProgressSection =
      [ "## In Progress (" <> T.pack (show $ length ctx.pcInProgress) <> ")"
      ] ++
      if null ctx.pcInProgress
        then ["_None_"]
        else map renderBead ctx.pcInProgress

    readySection =
      [ "## Ready (" <> T.pack (show $ length ctx.pcReady) <> ")"
      ] ++
      if null ctx.pcReady
        then ["_None_"]
        else map renderBead ctx.pcReady

    epicSection =
      case ctx.pcEpicContext of
        Nothing -> []
        Just epic ->
          [ ""
          , "## Epic Context"
          , ""
          , "**" <> epic.biId <> "**: " <> epic.biTitle
          ]

    renderBead :: BeadInfo -> Text
    renderBead b =
      "- [" <> b.biId <> "] " <> b.biTitle <> " (P" <> T.pack (show b.biPriority) <> ")"


-- | Render context as JSON.
renderPrimeJSON :: PrimeContext -> Text
renderPrimeJSON = TE.decodeUtf8 . LBS.toStrict . encode


-- ════════════════════════════════════════════════════════════════════════════
-- GIT HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Get list of dirty (uncommitted) files.
getGitDirtyFiles :: IO [FilePath]
getGitDirtyFiles = do
  result <- try $ readProcessWithExitCode "git" ["status", "--porcelain"] ""
  case result of
    Left (_ :: SomeException) -> pure []
    Right (ExitSuccess, stdout, _) ->
      pure $ map (drop 3) $ lines stdout  -- Drop status prefix "XY "
    Right _ -> pure []


-- | Get recent commit subjects.
getGitRecentCommits :: Int -> IO [Text]
getGitRecentCommits n = do
  result <- try $ readProcessWithExitCode "git"
    ["log", "--oneline", "-" ++ show n, "--format=%s"] ""
  case result of
    Left (_ :: SomeException) -> pure []
    Right (ExitSuccess, stdout, _) ->
      pure $ map T.pack $ take n $ lines stdout
    Right _ -> pure []


-- ════════════════════════════════════════════════════════════════════════════
-- BD HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | List beads by status via bd CLI.
bdListByStatus :: BDConfig -> String -> IO [BeadInfo]
bdListByStatus config status = bdListWithArgs config ["--status=" ++ status]


-- | List beads by type via bd CLI.
bdListByType :: BDConfig -> String -> IO [BeadInfo]
bdListByType config btype = bdListWithArgs config ["--type=" ++ btype]


-- | Run bd list with additional arguments.
bdListWithArgs :: BDConfig -> [String] -> IO [BeadInfo]
bdListWithArgs config extraArgs = do
  let args = ["list", "--json"] ++ extraArgs
            ++ maybe [] (\d -> ["--db", d]) config.bcBeadsDir

  result <- try $ readProcessWithExitCode "bd" args ""
  case result of
    Left (_ :: SomeException) -> pure []
    Right (ExitSuccess, stdout, _) ->
      case decodeBeadList (LBS.fromStrict $ TE.encodeUtf8 $ T.pack stdout) of
        Just beads -> pure beads
        Nothing -> pure []
    Right _ -> pure []


-- | Decode JSON list of beads.
decodeBeadList :: LBS.ByteString -> Maybe [BeadInfo]
decodeBeadList bs =
  case Data.Aeson.decode bs of
    Just beads -> Just beads
    Nothing -> Nothing


-- | Check if a bead is unblocked (no open dependencies).
isUnblocked :: BeadInfo -> Bool
isUnblocked bead =
  all depIsClosed bead.biDependencies
  where
    depIsClosed (DependencyInfo { diStatus = s }) = s == StatusClosed


-- | Find epic matching worktree name.
--
-- Matches by:
-- 1. Title contains worktree name
-- 2. ID contains worktree name
findMatchingEpic :: Text -> [BeadInfo] -> Maybe BeadInfo
findMatchingEpic wtName epics =
  listToMaybe $ filter matches epics
  where
    matches epic =
      T.toLower wtName `T.isInfixOf` T.toLower epic.biTitle ||
      T.toLower wtName `T.isInfixOf` T.toLower epic.biId


-- | Sort beads by priority (lower = higher priority).
sortByPriority :: [BeadInfo] -> [BeadInfo]
sortByPriority = sortOn (.biPriority)
