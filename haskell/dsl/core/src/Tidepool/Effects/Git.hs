-- | Git effect for querying repository state.
--
-- Effect type only - interpreters live in tidepool-bd-interpreter.
-- Enables graphs to query git worktree info, dirty files, and commit history.
--
-- = Example Usage
--
-- @
-- import Tidepool.Effects.Git (Git, getWorktreeInfo, getDirtyFiles)
--
-- myHandler :: Member Git effs => Eff effs ()
-- myHandler = do
--   wt <- getWorktreeInfo
--   dirtyFiles <- getDirtyFiles
--   -- process git info...
-- @
module Tidepool.Effects.Git
  ( -- * Effect
    Git(..)
  , getWorktreeInfo
  , getDirtyFiles
  , getRecentCommits
  , getCurrentBranch
  , getCommitsAhead

    -- * Types
  , WorktreeInfo(..)
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson (ToJSON(..), FromJSON(..), object, withObject, (.=), (.:))
import Data.Text (Text)
import GHC.Generics (Generic)


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Information about the current git worktree or repo.
data WorktreeInfo = WorktreeInfo
  { wiName       :: Text
    -- ^ Worktree name (e.g., "bd", "native-server").
    -- For main repo, this is "main".
  , wiPath       :: FilePath
    -- ^ Absolute path to the worktree root.
  , wiBranch     :: Text
    -- ^ Current branch name.
  , wiRepoRoot   :: FilePath
    -- ^ Path to the main git repository (may be same as wiPath).
  , wiIsWorktree :: Bool
    -- ^ True if this is a worktree, False if main repo.
  }
  deriving (Show, Eq, Generic)

instance ToJSON WorktreeInfo where
  toJSON wt = object
    [ "name"        .= wt.wiName
    , "path"        .= wt.wiPath
    , "branch"      .= wt.wiBranch
    , "repo_root"   .= wt.wiRepoRoot
    , "is_worktree" .= wt.wiIsWorktree
    ]

instance FromJSON WorktreeInfo where
  parseJSON = withObject "WorktreeInfo" $ \v ->
    WorktreeInfo
      <$> v .: "name"
      <*> v .: "path"
      <*> v .: "branch"
      <*> v .: "repo_root"
      <*> v .: "is_worktree"


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Git effect for querying repository state.
--
-- Read-only queries against git.
data Git r where
  -- | Get worktree/repo information.
  -- Returns Nothing if not in a git repository.
  GetWorktreeInfo :: Git (Maybe WorktreeInfo)

  -- | Get list of dirty (uncommitted) files.
  GetDirtyFiles :: Git [FilePath]

  -- | Get recent commit subjects.
  GetRecentCommits :: Int -> Git [Text]

  -- | Get current branch name.
  GetCurrentBranch :: Git Text

  -- | Get number of commits ahead of a ref (e.g., "origin/main").
  GetCommitsAhead :: Text -> Git Int


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Get worktree/repo information.
getWorktreeInfo :: Member Git effs => Eff effs (Maybe WorktreeInfo)
getWorktreeInfo = send GetWorktreeInfo

-- | Get list of dirty files.
getDirtyFiles :: Member Git effs => Eff effs [FilePath]
getDirtyFiles = send GetDirtyFiles

-- | Get recent commit subjects.
getRecentCommits :: Member Git effs => Int -> Eff effs [Text]
getRecentCommits = send . GetRecentCommits

-- | Get current branch name.
getCurrentBranch :: Member Git effs => Eff effs Text
getCurrentBranch = send GetCurrentBranch

-- | Get number of commits ahead of a ref (e.g., "origin/main").
getCommitsAhead :: Member Git effs => Text -> Eff effs Int
getCommitsAhead = send . GetCommitsAhead
