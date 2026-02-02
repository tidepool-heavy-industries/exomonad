-- | Pure Git effect handler injection for testing (no CLI/IO interpreter).
--
-- Note: The IO-based interpreter `runGitIO` has been removed from this module.
-- For production use with remote execution, use `ExoMonad.Control.Effects.Git.runGitRemote`.
module ExoMonad.Git.Interpreter
  ( runGit,
    worktreeName,
  )
where

import Polysemy (Sem, interpret)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Effects.Git (Git (..), WorktreeInfo)
import System.FilePath (takeFileName)

-- | Run Git effects with pure handlers (for testing).
runGit ::
  Sem r (Maybe WorktreeInfo) ->
  Sem r [FilePath] ->
  (Int -> Sem r [Text]) ->
  Sem r Text ->
  (Text -> Sem r Int) ->
  (Text -> Maybe Text -> Sem r ()) ->
  Sem (Git ': r) a ->
  Sem r a
runGit hWorktree hDirty hCommits hBranch hAhead hFetch = interpret $ \case
  GetWorktreeInfo -> hWorktree
  GetDirtyFiles -> hDirty
  GetRecentCommits n -> hCommits n
  GetCurrentBranch -> hBranch
  GetCommitsAhead ref -> hAhead ref
  FetchRemote remote refspec -> hFetch remote refspec

-- | Extract worktree name from path.
worktreeName :: FilePath -> Text
worktreeName path =
  let parts = T.splitOn "/" (T.pack path)
      findAfterWorktrees [] = Nothing
      findAfterWorktrees [_] = Nothing
      findAfterWorktrees (x : y : rest)
        | x == "worktrees" = Just y
        | otherwise = findAfterWorktrees (y : rest)
   in case findAfterWorktrees parts of
        Just name -> name
        Nothing -> T.pack $ takeFileName path
