-- | Git effect interpreter - pure handler injection for testing.
module ExoMonad.Git.Interpreter
  ( runGit,
    worktreeName,
  )
where

import Control.Monad.Freer (Eff, interpret)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Effects.Git (Git (..), WorktreeInfo)
import System.FilePath (takeFileName)

-- | Run Git effects with pure handlers (for testing).
runGit ::
  Eff effs (Maybe WorktreeInfo) ->
  Eff effs [FilePath] ->
  (Int -> Eff effs [Text]) ->
  Eff effs Text ->
  (Text -> Eff effs Int) ->
  (Text -> Maybe Text -> Eff effs ()) ->
  Eff (Git ': effs) a ->
  Eff effs a
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
