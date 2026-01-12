{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Effect for querying git repository state.
--
-- Read-only queries for current HEAD and working directory status.
-- Used by handlers to track commits made during LLM sessions.
module TypesFirstDev.Effect.GitQuery
  ( -- * Effect
    GitQuery(..)
  , queryGitStatus

    -- * Types
  , GitStatus(..)

    -- * Interpreter
  , runGitQuery
  ) where

import Control.Monad.Freer (Eff, LastMember, Member, interpret, send, sendM)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Current git repository state.
data GitStatus = GitStatus
  { gsHeadHash      :: Text    -- ^ Current HEAD commit hash (short)
  , gsHeadMessage   :: Text    -- ^ Current HEAD commit message (first line)
  , gsStagedFiles   :: [Text]  -- ^ Files staged for commit
  , gsUnstagedFiles :: [Text]  -- ^ Modified/untracked files not staged
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Git query effect.
data GitQuery r where
  -- | Query current git status (HEAD + staging state)
  QueryGitStatus :: FilePath -> GitQuery GitStatus


-- | Query git status in the given directory.
queryGitStatus :: Member GitQuery es => FilePath -> Eff es GitStatus
queryGitStatus path = send $ QueryGitStatus path


-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run GitQuery effect by shelling out to git.
runGitQuery
  :: LastMember IO es
  => Eff (GitQuery ': es) a
  -> Eff es a
runGitQuery = interpret $ \case
  QueryGitStatus path -> sendM $ queryGitStatusIO path


-- | Query git status via shell commands.
queryGitStatusIO :: FilePath -> IO GitStatus
queryGitStatusIO path = do
  -- Get HEAD hash (short)
  headHash <- gitInDir path ["rev-parse", "--short", "HEAD"]

  -- Get HEAD message (first line)
  headMessage <- gitInDir path ["log", "-1", "--format=%s"]

  -- Get status (porcelain format)
  statusOutput <- gitInDir path ["status", "--porcelain"]

  let (staged, unstaged) = parseStatusOutput statusOutput

  pure GitStatus
    { gsHeadHash = T.strip $ T.pack headHash
    , gsHeadMessage = T.strip $ T.pack headMessage
    , gsStagedFiles = staged
    , gsUnstagedFiles = unstaged
    }


-- | Run git command in a directory.
gitInDir :: FilePath -> [String] -> IO String
gitInDir dir args = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "git" (["-C", dir] <> args) ""
  case exitCode of
    ExitSuccess -> pure stdout
    ExitFailure _ -> pure $ "error: " <> stderr


-- | Parse git status --porcelain output into (staged, unstaged) files.
--
-- Format: XY filename
--   X = staging area status
--   Y = working tree status
--   ' ' = unmodified, M = modified, A = added, D = deleted, ? = untracked
parseStatusOutput :: String -> ([Text], [Text])
parseStatusOutput output =
  let ls = filter (not . null) $ lines output
      parsed = map parseLine ls
      staged = [f | (True, _, f) <- parsed]
      unstaged = [f | (_, True, f) <- parsed]
  in (staged, unstaged)
  where
    parseLine :: String -> (Bool, Bool, Text)
    parseLine line = case line of
      (x:y:' ':rest) ->
        let file = T.pack rest
            isStaged = x /= ' ' && x /= '?'
            isUnstaged = y /= ' ' || x == '?'
        in (isStaged, isUnstaged, file)
      _ -> (False, False, T.pack line)
