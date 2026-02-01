{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Guest.Effects.Git
  ( -- * Effect type
    Git (..),
    -- * Smart constructors
    gitDiff,
    gitBranch,
    gitRemoteUrl,
    -- * Interpreter
    runGit,
    -- * Types
    GitInput (..),
  )
where

import Control.Monad.Freer
import Data.Aeson (FromJSON, ToJSON (..), object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.HostCall (callHost, host_git_get_dirty_files, host_git_get_branch, host_git_get_remote_url)
import ExoMonad.Guest.Effects.FileSystem (HostResult (..))
import GHC.Generics (Generic)

-- Types

data GitInput = GitInput
  { giWorkingDir :: Text,
    giContainerId :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON GitInput where
  toJSON (GitInput w c) = object
    [ "workingDir" .= w,
    "containerId" .= c
    ]

data GitRemoteInput = GitRemoteInput
  { griWorkingDir :: Text,
    griContainerId :: Text,
    griRemote :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON GitRemoteInput where
  toJSON (GitRemoteInput w c r) = object
    [ "workingDir" .= w,
    "containerId" .= c,
    "remote" .= r
    ]

-- Effect

data Git r where
  GitDiff :: Text -> Text -> Git (Either Text [Text]) -- workingDir, containerId -> changed files
  GitBranch :: Text -> Text -> Git (Either Text Text) -- workingDir, containerId -> branch
  GitRemoteUrl :: Text -> Text -> Text -> Git (Either Text Text) -- workingDir, containerId, remote -> url

-- Smart Constructors

gitDiff :: (Member Git effs) => Text -> Text -> Eff effs (Either Text [Text])
gitDiff wd cid = send (GitDiff wd cid)

gitBranch :: (Member Git effs) => Text -> Text -> Eff effs (Either Text Text)
gitBranch wd cid = send (GitBranch wd cid)

gitRemoteUrl :: (Member Git effs) => Text -> Text -> Text -> Eff effs (Either Text Text)
gitRemoteUrl wd cid remote = send (GitRemoteUrl wd cid remote)

-- Interpreter

runGit :: (LastMember IO effs) => Eff (Git ': effs) a -> Eff effs a
runGit = interpret $ \case
  GitDiff wd cid -> sendM $ do
    let input = GitInput wd cid
    res <- callHost host_git_get_dirty_files input
    pure $ case res of
      Left err -> Left (T.pack err)
      Right (Success r) -> Right (map T.pack r) -- Rust returns Vec<String>
      Right (HostError msg) -> Left msg
      
  GitBranch wd cid -> sendM $ do
    let input = GitInput wd cid
    res <- callHost host_git_get_branch input
    pure $ case res of
      Left err -> Left (T.pack err)
      Right (Success r) -> Right (T.pack r)
      Right (HostError msg) -> Left msg

  GitRemoteUrl wd cid remote -> sendM $ do
    let input = GitRemoteInput wd cid remote
    res <- callHost host_git_get_remote_url input
    pure $ case res of
      Left err -> Left (T.pack err)
      Right (Success r) -> Right (T.pack r)
      Right (HostError msg) -> Left msg
