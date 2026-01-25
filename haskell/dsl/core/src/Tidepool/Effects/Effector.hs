{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tidepool.Effects.Effector
  ( -- * Effect
    Effector(..)
  , runEffector
  , effectorGitStatus
  , effectorGitDiff
  , effectorGitLsFiles

    -- * Types
  , GhPrStatusResult(..)
  , GhPrCreateResult(..)
  , PrComment(..)
  , GitStatusResult(..)
  , GitDiffResult(..)
  , DiffFile(..)
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.=), (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES: GitHub
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of @effector gh pr-status@
data GhPrStatusResult = GhPrStatusResult
  { exists :: Bool
  , url :: Maybe Text
  , number :: Maybe Int
  , state :: Maybe Text
  , review_status :: Maybe Text
  , comments :: [PrComment]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Comment on a PR, potentially inline
data PrComment = PrComment
  { author :: Text
  , body :: Text
  , path :: Maybe Text
  , line :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Result of @effector gh pr-create@
data GhPrCreateResult = GhPrCreateResult
  { url :: Text
  , number :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES: Git
-- ════════════════════════════════════════════════════════════════════════════

data GitStatusResult = GitStatusResult
  { gsrBranch :: Text
  , gsrDirty :: [FilePath]
  , gsrStaged :: [FilePath]
  , gsrAhead :: Int
  , gsrBehind :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON GitStatusResult where
  parseJSON = withObject "GitStatusResult" $ \v ->
    GitStatusResult
      <$> v .: "branch"
      <*> v .: "dirty"
      <*> v .: "staged"
      <*> v .: "ahead"
      <*> v .: "behind"

instance ToJSON GitStatusResult

data GitDiffResult = GitDiffResult
  { gdrFiles :: [DiffFile]
  , gdrAdditions :: Int
  , gdrDeletions :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON GitDiffResult where
  parseJSON = withObject "GitDiffResult" $ \v ->
    GitDiffResult
      <$> v .: "files"
      <*> v .: "additions"
      <*> v .: "deletions"

instance ToJSON GitDiffResult

data DiffFile = DiffFile
  { dfPath :: FilePath
  , dfStatus :: Text
  , dfAdditions :: Int
  , dfDeletions :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON DiffFile where
  parseJSON = withObject "DiffFile" $ \v ->
    DiffFile
      <$> v .: "path"
      <*> v .: "status"
      <*> v .: "additions"
      <*> v .: "deletions"

instance ToJSON DiffFile

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Effector effect for running the @effector@ tool.
--
-- This tool handles SSH transparently for subagent control.
data Effector r where
  RunEffector :: Text -> [Text] -> Effector Text
  EffectorGitStatus :: FilePath -> Effector GitStatusResult
  EffectorGitDiff :: FilePath -> Bool -> Effector GitDiffResult
  EffectorGitLsFiles :: FilePath -> [Text] -> Effector [FilePath]

-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the @effector@ tool with the given command and arguments.
-- Returns the raw stdout as Text.
runEffector :: Member Effector effs => Text -> [Text] -> Eff effs Text
runEffector cmd args = send (RunEffector cmd args)

effectorGitStatus :: Member Effector effs => FilePath -> Eff effs GitStatusResult
effectorGitStatus cwd = send (EffectorGitStatus cwd)

effectorGitDiff :: Member Effector effs => FilePath -> Bool -> Eff effs GitDiffResult
effectorGitDiff cwd staged = send (EffectorGitDiff cwd staged)

effectorGitLsFiles :: Member Effector effs => FilePath -> [Text] -> Eff effs [FilePath]
effectorGitLsFiles cwd args = send (EffectorGitLsFiles cwd args)