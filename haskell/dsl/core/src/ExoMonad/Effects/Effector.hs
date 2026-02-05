{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Effects.Effector
  ( -- * Effect
    Effector (..),
    runEffector,
    effectorGitStatus,
    effectorGitDiff,
    effectorGitLsFiles,

    -- * Types
    GhPrStatusResult (..),
    GhPrCreateResult (..),
    PrComment (..),
    GitStatusResult (..),
    GitDiffResult (..),
    DiffFile (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:))
import Data.Kind (Type)
import Polysemy (Member, Sem, makeSem)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES: GitHub
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of @effector gh pr-status@
data GhPrStatusResult = GhPrStatusResult
  { exists :: Bool,
    url :: Maybe Text,
    number :: Maybe Int,
    state :: Maybe Text,
    review_status :: Maybe Text,
    comments :: [PrComment]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Comment on a PR, potentially inline
data PrComment = PrComment
  { author :: Text,
    body :: Text,
    path :: Maybe Text,
    line :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Result of @effector gh pr-create@
data GhPrCreateResult = GhPrCreateResult
  { url :: Text,
    number :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES: Git
-- ════════════════════════════════════════════════════════════════════════════

data GitStatusResult = GitStatusResult
  { gsrBranch :: Text,
    gsrDirty :: [FilePath],
    gsrStaged :: [FilePath],
    gsrAhead :: Int,
    gsrBehind :: Int
  }
  deriving (Show, Eq, Generic)

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
  { gdrFiles :: [DiffFile],
    gdrAdditions :: Int,
    gdrDeletions :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON GitDiffResult where
  parseJSON = withObject "GitDiffResult" $ \v ->
    GitDiffResult
      <$> v .: "files"
      <*> v .: "additions"
      <*> v .: "deletions"

instance ToJSON GitDiffResult

data DiffFile = DiffFile
  { dfPath :: FilePath,
    dfStatus :: Text,
    dfAdditions :: Int,
    dfDeletions :: Int
  }
  deriving (Show, Eq, Generic)

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
data Effector m a where
  RunEffector :: Text -> [Text] -> Effector m Text
  EffectorGitStatus :: FilePath -> Effector m GitStatusResult
  EffectorGitDiff :: FilePath -> Bool -> Effector m GitDiffResult
  EffectorGitLsFiles :: FilePath -> [Text] -> Effector m [FilePath]

makeSem ''Effector
