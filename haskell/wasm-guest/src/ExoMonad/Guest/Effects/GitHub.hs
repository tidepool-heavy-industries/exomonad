{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Guest.Effects.GitHub
  ( -- * Effect type
    GitHub (..),

    -- * Smart constructors
    ghListPRs,
    ghCreatePR,

    -- * Interpreter
    runGitHub,

    -- * Types
    Repo (..),
    PRFilter (..),
    CreatePRSpec (..),
    PullRequest (..),
  )
where

import Control.Monad.Freer
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.Effects.FileSystem (HostResult (..))
import ExoMonad.Guest.HostCall (callHost, host_github_create_pr, host_github_list_prs)
import GHC.Generics (Generic)

-- Types

data Repo = Repo
  { repoOwner :: Text,
    repoName :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Repo where
  toJSON (Repo o n) = object ["owner" .= o, "name" .= n]

instance FromJSON Repo where
  parseJSON = withObject "Repo" $ \v ->
    Repo
      <$> v .: "owner"
      <*> v .: "name"

data PRFilter = PRFilter
  { prfState :: Maybe Text, -- "open", "closed", "all"
    prfLimit :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON PRFilter where
  toJSON (PRFilter s l) =
    object
      [ "state" .= s,
        "limit" .= l
      ]

instance FromJSON PRFilter where
  parseJSON = withObject "PRFilter" $ \v ->
    PRFilter
      <$> v .:? "state"
      <*> v .:? "limit"

data CreatePRSpec = CreatePRSpec
  { cprTitle :: Text,
    cprBody :: Text,
    cprHead :: Text,
    cprBase :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON CreatePRSpec where
  toJSON (CreatePRSpec t b h base) =
    object
      [ "title" .= t,
        "body" .= b,
        "head" .= h,
        "base" .= base
      ]

instance FromJSON CreatePRSpec where
  parseJSON = withObject "CreatePRSpec" $ \v ->
    CreatePRSpec
      <$> v .: "title"
      <*> v .: "body"
      <*> v .: "head"
      <*> v .: "base"

data PullRequest = PullRequest
  { prNumber :: Int,
    prTitle :: Text,
    prBody :: Text,
    prState :: Text,
    prUrl :: Text,
    prAuthor :: Text,
    prHeadRef :: Text,
    prBaseRef :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON PullRequest where
  parseJSON = withObject "PullRequest" $ \v ->
    PullRequest
      <$> v .: "number"
      <*> v .: "title"
      <*> v .: "body"
      <*> v .: "state"
      <*> v .: "url"
      <*> v .: "author"
      <*> v .: "head_ref"
      <*> v .: "base_ref"

instance ToJSON PullRequest

-- Input structs for Host Calls

data ListPRsInput = ListPRsInput
  { lpiRepo :: Repo,
    lpiFilter :: Maybe PRFilter
  }
  deriving (Show, Eq, Generic)

instance ToJSON ListPRsInput where
  toJSON (ListPRsInput r f) = object ["repo" .= r, "filter" .= f]

data CreatePRInput = CreatePRInput
  { cpiRepo :: Repo,
    cpiSpec :: CreatePRSpec
  }
  deriving (Show, Eq, Generic)

instance ToJSON CreatePRInput where
  toJSON (CreatePRInput r s) = object ["repo" .= r, "spec" .= s]

-- Effect

data GitHub r where
  GhListPRs :: Repo -> Maybe PRFilter -> GitHub (Either Text [PullRequest])
  GhCreatePR :: Repo -> CreatePRSpec -> GitHub (Either Text PullRequest)

-- Smart Constructors

ghListPRs :: (Member GitHub effs) => Repo -> Maybe PRFilter -> Eff effs (Either Text [PullRequest])
ghListPRs r f = send (GhListPRs r f)

ghCreatePR :: (Member GitHub effs) => Repo -> CreatePRSpec -> Eff effs (Either Text PullRequest)
ghCreatePR r s = send (GhCreatePR r s)

-- Interpreter

runGitHub :: (LastMember IO effs) => Eff (GitHub ': effs) a -> Eff effs a
runGitHub = interpret $ \case
  GhListPRs r f -> sendM $ do
    let input = ListPRsInput r f
    res <- callHost host_github_list_prs input
    pure $ case res of
      Left err -> Left (T.pack err)
      Right (Success ps) -> Right ps
      Right (HostError msg) -> Left msg
  GhCreatePR r s -> sendM $ do
    let input = CreatePRInput r s
    res <- callHost host_github_create_pr input
    pure $ case res of
      Left err -> Left (T.pack err)
      Right (Success p) -> Right p
      Right (HostError msg) -> Left msg
