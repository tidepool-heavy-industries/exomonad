{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module ExoMonad.Generated.Effects
  ( ExoMonadTypes (..),
    Effect (..),
    Common (..),
    Payload (..),
    IssueFilter (..),
    Repo (..),
    EffectResult (..),
    WorktreeInfo (..),
    EffectKind (..),
    EffectResultKind (..),
  )
where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

type Common = Maybe Text

data ExoMonadTypes = ExoMonadTypes
  { effectExoMonadTypes :: Effect,
    effectResultExoMonadTypes :: EffectResult,
    worktreeInfoExoMonadTypes :: Maybe WorktreeInfo
  }
  deriving (Show)

data Effect = Effect
  { kindEffect :: EffectKind,
    payloadEffect :: Payload
  }
  deriving (Show)

data EffectKind
  = DockerExecEffectKind
  | GitGetBranchEffectKind
  | GitGetWorktreeEffectKind
  | GitHubListIssuesEffectKind
  | LogEffectKind
  deriving (Show)

data Payload = Payload
  { fieldsPayload :: Maybe (HashMap Text Text),
    levelPayload :: Maybe Text,
    messagePayload :: Maybe Text,
    workingDirPayload :: Maybe Text,
    filterPayload :: Maybe IssueFilter,
    repoPayload :: Maybe Repo,
    commandPayload :: Maybe (Vector Text),
    containerIDPayload :: Maybe Text
  }
  deriving (Show)

data IssueFilter = IssueFilter
  { assigneeIssueFilter :: Maybe Text,
    labelsIssueFilter :: Maybe (Vector Text),
    stateIssueFilter :: Maybe Text
  }
  deriving (Show)

data Repo = Repo
  { nameRepo :: Text,
    ownerRepo :: Text
  }
  deriving (Show)

data EffectResult = EffectResult
  { kindEffectResult :: EffectResultKind,
    payloadEffectResult :: Maybe Text
  }
  deriving (Show)

data EffectResultKind
  = KindErrorEffectResultKind
  | SuccessEffectResultKind
  deriving (Show)

data WorktreeInfo = WorktreeInfo
  { branchWorktreeInfo :: Text,
    commitWorktreeInfo :: Text,
    pathWorktreeInfo :: Text
  }
  deriving (Show)

instance ToJSON ExoMonadTypes where
  toJSON (ExoMonadTypes effectExoMonadTypes effectResultExoMonadTypes worktreeInfoExoMonadTypes) =
    object
      [ "effect" .= effectExoMonadTypes,
        "effectResult" .= effectResultExoMonadTypes,
        "worktreeInfo" .= worktreeInfoExoMonadTypes
      ]

instance FromJSON ExoMonadTypes where
  parseJSON (Object v) =
    ExoMonadTypes
      <$> v .: "effect"
      <*> v .: "effectResult"
      <*> v .:? "worktreeInfo"

instance ToJSON Effect where
  toJSON (Effect kindEffect payloadEffect) =
    object
      [ "kind" .= kindEffect,
        "payload" .= payloadEffect
      ]

instance FromJSON Effect where
  parseJSON (Object v) =
    Effect
      <$> v .: "kind"
      <*> v .: "payload"

instance ToJSON EffectKind where
  toJSON DockerExecEffectKind = "DockerExec"
  toJSON GitGetBranchEffectKind = "GitGetBranch"
  toJSON GitGetWorktreeEffectKind = "GitGetWorktree"
  toJSON GitHubListIssuesEffectKind = "GitHubListIssues"
  toJSON LogEffectKind = "Log"

instance FromJSON EffectKind where
  parseJSON = withText "EffectKind" parseText
    where
      parseText "DockerExec" = return DockerExecEffectKind
      parseText "GitGetBranch" = return GitGetBranchEffectKind
      parseText "GitGetWorktree" = return GitGetWorktreeEffectKind
      parseText "GitHubListIssues" = return GitHubListIssuesEffectKind
      parseText "Log" = return LogEffectKind

instance ToJSON Payload where
  toJSON (Payload fieldsPayload levelPayload messagePayload workingDirPayload filterPayload repoPayload commandPayload containerIDPayload) =
    object
      [ "fields" .= fieldsPayload,
        "level" .= levelPayload,
        "message" .= messagePayload,
        "workingDir" .= workingDirPayload,
        "filter" .= filterPayload,
        "repo" .= repoPayload,
        "command" .= commandPayload,
        "containerId" .= containerIDPayload
      ]

instance FromJSON Payload where
  parseJSON (Object v) =
    Payload
      <$> v .:? "fields"
      <*> v .:? "level"
      <*> v .:? "message"
      <*> v .:? "workingDir"
      <*> v .:? "filter"
      <*> v .:? "repo"
      <*> v .:? "command"
      <*> v .:? "containerId"

instance ToJSON IssueFilter where
  toJSON (IssueFilter assigneeIssueFilter labelsIssueFilter stateIssueFilter) =
    object
      [ "assignee" .= assigneeIssueFilter,
        "labels" .= labelsIssueFilter,
        "state" .= stateIssueFilter
      ]

instance FromJSON IssueFilter where
  parseJSON (Object v) =
    IssueFilter
      <$> v .:? "assignee"
      <*> v .:? "labels"
      <*> v .:? "state"

instance ToJSON Repo where
  toJSON (Repo nameRepo ownerRepo) =
    object
      [ "name" .= nameRepo,
        "owner" .= ownerRepo
      ]

instance FromJSON Repo where
  parseJSON (Object v) =
    Repo
      <$> v .: "name"
      <*> v .: "owner"

instance ToJSON EffectResult where
  toJSON (EffectResult kindEffectResult payloadEffectResult) =
    object
      [ "kind" .= kindEffectResult,
        "payload" .= payloadEffectResult
      ]

instance FromJSON EffectResult where
  parseJSON (Object v) =
    EffectResult
      <$> v .: "kind"
      <*> v .: "payload"

instance ToJSON EffectResultKind where
  toJSON KindErrorEffectResultKind = "Error"
  toJSON SuccessEffectResultKind = "Success"

instance FromJSON EffectResultKind where
  parseJSON = withText "EffectResultKind" parseText
    where
      parseText "Error" = return KindErrorEffectResultKind
      parseText "Success" = return SuccessEffectResultKind

instance ToJSON WorktreeInfo where
  toJSON (WorktreeInfo branchWorktreeInfo commitWorktreeInfo pathWorktreeInfo) =
    object
      [ "branch" .= branchWorktreeInfo,
        "commit" .= commitWorktreeInfo,
        "path" .= pathWorktreeInfo
      ]

instance FromJSON WorktreeInfo where
  parseJSON (Object v) =
    WorktreeInfo
      <$> v .: "branch"
      <*> v .: "commit"
      <*> v .: "path"
