{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Dev role config: PR and notify tools with state transitions and stop hook checks.
module DevRole (config, Tools) where

import Control.Monad (void)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import ExoMonad
import ExoMonad.Guest.Tools.FilePR (filePRCore, filePRDescription, filePRSchema, FilePRArgs, FilePROutput (..))
import ExoMonad.Guest.Tools.Events
  ( notifyParentCore, notifyParentDescription, notifyParentSchema, NotifyParentArgs (..), NotifyStatus (..),
    shutdownCore, shutdownDescription, shutdownSchema, ShutdownArgs
  )
import ExoMonad.Guest.StateMachine (applyEvent)
import DevPhase (DevPhase (..), DevEvent (..))
import HttpDevHooks (httpDevHooks)
import PRReviewHandler (prReviewEventHandlers)

-- | Dev-specific file_pr: files PR, then transitions DevPhase.
data DevFilePR

instance MCPTool DevFilePR where
  type ToolArgs DevFilePR = FilePRArgs
  toolName = "file_pr"
  toolDescription = filePRDescription
  toolSchema = filePRSchema
  toolHandlerEff args = do
    result <- filePRCore args
    case result of
      Left err -> pure $ errorResult err
      Right output -> do
        void $ applyEvent @DevPhase @DevEvent DevSpawned
          (PRCreated (fpoNumber output) (fpoUrl output) (fpoHeadBranch output))
        pure $ successResult (Aeson.toJSON output)

-- | Dev-specific notify_parent: notifies parent, then transitions to DevDone/DevFailed.
data DevNotifyParent

instance MCPTool DevNotifyParent where
  type ToolArgs DevNotifyParent = NotifyParentArgs
  toolName = "notify_parent"
  toolDescription = notifyParentDescription
  toolSchema = notifyParentSchema
  toolHandlerEff args = do
    result <- notifyParentCore args
    case result of
      Left err -> pure $ errorResult err
      Right _ -> do
        case npStatus args of
          Success -> void $ applyEvent @DevPhase @DevEvent DevSpawned (NotifyParentSuccess (npMessage args))
          Failure -> void $ applyEvent @DevPhase @DevEvent DevSpawned (NotifyParentFailure (npMessage args))
        pure $ successResult $ object ["success" .= True]

-- | Dev-specific shutdown: transitions to DevDone, then exits.
data DevShutdown

instance MCPTool DevShutdown where
  type ToolArgs DevShutdown = ShutdownArgs
  toolName = "shutdown"
  toolDescription = shutdownDescription
  toolSchema = shutdownSchema
  toolHandlerEff args = do
    void $ applyEvent @DevPhase @DevEvent DevSpawned ShutdownRequested
    shutdownCore args

data Tools mode = Tools
  { pr :: mode :- DevFilePR,
    notifyParent :: mode :- DevNotifyParent,
    sendMessage :: mode :- SendMessage,
    shutdown :: mode :- DevShutdown
  }
  deriving (Generic)

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "dev",
      tools =
        Tools
          { pr = mkHandler @DevFilePR,
            notifyParent = mkHandler @DevNotifyParent,
            sendMessage = mkHandler @SendMessage,
            shutdown = mkHandler @DevShutdown
          },
      hooks = httpDevHooks,
      eventHandlers = prReviewEventHandlers
    }
