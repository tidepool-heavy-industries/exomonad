{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Testrunner role config for e2e-test: instruct + post_review + notify_parent.
-- Identical to devswarm/TestrunnerRole.hs.
module TestrunnerRole (config, Tools) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import ExoMonad
import ExoMonad.Guest.Tools.Events
  ( notifyParentCore, notifyParentDescription, notifyParentSchema, NotifyParentArgs
  )
import ExoMonad.Guest.Tools.TestRunner
  ( instructCore, instructDescription, instructSchema, InstructArgs
  , postReviewCore, postReviewDescription, postReviewSchema, PostReviewArgs
  )
import ExoMonad.Guest.Types (allowResponse, allowStopResponse, postToolUseResponse, BeforeModelOutput (..), AfterModelOutput (..))
import ExoMonad.Types (HookConfig (..), defaultSessionStartHook)

-- | Instruct tool: sends a message to the root agent under test.
data Instruct

instance MCPTool Instruct where
  type ToolArgs Instruct = InstructArgs
  toolName = "instruct"
  toolDescription = instructDescription
  toolSchema = instructSchema
  toolHandlerEff args = do
    result <- instructCore args
    case result of
      Left err -> pure $ errorResult err
      Right val -> pure $ successResult val

-- | Post review tool: simulate a Copilot review via mock GitHub control API.
data PostReview

instance MCPTool PostReview where
  type ToolArgs PostReview = PostReviewArgs
  toolName = "post_review"
  toolDescription = postReviewDescription
  toolSchema = postReviewSchema
  toolHandlerEff args = do
    result <- postReviewCore args
    case result of
      Left err -> pure $ errorResult err
      Right val -> pure $ successResult val

-- | Testrunner notify_parent: thin wrapper, no phase transitions.
data TestrunnerNotifyParent

instance MCPTool TestrunnerNotifyParent where
  type ToolArgs TestrunnerNotifyParent = NotifyParentArgs
  toolName = "notify_parent"
  toolDescription = notifyParentDescription
  toolSchema = notifyParentSchema
  toolHandlerEff args = do
    result <- notifyParentCore args
    case result of
      Left err -> pure $ errorResult err
      Right _ -> pure $ successResult $ object ["success" .= True]

data Tools mode = Tools
  { instruct :: mode :- Instruct,
    postReview :: mode :- PostReview,
    notifyParent :: mode :- TestrunnerNotifyParent
  }
  deriving (Generic)

config :: RoleConfig (Tools AsHandler)
config =
  RoleConfig
    { roleName = "testrunner",
      tools =
        Tools
          { instruct = mkHandler @Instruct,
            postReview = mkHandler @PostReview,
            notifyParent = mkHandler @TestrunnerNotifyParent
          },
      hooks =
        HookConfig
          { preToolUse = \_ -> pure (allowResponse Nothing),
            postToolUse = \_ -> pure (postToolUseResponse Nothing),
            onStop = \_ -> pure allowStopResponse,
            onSubagentStop = \_ -> pure allowStopResponse,
            onSessionStart = defaultSessionStartHook,
            beforeModel = \_ -> pure (BeforeModelAllow Nothing),
            afterModel = \_ -> pure (AfterModelAllow Nothing)
          },
      eventHandlers = defaultEventHandlers
    }
