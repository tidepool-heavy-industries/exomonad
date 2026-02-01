{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Guest.Effects.WorkflowState
  ( -- * Effect type
    WorkflowState (..),
    -- * Smart constructors
    getAttempts,
    incrementAttempts,
    resetAttempts,
    -- * Interpreter
    runWorkflowState,
    -- * Types
    AgentIdInput (..),
  )
where

import Control.Monad.Freer
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.HostCall (callHost, host_workflow_state_get_attempts, host_workflow_state_increment_attempts, host_workflow_state_reset_attempts)
import ExoMonad.Guest.Effects.FileSystem (HostResult (..))
import GHC.Generics (Generic)

-- Types

data AgentIdInput = AgentIdInput
  { aiiAgentId :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON AgentIdInput
instance FromJSON AgentIdInput

-- Effect

data WorkflowState r where
  GetAttempts :: Text -> WorkflowState (Either Text Int)
  IncrementAttempts :: Text -> WorkflowState (Either Text ())
  ResetAttempts :: Text -> WorkflowState (Either Text ())

-- Smart Constructors

getAttempts :: (Member WorkflowState effs) => Text -> Eff effs (Either Text Int)
getAttempts agentId = send (GetAttempts agentId)

incrementAttempts :: (Member WorkflowState effs) => Text -> Eff effs (Either Text ())
incrementAttempts agentId = send (IncrementAttempts agentId)

resetAttempts :: (Member WorkflowState effs) => Text -> Eff effs (Either Text ())
resetAttempts agentId = send (ResetAttempts agentId)

-- Interpreter

runWorkflowState :: (LastMember IO effs) => Eff (WorkflowState ': effs) a -> Eff effs a
runWorkflowState = interpret $ \case
  GetAttempts agentId -> sendM $ do
    let input = AgentIdInput agentId
    res <- callHost host_workflow_state_get_attempts input
    pure $ case res of
      Left err -> Left (T.pack err)
      Right (Success r) -> Right r
      Right (HostError msg) -> Left msg

  IncrementAttempts agentId -> sendM $ do
    let input = AgentIdInput agentId
    res <- callHost host_workflow_state_increment_attempts input
    pure $ case res of
      Left err -> Left (T.pack err)
      Right (Success r) -> Right r
      Right (HostError msg) -> Left msg

  ResetAttempts agentId -> sendM $ do
    let input = AgentIdInput agentId
    res <- callHost host_workflow_state_reset_attempts input
    pure $ case res of
      Left err -> Left (T.pack err)
      Right (Success r) -> Right r
      Right (HostError msg) -> Left msg
