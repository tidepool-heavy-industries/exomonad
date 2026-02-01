{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | LLM effects for AI capabilities.
module ExoMonad.Guest.Effects.LLM
  ( -- * Effect type
    LLM (..),

    -- * Smart constructors
    complete,

    -- * Interpreter
    runLLM,

    -- * Types
    ChatMessage (..),
    LlmCompleteInput (..),
    LlmCompleteOutput (..),
    Usage (..),
    HostResult (..),
  )
where

import Control.Monad.Freer
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.HostCall (callHost, host_llm_complete)
import GHC.Generics (Generic)

-- ============================================================================
-- Types
-- ============================================================================

data ChatMessage = ChatMessage
  { cmRole :: Text,
    cmContent :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ChatMessage where
  toJSON (ChatMessage r c) =
    object
      [ "role" .= r,
        "content" .= c
      ]

instance FromJSON ChatMessage where
  parseJSON = withObject "ChatMessage" $ \v ->
    ChatMessage
      <$> v .: "role"
      <*> v .: "content"

data LlmCompleteInput = LlmCompleteInput
  { lciModel :: Text,
    lciMessages :: [ChatMessage],
    lciMaxTokens :: Int,
    lciSystem :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON LlmCompleteInput where
  toJSON (LlmCompleteInput m ms mt s) =
    object
      [ "model" .= m,
        "messages" .= ms,
        "max_tokens" .= mt,
        "system" .= s
      ]

data Usage = Usage
  { usageInputTokens :: Int,
    usageOutputTokens :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \v ->
    Usage
      <$> v .: "input_tokens"
      <*> v .: "output_tokens"

data LlmCompleteOutput = LlmCompleteOutput
  { lcoContent :: Text,
    lcoStopReason :: Text,
    lcoUsage :: Usage
  }
  deriving (Show, Eq, Generic)

instance FromJSON LlmCompleteOutput where
  parseJSON = withObject "LlmCompleteOutput" $ \v ->
    LlmCompleteOutput
      <$> v .: "content"
      <*> v .: "stop_reason"
      <*> v .: "usage"

-- | Host result wrapper (matches Rust HostResult).
data HostResult a
  = Success a
  | HostError Text
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (HostResult a) where
  parseJSON = withObject "HostResult" $ \v -> do
    kind <- v .: "kind" :: Parser Text
    case kind of
      "Success" -> Success <$> v .: "payload"
      "Error" -> do
        errObj <- v .: "payload"
        HostError <$> (errObj .: "message")
      _ -> fail "Unknown HostResult kind"

-- ============================================================================
-- Effect type
-- ============================================================================

data LLM r where
  Complete :: Text -> [ChatMessage] -> Int -> Maybe Text -> LLM LlmCompleteOutput

-- ============================================================================
-- Smart constructors
-- ============================================================================

complete :: (Member LLM effs) => Text -> [ChatMessage] -> Int -> Maybe Text -> Eff effs LlmCompleteOutput
complete model messages maxTokens system = send (Complete model messages maxTokens system)

-- ============================================================================
-- Interpreter
-- ============================================================================

runLLM :: (LastMember IO effs) => Eff (LLM ': effs) a -> Eff effs a
runLLM = interpret $ \case
  Complete model messages maxTokens system -> sendM $ do
    let input = LlmCompleteInput model messages maxTokens system
    res <- callHost host_llm_complete input
    case res of
      Left err -> error $ "LLM host call failed: " ++ err
      Right (Success output) -> pure output
      Right (HostError msg) -> error $ "LLM error: " ++ T.unpack msg
