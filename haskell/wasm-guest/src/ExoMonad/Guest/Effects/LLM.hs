{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Guest.Effects.LLM where

import Control.Monad.Freer (Eff, LastMember, Member, interpret, send, sendM)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import ExoMonad.Guest.HostCall
import Extism.PDK.HTTP (Request (..), Response (..), newRequest, sendRequest)
import Extism.PDK.Memory (load)
import GHC.Generics (Generic)

-- Simple LLM Call effect
data LLMCall r where
  CallHaiku :: Text -> LLMCall (Either Text Text) -- Prompt -> Response

callHaiku :: (Member LLMCall effs) => Text -> Eff effs (Either Text Text)
callHaiku prompt = send (CallHaiku prompt)

-- Anthropic API Types
data AnthropicMessage = AnthropicMessage
  { role :: Text,
    content :: Text
  }
  deriving (Show, Generic)

instance ToJSON AnthropicMessage

data AnthropicRequest = AnthropicRequest
  { model :: Text,
    messages :: [AnthropicMessage],
    max_tokens :: Int
  }
  deriving (Show, Generic)

instance ToJSON AnthropicRequest

data AnthropicResponse = AnthropicResponse
  { content :: [AnthropicContent]
  }
  deriving (Show, Generic)

instance FromJSON AnthropicResponse

data AnthropicContent = AnthropicContent
  { text :: Text
  }
  deriving (Show, Generic)

instance FromJSON AnthropicContent

-- Secrets DTOs
data GetSecretInput = GetSecretInput
  { key :: Text
  }
  deriving (Show, Generic)

instance ToJSON GetSecretInput

data GetSecretOutput = GetSecretOutput
  { value :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON GetSecretOutput

runLLMCall :: (LastMember IO effs) => Eff (LLMCall ': effs) a -> Eff effs a
runLLMCall = interpret $ \case
  CallHaiku prompt -> sendM $ do
    -- Get API Key
    res <- callHost host_secrets_get (GetSecretInput "ANTHROPIC_API_KEY")
    case res of
      Left err -> pure $ Left $ "Failed to get API key: " <> T.pack err
      Right (GetSecretOutput Nothing) -> pure $ Left "ANTHROPIC_API_KEY not found in secrets"
      Right (GetSecretOutput (Just apiKey)) -> do
        let reqBody =
              AnthropicRequest
                { model = "claude-3-haiku-20240307",
                  messages = [AnthropicMessage "user" prompt],
                  max_tokens = 4096
                }

        let req = newRequest "https://api.anthropic.com/v1/messages"
            req' =
              req
                { method = "POST",
                  headers =
                    [ ("x-api-key", T.unpack apiKey),
                      ("anthropic-version", "2023-06-01"),
                      ("content-type", "application/json")
                    ]
                }

        res <- sendRequest req' (Just $ BSL.toStrict $ encode reqBody)

        case res of
          Response status bodyBytes _headers -> do
            if status /= 200
              then pure $ Left $ "HTTP Error: " <> T.pack (show status) <> " " <> TE.decodeUtf8 bodyBytes
              else case eitherDecodeStrict bodyBytes of
                Left err -> pure $ Left $ "JSON Decode Error: " <> T.pack err
                Right (AnthropicResponse contents) ->
                  case contents of
                    (c : _) -> pure $ Right (text c)
                    [] -> pure $ Left "Empty response from Anthropic"
