{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module ExoMonad.Gemini.Interpreter
  ( runGeminiIO
  , runGeminiWithConfig
  , GeminiConfig(..)
  , parseGeminiOutput
  ) where

import Control.Monad.Freer (Eff, interpret, sendM, LastMember)
import Data.Aeson (decodeStrict, Value(Null))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import ExoMonad.Effect.Gemini (GeminiOp(..), GeminiModel(..), GeminiResult(..))
import ExoMonad.Effects.SocketClient
  ( SocketConfig(..)
  , ServiceRequest(..)
  , ServiceResponse(..)
  , sendRequest
  )
import GHC.Generics (Generic)
-- ...
    GeminiSocketConfig path -> sendM $ do
      let modelStr = T.pack $ geminiModelToCliId model
      let req = OllamaGenerate { model = modelStr, prompt = prompt, system = Nothing }
      result <- sendRequest (SocketConfig path 30000) req
      case result of
        Right (OllamaGenerateResponse resp _done) ->
          -- Reuse the same JSON parsing logic as the CLI path
          pure $ parseGeminiOutput (T.unpack resp)
        Right (ErrorResponse _code msg) ->
          pure GeminiResult
            { grOutput = Null
            , grRawResponse = "ERROR: " <> msg
            }
        Right _ ->
          pure GeminiResult
            { grOutput = Null
            , grRawResponse = "ERROR: Unexpected response type"
            }
        Left err ->
          pure GeminiResult
            { grOutput = Null
            , grRawResponse = "ERROR: " <> T.pack (show err)
            }

-- | Map our internal 'GeminiModel' to the corresponding 'gemini' CLI model id.
--
-- These identifiers are specific to the installed version of the Gemini CLI
-- and may change in future releases. When upgrading the 'gemini' CLI, verify
-- that these model ids are still valid and update them if necessary.
--
-- Compatibility: Gemini CLI v2.x
geminiModelToCliId :: GeminiModel -> String
geminiModelToCliId = \case
  Flash -> "flash-2"
  Pro   -> "pro-2"
  Ultra -> "ultra-2"

-- | Parse the output of the gemini CLI.
parseGeminiOutput :: String -> GeminiResult
parseGeminiOutput stdout =
  let outBS = BS.pack stdout
      val = case decodeStrict outBS of
            Just v -> v
            Nothing -> Null
  in GeminiResult
    { grOutput = val
    , grRawResponse = T.pack stdout
    }