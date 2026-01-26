{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module ExoMonad.Gemini.Interpreter
  ( runGeminiIO
  , parseGeminiOutput
  ) where

import Control.Monad.Freer (Eff, interpret, sendM, LastMember)
import Data.Aeson (decodeStrict, Value(Null))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import ExoMonad.Effect.Gemini (GeminiOp(..), GeminiModel(..), GeminiResult(..))

-- | Run Gemini effect via subprocess spawning of 'gemini' CLI.
runGeminiIO :: LastMember IO effs => Eff (GeminiOp ': effs) a -> Eff effs a
runGeminiIO = interpret $ \case
  RunGemini _meta model prompt -> sendM $ do
    let modelStr = geminiModelToCliId model
    
    (exitCode, stdout, stderr) <- readProcessWithExitCode "gemini" 
      [ "--model", modelStr
      , "--prompt", T.unpack prompt
      , "--output", "json"
      ] ""
    
    case exitCode of
      ExitSuccess -> do
        pure $ parseGeminiOutput stdout
      ExitFailure _ -> do
        pure GeminiResult
          { grOutput = Null
          , grRawResponse = "ERROR: " <> T.pack stderr
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