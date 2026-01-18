{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Tidepool.Teaching.Execute
  ( executeWithTeaching
  , executeWithTeachingSafe
  , TeachingError(..)
  , buildToolPrompt
  ) where

import Control.Monad.Freer (Eff, Member, LastMember, sendM, runM)
import Data.Aeson (ToJSON, FromJSON, encode, fromJSON, Result(..), Value, toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time (getCurrentTime)

import Tidepool.Graph.Tool (ToolDef(..))
import Tidepool.Tool.Convert (toAnthropicTool, ToAnthropicTool)
import Tidepool.Teaching.Types
  ( RecordingHandles
  , TrainingExample(..)
  , AnthropicApiKey(..)
  )
import Tidepool.Teaching.Teacher (FineTrainingTeacher(..), baseSystemPrompt)
import Tidepool.Teaching.Anthropic (extractTeachingTurn, AnthropicResponse)
import Tidepool.Teaching.Convert (anthropicToFunctionGemma, TeachingTurn(..))
import Tidepool.Teaching.Record (recordExample)
import Tidepool.LLM.Interpreter (runLLMComplete, mkLLMEnv)
import Tidepool.LLM.Types (LLMConfig(..), AnthropicSecrets(..), BaseUrl(..), ApiKey(..))
import Tidepool.Effects.LLMProvider
  ( AnthropicConfig(..)
  , ThinkingBudget(..)
  , complete
  , SProvider(SAnthropic)
  )

-- | Teaching mode errors
data TeachingError
  = HaikuAPIError Text
    -- ^ Haiku API call failed
  | ParseError Text
    -- ^ Failed to parse Anthropic response
  | ToolOutputError Text
    -- ^ Failed to parse tool output
  deriving (Show, Eq)

-- | Execute tool with optional teaching mode
--
-- This is the main integration point for teaching infrastructure.
-- It:
-- 1. Builds user prompt from tool input
-- 2. Gets teacher guidance for effect
-- 3. Calls Haiku with tools (with extended thinking)
-- 4. Extracts teaching turn (reasoning + tool use)
-- 5. Converts to FunctionGemma format
-- 6. Records both formats
-- 7. Parses tool output and continues execution
executeWithTeaching
  :: forall tool effect es.
     ( ToolDef tool
     , FineTrainingTeacher effect
     , ToAnthropicTool tool
     , ToJSON (ToolInput tool)
     , FromJSON (ToolOutput tool)
     , LastMember IO (ToolEffects tool)
     )
  => AnthropicApiKey
  -> RecordingHandles
  -> tool
  -> ToolInput tool
  -> Eff (ToolEffects tool) (ToolOutput tool)
executeWithTeaching apiKey handles tool input = do
      -- 1. Build user prompt from input
      let userPrompt = buildToolPrompt tool input

      -- 2. Get teacher guidance
      let guidance = teacherGuidance @effect
      let systemPrompt = baseSystemPrompt <> "\n\n" <> guidance

      -- 3. Call Haiku with tools (with extended thinking for better training data)
      let cfg = AnthropicConfig
            { acModel = "claude-3-5-haiku-20241022"
            , acMaxTokens = 4096
            , acThinking = ThinkingEnabled 1024  -- Enable thinking for quality reasoning
            , acSystemPrompt = Just systemPrompt
            }
      let tools = [toJSON $ toAnthropicTool tool]  -- Convert to Value for LLM effect

      anthropicResp <- sendM $ do
        let llmConfig = LLMConfig
              { lcAnthropicSecrets = Just $ AnthropicSecrets
                  { asApiKey = ApiKey (unAnthropicApiKey apiKey)
                  , asBaseUrl = BaseUrl "https://api.anthropic.com"
                  }
              , lcOpenAISecrets = Nothing
              }
        env <- mkLLMEnv llmConfig
        runM $ runLLMComplete env $ complete SAnthropic cfg userPrompt (Just tools)

      -- 4. Extract teaching turn
      case extractTeachingTurn anthropicResp of
        Left parseErr -> error $ "Parse failed: " <> T.unpack parseErr
        Right (reasoning, toolName, toolArgs) -> do
          -- 5. Convert to FunctionGemma format
          let turn = TeachingTurn
                { ttReasoning = reasoning
                , ttToolName = toolName
                , ttToolArgs = toolArgs
                }
          let gemmaLine = anthropicToFunctionGemma userPrompt turn

          -- 6. Record both formats
          now <- sendM getCurrentTime
          let example = TrainingExample
                { teAnthropicRaw = toJSON anthropicResp  -- Serialize response to Value
                , teFunctionGemmaFormatted = gemmaLine
                , teTeacherGuidance = Just guidance
                , teTimestamp = now
                , teToolName = toolName
                }
          sendM $ recordExample handles example

          -- 7. Parse tool output and continue execution
          case fromJSON toolArgs of
            Error err -> error $ "Tool output parse failed: " <> err
            Success output -> pure output

-- | Execute tool with optional teaching (safe version)
--
-- Same as 'executeWithTeaching' but returns Either instead of throwing errors.
executeWithTeachingSafe
  :: forall tool effect es.
     ( ToolDef tool
     , FineTrainingTeacher effect
     , ToAnthropicTool tool
     , ToJSON (ToolInput tool)
     , FromJSON (ToolOutput tool)
     , LastMember IO (ToolEffects tool)
     )
  => AnthropicApiKey
  -> RecordingHandles
  -> tool
  -> ToolInput tool
  -> Eff (ToolEffects tool) (Either TeachingError (ToolOutput tool))
executeWithTeachingSafe apiKey handles tool input = do
      let userPrompt = buildToolPrompt tool input
      let guidance = teacherGuidance @effect
      let systemPrompt = baseSystemPrompt <> "\n\n" <> guidance

      let cfg = AnthropicConfig
            { acModel = "claude-3-5-haiku-20241022"
            , acMaxTokens = 4096
            , acThinking = ThinkingEnabled 1024  -- Enable thinking for quality reasoning
            , acSystemPrompt = Just systemPrompt
            }
      let tools = [toJSON $ toAnthropicTool tool]  -- Convert to Value for LLM effect

      anthropicResp <- sendM $ do
        let llmConfig = LLMConfig
              { lcAnthropicSecrets = Just $ AnthropicSecrets
                  { asApiKey = ApiKey (unAnthropicApiKey apiKey)
                  , asBaseUrl = BaseUrl "https://api.anthropic.com"
                  }
              , lcOpenAISecrets = Nothing
              }
        env <- mkLLMEnv llmConfig
        runM $ runLLMComplete env $ complete SAnthropic cfg userPrompt (Just tools)

      case extractTeachingTurn anthropicResp of
        Left parseErr -> pure $ Left $ ParseError parseErr
        Right (reasoning, toolName, toolArgs) -> do
          let turn = TeachingTurn
                { ttReasoning = reasoning
                , ttToolName = toolName
                , ttToolArgs = toolArgs
                }
          let gemmaLine = anthropicToFunctionGemma userPrompt turn

          now <- sendM getCurrentTime
          let example = TrainingExample
                { teAnthropicRaw = toJSON anthropicResp  -- Serialize response to Value
                , teFunctionGemmaFormatted = gemmaLine
                , teTeacherGuidance = Just guidance
                , teTimestamp = now
                , teToolName = toolName
                }
          sendM $ recordExample handles example

          case fromJSON toolArgs of
            Error err -> pure $ Left $ ToolOutputError $ T.pack err
            Success output -> pure $ Right output

-- | Build user prompt from tool input
--
-- Serializes the input as JSON and wraps it with tool metadata.
-- This gives Haiku context about what tool it should invoke and with what parameters.
buildToolPrompt
  :: forall tool. (ToolDef tool, ToJSON (ToolInput tool))
  => tool
  -> ToolInput tool
  -> Text
buildToolPrompt tool input =
  let inputJson = encode input
      toolDesc = toolDescription tool
  in T.unlines
    [ "Tool: " <> toolName tool
    , "Description: " <> toolDesc
    , ""
    , "Input:"
    , TL.toStrict $ TL.decodeUtf8 inputJson
    ]
