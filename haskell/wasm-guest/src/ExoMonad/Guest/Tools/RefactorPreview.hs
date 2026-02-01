{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Guest.Tools.RefactorPreview where

import Control.Monad.Freer (Eff, LastMember, Member, runM, sendM)
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.Effects.AstGrep (AstGrep, apply, runAstGrep, scan)
import ExoMonad.Guest.Effects.AstGrep qualified as AG
import ExoMonad.Guest.Effects.LLM (LLMCall, callHaiku, runLLMCall)
import ExoMonad.Guest.Effects.PlanStore (NewRefactorPlan (..), PlanStore, RefactorPlan (..), deletePlan, getPlan, runPlanStore, storePlan)
import ExoMonad.Guest.Effects.PlanStore qualified as PS
import ExoMonad.Guest.HostCall (callHostVoid, host_log_error, host_log_info)
import ExoMonad.Guest.Tool.Class (MCPCallOutput (..), MCPTool (..), errorResult, successResult)
import GHC.Generics (Generic)
import Prelude hiding (id)

-- Tool Types
data RefactorPreview

data EnactRefactor

data SteerRefactor

data DiscardRefactor

-- Input Types
data RefactorPreviewInput = RefactorPreviewInput
  { description :: Text,
    language :: Text,
    scope :: Text,
    max_iterations :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON RefactorPreviewInput

data EnactRefactorInput = EnactRefactorInput
  { plan_id :: Text
  }
  deriving (Show, Generic)

instance FromJSON EnactRefactorInput

data SteerRefactorInput = SteerRefactorInput
  { plan_id :: Text,
    feedback :: Text
  }
  deriving (Show, Generic)

instance FromJSON SteerRefactorInput

data DiscardRefactorInput = DiscardRefactorInput
  { plan_id :: Text
  }
  deriving (Show, Generic)

instance FromJSON DiscardRefactorInput

-- Logging Helper
data LogInput = LogInput
  { message :: Text
  }
  deriving (Generic)

instance ToJSON LogInput

logInfo :: (LastMember IO effs) => Text -> Eff effs ()
logInfo msg = sendM $ callHostVoid host_log_info (LogInput msg)

logError :: (LastMember IO effs) => Text -> Eff effs ()
logError msg = sendM $ callHostVoid host_log_error (LogInput msg)

-- Runner
runEffects :: Eff '[AstGrep, PlanStore, LLMCall, IO] a -> IO a
runEffects = runM . runLLMCall . runPlanStore . runAstGrep

-- Haiku Loop
runHaikuLoop ::
  (Member AstGrep effs, Member LLMCall effs, LastMember IO effs) =>
  Text -> -- Description
  Text -> -- Language
  Text -> -- Scope
  Maybe Text -> -- Optional Feedback (from user or previous error)
  Maybe Text -> -- Optional Previous Rule
  Int -> -- Max Iterations
  Eff effs (Either Text NewRefactorPlan)
runHaikuLoop desc lang scope feedback prevRule maxIter = go 1 feedback prevRule
  where
    go attempt currentFeedback currentPrevRule
      | attempt > maxIter = pure $ Left "Max iterations reached without success."
      | otherwise = do
          logInfo $ "Haiku loop attempt " <> T.pack (show attempt) <> "/" <> T.pack (show maxIter)

          let basePrompt =
                "You are an expert in writing ast-grep rules."
                  <> "Task: "
                  <> desc
                  <> "\n"
                  <> "Language: "
                  <> lang
                  <> "\n"
                  <> "Return ONLY the YAML rule within ```yaml``` block."

          let feedbackPart = case currentFeedback of
                Just f -> "\n\nFeedback/Error from previous attempt:\n" <> f
                Nothing -> ""

          let rulePart = case currentPrevRule of
                Just r -> "\n\nPrevious Rule:\n```yaml\n" <> r <> "\n```"
                Nothing -> ""

          let prompt = basePrompt <> rulePart <> feedbackPart

          res <- callHaiku prompt
          case res of
            Left err -> pure $ Left $ "Haiku call failed: " <> err
            Right response -> do
              let rule = extractYaml response
              -- logInfo $ "Generated rule: " <> rule

              scanRes <- scan rule scope
              case scanRes of
                Left err -> do
                  -- Scan failed (syntax error usually), retry
                  logInfo $ "Scan failed (syntax error?): " <> err
                  go (attempt + 1) (Just $ "The ast-grep rule caused an error: " <> err) (Just rule)
                Right jsonMatches -> do
                  -- Check if we have matches. ast-grep returns "[]" if no matches.
                  if jsonMatches == "[]"
                    then do
                      logInfo "No matches found, retrying..."
                      go (attempt + 1) (Just "The rule returned no matches. Please adjust it to match the code.") (Just rule)
                    else do
                      pure $
                        Right $
                          NewRefactorPlan
                            { rule = rule,
                              scope = scope,
                              language = lang,
                              diff = "Matches found: " <> jsonMatches, -- In real impl we might want to parse this
                              files = [] -- We could parse jsonMatches to get file list
                            }

extractYaml :: Text -> Text
extractYaml text =
  let parts = T.splitOn "```yaml" text
   in case parts of
        (_ : content : _) -> T.strip $ fst $ T.breakOn "```" content
        _ -> text

-- Handlers

handleRefactorPreview ::
  (Member AstGrep effs, Member PlanStore effs, Member LLMCall effs, LastMember IO effs) =>
  RefactorPreviewInput ->
  Eff effs MCPCallOutput
handleRefactorPreview input = do
  let maxIter = fromMaybe 5 input.max_iterations
  res <- runHaikuLoop input.description input.language input.scope Nothing Nothing maxIter
  case res of
    Left err -> pure $ errorResult err
    Right plan -> do
      resStore <- storePlan plan
      case resStore of
        Left err -> pure $ errorResult $ "Failed to store plan: " <> err
        Right planId -> do
          pure $
            successResult $
              object
                [ "plan_id" .= planId,
                  "summary" .= ("Found matches for: " <> input.description),
                  "diff_preview" .= plan.diff
                ]

handleEnactRefactor ::
  (Member AstGrep effs, Member PlanStore effs, LastMember IO effs) =>
  EnactRefactorInput ->
  Eff effs MCPCallOutput
handleEnactRefactor input = do
  res <- getPlan input.plan_id
  case res of
    Left err -> pure $ errorResult $ "Failed to get plan: " <> err
    Right Nothing -> pure $ errorResult "Plan not found or expired"
    Right (Just plan) -> do
      applyRes <- apply plan.rule plan.scope
      case applyRes of
        Left err -> pure $ errorResult $ "Failed to apply refactor: " <> err
        Right () -> do
          _ <- deletePlan input.plan_id
          pure $ successResult $ object ["success" .= True]

handleSteerRefactor ::
  (Member PlanStore effs, Member AstGrep effs, Member LLMCall effs, LastMember IO effs) =>
  SteerRefactorInput ->
  Eff effs MCPCallOutput
handleSteerRefactor input = do
  res <- getPlan input.plan_id
  case res of
    Left err -> pure $ errorResult $ "Failed to get plan: " <> err
    Right Nothing -> pure $ errorResult "Plan not found or expired"
    Right (Just oldPlan) -> do
      -- Iterate starting from the old plan's state
      let maxIter = 5 -- Default for steering
      resLoop <-
        runHaikuLoop
          ("Refine rule: " <> oldPlan.rule) -- Description is now refinement
          oldPlan.language
          oldPlan.scope
          (Just input.feedback)
          (Just oldPlan.rule)
          maxIter

      case resLoop of
        Left err -> pure $ errorResult err
        Right newPlan -> do
          resStore <- storePlan newPlan
          case resStore of
            Left err -> pure $ errorResult $ "Failed to store new plan: " <> err
            Right newPlanId -> do
              -- Optional: delete old plan? Or keep history?
              -- _ <- deletePlan (plan_id input)
              pure $
                successResult $
                  object
                    [ "plan_id" .= newPlanId,
                      "summary" .= ("Refined matches based on feedback: " <> input.feedback),
                      "diff_preview" .= newPlan.diff
                    ]

handleDiscardRefactor ::
  (Member PlanStore effs, LastMember IO effs) =>
  DiscardRefactorInput ->
  Eff effs MCPCallOutput
handleDiscardRefactor input = do
  res <- deletePlan input.plan_id
  case res of
    Left err -> pure $ errorResult $ "Failed to delete plan: " <> err
    Right deleted -> pure $ successResult $ object ["discarded" .= deleted]

-- MCP Tool Instances

instance MCPTool RefactorPreview where
  type ToolArgs RefactorPreview = RefactorPreviewInput
  toolName = "refactor_preview"
  toolDescription = "Generate ast-grep rule and preview diff"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "description" .= object ["type" .= ("string" :: Text), "description" .= ("Description of refactoring" :: Text)],
              "language" .= object ["type" .= ("string" :: Text)],
              "scope" .= object ["type" .= ("string" :: Text), "description" .= ("File scope (e.g. src/)" :: Text)],
              "max_iterations" .= object ["type" .= ("integer" :: Text), "description" .= ("Max Haiku iterations (default 5)" :: Text)]
            ],
        "required" .= (["description", "language", "scope"] :: [Text])
      ]
  toolHandler args = runEffects $ handleRefactorPreview args

instance MCPTool EnactRefactor where
  type ToolArgs EnactRefactor = EnactRefactorInput
  toolName = "enact_refactor"
  toolDescription = "Apply the refactoring plan"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "plan_id" .= object ["type" .= ("string" :: Text), "description" .= ("UUID of the plan" :: Text)]
            ],
        "required" .= (["plan_id"] :: [Text])
      ]
  toolHandler args = runEffects $ handleEnactRefactor args

instance MCPTool SteerRefactor where
  type ToolArgs SteerRefactor = SteerRefactorInput
  toolName = "steer_refactor"
  toolDescription = "Steer the refactoring with feedback"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "plan_id" .= object ["type" .= ("string" :: Text)],
              "feedback" .= object ["type" .= ("string" :: Text)]
            ],
        "required" .= (["plan_id", "feedback"] :: [Text])
      ]
  toolHandler args = runEffects $ handleSteerRefactor args

instance MCPTool DiscardRefactor where
  type ToolArgs DiscardRefactor = DiscardRefactorInput
  toolName = "discard_refactor"
  toolDescription = "Discard the refactoring plan"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "properties"
          .= object
            [ "plan_id" .= object ["type" .= ("string" :: Text)]
            ],
        "required" .= (["plan_id"] :: [Text])
      ]
  toolHandler args = runEffects $ handleDiscardRefactor args
