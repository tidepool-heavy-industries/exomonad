{-# LANGUAGE RecordWildCards #-}

module Tidepool.Training.Format
  ( formatSelectSymbolsExample
  , formatSelectSymbolsExampleGrouped
  , formatDeveloperTurn
  , formatUserTurn
  , formatUserTurnGrouped
  , formatCandidateGroups
  , formatModelTurnWithHole
  , holeMarker
  -- Edge Scoring format
  , formatEdgeTrainingExample
  -- V3: Code-Native Training Format
  , formatCodeExample
  , formatGemmaConversation
  , formatTrainingFromSkeleton
  ) where

import Data.Aeson (encode, object, (.=), Value(..), (.:))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Training.Types (CandidateGroups(..), ScoreEdgeInput(..), ScoreEdgeOutput(..), EdgeTrainingExample(..), edgeTypeToText)

-- | Create a hole marker for human annotation.
holeMarker :: Text -> Text
holeMarker fieldName = "<!-- REPLACE: " <> fieldName <> " -->"

-- | Escape a string for FunctionGemma 270M format.
escape :: Text -> Text
escape t = "<escape>" <> t <> "<escape>"

-- | Format an edge training example as a JSONL conversation.
formatEdgeTrainingExample :: EdgeTrainingExample -> ByteString
formatEdgeTrainingExample EdgeTrainingExample{eteInput=ScoreEdgeInput{..}, eteOutput=ScoreEdgeOutput{..}} =
  let
    userTurn = T.unlines
      [ "<start_of_turn>user"
      , "Score this edge:"
      , "Query: " <> escape seiQuery
      , "Source: " <> escape (seiSourceFile <> ":" <> T.pack (show seiSourceLine))
      , "Source hover: " <> escape seiSourceHover
      , "Target: " <> escape (seiTargetFile <> ":" <> T.pack (show seiTargetLine))
      , "Target hover: " <> escape seiTargetHover
      , "Edge type: " <> escape (edgeTypeToText seiEdgeType)
      , "<end_of_turn>"
      ]
    
    modelTurn = T.concat
      [ "<start_of_turn>model\n"
      , "<start_function_call>"
      , "call:score_edge{"
      , "relevance:" <> T.pack (show seoRelevance) <> ","
      , "risk:" <> T.pack (show seoRisk) <> ","
      , "reasoning:" <> escape seoReasoning <> ","
      , "is_exhaustive:" <> T.toLower (T.pack (show seoIsExhaustive)) <> ","
      , "is_type_family:" <> T.toLower (T.pack (show seoIsTypeFamily)) <> ","
      , "is_exported:" <> T.toLower (T.pack (show seoIsExported))
      , "}"
      , "<end_function_call>\n"
      , "<end_of_turn>"
      ]
      
    text = userTurn <> modelTurn
  in encode $ object ["text" .= text]

-- | Format developer turn (fixed for all examples).
formatDeveloperTurn :: Text
formatDeveloperTurn = T.unlines
  [ "<start_of_turn>developer"
  , "You are an expert function calling AI assistant."
  , "You have access to the following functions:"
  , ""
  , "<start_function_declaration>select_symbols"
  , "Select relevant symbols from candidates that help understand the topic in context of the current symbol."
  , "Parameters:"
  , "  selected (array): Array of selected symbol names from the candidate list. (required)"
  , "<end_function_declaration>"
  , ""
  , "<end_of_turn>"
  ]

-- | Format user turn with LSP context.
--
-- Lean format optimized for token density:
-- - No escape tags in user turn (plain text context)
-- - Module + Package for disambiguation
-- - First-sentence docs only
-- - Signature pre-cleaned (no forall, no markdown)
formatUserTurn
  :: Text         -- Symbol name
  -> Text         -- Module name (e.g., "Tidepool.Effect.LSP")
  -> Text         -- Package name (e.g., "tidepool-core")
  -> Text         -- Signature (cleaned)
  -> Maybe Text   -- Documentation (first sentence only)
  -> [Text]       -- Candidates
  -> Text
formatUserTurn symName moduleName packageName signature maybeDocs candidates = T.unlines $
  [ "<start_of_turn>user"
  , "Topic: " <> holeMarker "topic"
  , "Symbol: " <> symName
  , "Module: " <> moduleName
  , "Package: " <> packageName
  , "Signature: " <> signature
  ]
  <> maybe [] (\docs -> ["Docs: " <> docs]) maybeDocs
  <>
  [ "Candidates: " <> T.intercalate ", " candidates
  , "<end_of_turn>"
  ]

-- | Format model turn with hole for selected symbols.
formatModelTurnWithHole :: Text
formatModelTurnWithHole = T.unlines
  [ "<start_of_turn>model"
  , "<start_function_call>"
  , "call:select_symbols{selected:<escape>" <> holeMarker "selected" <> "<escape>}"
  , "<end_function_call>"
  , "<end_of_turn>"
  ]

-- | Format complete training example as JSONL line.
--
-- Lean format: Module + Package + cleaned signature + first-sentence docs.
formatSelectSymbolsExample
  :: Text         -- Symbol name
  -> Text         -- Module name
  -> Text         -- Package name
  -> Text         -- Signature (cleaned)
  -> Maybe Text   -- Documentation (first sentence)
  -> [Text]       -- Candidates
  -> ByteString
formatSelectSymbolsExample symName moduleName packageName signature maybeDocs candidates =
  let text = T.concat
        [ formatDeveloperTurn
        , formatUserTurn symName moduleName packageName signature maybeDocs candidates
        , formatModelTurnWithHole
        ]
  in encode $ object ["text" .= text]


-- | Format candidate groups (Fields, Inputs, Output, References).
formatCandidateGroups :: CandidateGroups -> Text
formatCandidateGroups CandidateGroups{..} = T.unlines
  [ "Candidates:"
  , "  Fields: " <> formatList cgFields
  , "  Inputs: " <> formatList cgInputs
  , "  Output: " <> formatList cgOutput
  , "  References: " <> case cgReferences of
      Left note -> note
      Right refs -> formatList refs
  ]
  where
    formatList [] = "(none)"
    formatList xs = T.intercalate ", " xs


-- | Format user turn with grouped candidates (v2 format).
formatUserTurnGrouped
  :: Text            -- Symbol name
  -> Text            -- Module name
  -> Text            -- Package name
  -> Text            -- Signature (cleaned)
  -> CandidateGroups -- Grouped candidates
  -> Text
formatUserTurnGrouped symName moduleName packageName signature groups = T.unlines
  [ "<start_of_turn>user"
  , "Topic: " <> holeMarker "topic"
  , "Symbol: " <> symName
  , "Module: " <> moduleName
  , "Package: " <> packageName
  , "Signature: " <> signature
  , ""
  , T.strip $ formatCandidateGroups groups
  , "<end_of_turn>"
  ]


-- | Format complete training example with grouped candidates (v2).
formatSelectSymbolsExampleGrouped
  :: Text            -- Symbol name
  -> Text            -- Module name
  -> Text            -- Package name
  -> Text            -- Signature (cleaned)
  -> CandidateGroups -- Grouped candidates
  -> ByteString
formatSelectSymbolsExampleGrouped symName moduleName packageName signature groups =
  let text = T.concat
        [ formatDeveloperTurn
        , formatUserTurnGrouped symName moduleName packageName signature groups
        , formatModelTurnWithHole
        ]
  in encode $ object ["text" .= text]


-- ════════════════════════════════════════════════════════════════════════════
-- V3: CODE-NATIVE TRAINING FORMAT
-- ════════════════════════════════════════════════════════════════════════════

-- | Format code-based training example (v3).
--
-- Feeds raw code to FunctionGemma with semantic criteria, not topics.
-- Model extracts symbols itself from code structure.
formatCodeExample :: Text -> Text -> [Text] -> Text
formatCodeExample criteria code selectedSymbols =
  let userTurn = T.unlines
        [ "Criteria: " <> criteria
        , ""
        , "Code:"
        , "```haskell"
        , code
        , "```"
        , ""
        , "Extract symbols:"
        ]
      modelTurn = if null selectedSymbols
        then "<start_function_call>\ncall:select_symbols{selected:<escape><escape>}\n<end_function_call>"
        else let symbols = T.intercalate "," selectedSymbols
             in "<start_function_call>\ncall:select_symbols{selected:<escape>" <> symbols <> "<escape>}\n<end_function_call>"
  in formatGemmaConversation userTurn modelTurn

-- | Format full Gemma conversation (developer + user + model turns).
--
-- Standard FunctionGemma 3-turn format:
-- 1. Developer turn: System prompt
-- 2. User turn: Query + code context
-- 3. Model turn: Function call with selected symbols
formatGemmaConversation :: Text -> Text -> Text
formatGemmaConversation userTurn modelTurn = T.unlines
  [ "<start_of_turn>developer"
  , "You are an expert code analysis assistant."
  , "<end_of_turn>"
  , "<start_of_turn>user"
  , userTurn
  , "<end_of_turn>"
  , "<start_of_turn>model"
  , modelTurn
  , "<end_of_turn>"
  ]

-- | Convert annotated skeleton to training JSONL.
--
-- Expects JSON with fields:
-- - "code": Text (Haskell code body)
-- - "criteria": Text (semantic criteria, not TODO)
-- - "selected": [Text] (symbol names, or [] for negative)
--
-- Returns: {"text": formatted_conversation}
formatTrainingFromSkeleton :: Value -> Either Text Value
formatTrainingFromSkeleton skeleton = case skeleton of
  Object obj -> do
    -- Extract fields from skeleton
    code <- case parseMaybe (.: "code") obj of
      Just c -> Right c
      Nothing -> Left "Missing 'code' field in skeleton"

    criteria <- case parseMaybe (.: "criteria") obj of
      Just c -> Right c
      Nothing -> Left "Missing 'criteria' field in skeleton"

    -- Check for unannotated skeleton
    when (criteria == "TODO: add criteria") $
      Left "Skeleton not annotated (criteria still TODO)"

    selected <- case parseMaybe (.: "selected") obj of
      Just s -> Right s
      Nothing -> Left "Missing 'selected' field in skeleton"

    -- Format as training example
    let formatted = formatCodeExample criteria code selected
    Right $ object ["text" .= formatted]

  _ -> Left "Skeleton must be a JSON object"
  where
    when :: Bool -> Either a () -> Either a ()
    when True err = err
    when False _ = Right ()
