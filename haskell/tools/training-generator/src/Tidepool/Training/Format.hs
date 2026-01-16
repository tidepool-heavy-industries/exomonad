-- | FunctionGemma 2-turn minimal format for training data.
--
-- Formats training examples as JSONL with the minimal control tokens
-- required by fine-tuned FunctionGemma 270M:
--
-- - Turn 1 (user): Edge context with \<escape\> tokens
-- - Turn 2 (model): Function call with the rubric
--
-- No schema turn (baked into weights). No response/synthesis turns.
-- Training format MUST match inference format exactly.
--
-- Uses Gemma turn structure: \<start_of_turn\>role\\n...\<end_of_turn\>
module Tidepool.Training.Format
  ( -- * Edge Training (2-turn minimal)
    formatEdgeTrainingLine
  , formatEdgeTrainingText
  , formatUserTurn
  , formatModelTurn

    -- * Helpers
  , escapeText
  ) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Training.Types


-- | Wrap text in \<escape\> tokens for FunctionGemma wire format.
--
-- CRITICAL: All string values in structured data must use this.
-- Failure to escape will cause parsing failures during training.
-- Haskell code contains brackets and commas that collide with JSON syntax.
escapeText :: Text -> Text
escapeText t = "<escape>" <> t <> "<escape>"


-- | Format an edge training example as a JSONL line.
--
-- Uses the "text" format preferred by TRL/Unsloth for direct fine-tuning.
-- Output: {"text": "<start_of_turn>user\n...<end_of_turn>\n<start_of_turn>model\n...<end_of_turn>"}
formatEdgeTrainingLine :: EdgeTrainingExample -> ByteString
formatEdgeTrainingLine ex = encode $ object
  [ "text" .= formatEdgeTrainingText ex ]


-- | Format the complete 2-turn conversation as raw text.
formatEdgeTrainingText :: EdgeTrainingExample -> Text
formatEdgeTrainingText ex = T.concat
  [ formatUserTurn ex.eteInput
  , "\n"
  , formatModelTurn ex.eteOutput
  ]


-- | Turn 1 (user): Edge context to be scored.
--
-- All string values wrapped in \<escape\> tokens.
formatUserTurn :: ScoreEdgeInput -> Text
formatUserTurn input = T.concat
  [ "<start_of_turn>user\n"
  , "Score this edge:\n"
  , "Query: ", escapeText input.seiQuery, "\n"
  , "Source: ", escapeText (input.seiSourceFile <> ":" <> T.pack (show input.seiSourceLine)), "\n"
  , "Source hover: ", escapeText input.seiSourceHover, "\n"
  , "Target: ", escapeText (input.seiTargetFile <> ":" <> T.pack (show input.seiTargetLine)), "\n"
  , "Target hover: ", escapeText input.seiTargetHover, "\n"
  , "Edge type: ", escapeText (edgeTypeToText input.seiEdgeType), "\n"
  , "<end_of_turn>"
  ]


-- | Turn 2 (model): Function call with semantic boolean answers.
--
-- This is the output we're training the model to produce.
-- Inference should stop at \<end_of_turn\> or \<end_function_call\>.
formatModelTurn :: ScoreEdgeOutput -> Text
formatModelTurn output = T.concat
  [ "<start_of_turn>model\n"
  , "<start_function_call>call:score_edge{"
  , "is_query_relevant:", boolToText output.seoIsQueryRelevant, ","
  , "is_breaking_boundary:", boolToText output.seoIsBreakingBoundary, ","
  , "is_stable_anchor:", boolToText output.seoIsStableAnchor, ","
  , "is_public_contract:", boolToText output.seoIsPublicContract, ","
  , "reasoning:", escapeText output.seoReasoning
  , "}<end_function_call>\n"
  , "<end_of_turn>"
  ]
  where
    boolToText True = "true"
    boolToText False = "false"
