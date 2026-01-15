-- | FunctionGemma Turn 1-5 format for training data.
--
-- Formats training examples as JSONL with the specific control tokens
-- required by FunctionGemma:
--
-- - Turn 1 (developer): Tool declaration with \<start_function_declaration\>
-- - Turn 2 (user): Rating request with \<escape\> tokens
-- - Turn 3 (model): Expected output with \<start_function_call\>
module Tidepool.Training.Format
  ( -- * Formatting
    formatTrainingLine
  , formatTurn1
  , formatTurn2
  , formatTurn3

    -- * Constants
  , triggerPhrase
  ) where

import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Training.Types


-- | The exact trigger phrase required by FunctionGemma.
triggerPhrase :: Text
triggerPhrase = "You are a model that can do function calling with the following functions"


-- | Format a complete training example as a JSONL line.
formatTrainingLine :: TrainingExample -> ByteString
formatTrainingLine ex = encode $ object
  [ "messages" .= messages ]
  where
    messages :: [Value]
    messages =
      [ object ["role" .= ("developer" :: Text), "content" .= formatTurn1]
      , object ["role" .= ("user" :: Text), "content" .= formatTurn2 ex.teQuery ex.teNode]
      , object ["role" .= ("model" :: Text), "content" .= formatTurn3 ex.teRubric]
      ]


-- | Turn 1: Tool declaration (developer role).
--
-- Must include the exact trigger phrase and control tokens.
formatTurn1 :: Text
formatTurn1 = T.unlines
  [ triggerPhrase
  , ""
  , "<start_function_declaration>"
  , "rate_node("
  , "  context: string,"
  , "  hover: string,"
  , "  query: string,"
  , "  query_tags: array<enum[" <> tagEnumList <> "]>"
  , "): {"
  , "  relevance: int,"
  , "  risk: int,"
  , "  complexity: int,"
  , "  confidence: int,"
  , "  tags: array<enum[" <> tagEnumList <> "]>"
  , "}"
  , "<end_function_declaration>"
  ]
  where
    tagEnumList = T.intercalate "," (map tagToText allTags)


-- | Turn 2: User request (user role).
--
-- Uses \<escape\> tokens to delimit string content.
formatTurn2 :: QueryContext -> NodeContext -> Text
formatTurn2 query node = T.unlines
  [ "Rate this code location for the query:"
  , "<escape>" <> query.qcQuery <> "<escape>"
  , ""
  , "Location: " <> node.ncLocation
  , "Depth: " <> T.pack (show node.ncDepth)
  , "Breadth: " <> T.pack (show node.ncBreadth)
  , ""
  , "Code:"
  , "<escape>"
  , node.ncCodeSnippet
  , "<escape>"
  , ""
  , "Hover info:"
  , "<escape>" <> node.ncHover <> "<escape>"
  , ""
  , "Query tags: [" <> T.intercalate ", " (map tagToText query.qcTags) <> "]"
  ]


-- | Turn 3: Expected model output (model role).
--
-- Uses FunctionGemma control tokens for function call.
formatTurn3 :: Rubric -> Text
formatTurn3 r = "<start_function_call>call:rate_node{"
  <> "relevance:" <> T.pack (show r.rRelevance)
  <> ",risk:" <> T.pack (show r.rRisk)
  <> ",complexity:" <> T.pack (show r.rComplexity)
  <> ",confidence:" <> T.pack (show r.rConfidence)
  <> ",tags:[" <> T.intercalate "," (map tagToText r.rTags) <> "]"
  <> "}<end_function_call>"
