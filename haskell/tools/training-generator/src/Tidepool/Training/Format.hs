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
  ) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Training.Types (CandidateGroups(..))

-- | Create a hole marker for human annotation.
holeMarker :: Text -> Text
holeMarker fieldName = "<!-- REPLACE: " <> fieldName <> " -->"

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
