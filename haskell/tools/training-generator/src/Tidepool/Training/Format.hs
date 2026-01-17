module Tidepool.Training.Format
  ( formatSelectSymbolsExample
  , formatDeveloperTurn
  , formatUserTurn
  , formatModelTurnWithHole
  , holeMarker
  ) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T

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
formatUserTurn
  :: Text         -- Symbol name
  -> Text         -- Location (File:Line)
  -> Text         -- Signature
  -> Maybe Text   -- Code snippet (optional)
  -> Maybe Text   -- Documentation (optional)
  -> [Text]       -- Candidates
  -> Text
formatUserTurn symName location signature maybeCode maybeDocs candidates = T.unlines $
  [ "<start_of_turn>user"
  , "Topic: <escape>" <> holeMarker "topic" <> "<escape>"
  , "Symbol: <escape>" <> symName <> "<escape>"
  , "Location: <escape>" <> location <> "<escape>"
  , "Signature: <escape>" <> signature <> "<escape>"
  ]
  <> (case maybeCode of
       Just code -> ["Code: <escape>" <> code <> "<escape>"]
       Nothing -> [])
  <> (case maybeDocs of
       Just docs -> ["Docs: <escape>" <> docs <> "<escape>"]
       Nothing -> [])
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
formatSelectSymbolsExample
  :: Text         -- Symbol name
  -> Text         -- Location
  -> Text         -- Signature
  -> Maybe Text   -- Code snippet
  -> Maybe Text   -- Documentation
  -> [Text]       -- Candidates
  -> ByteString
formatSelectSymbolsExample symName location signature maybeCode maybeDocs candidates =
  let text = T.concat
        [ formatDeveloperTurn
        , formatUserTurn symName location signature maybeCode maybeDocs candidates
        , formatModelTurnWithHole
        ]
  in encode $ object ["text" .= text]
