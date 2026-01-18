{-# LANGUAGE RecordWildCards #-}

module Tidepool.Teaching.Convert
  ( TeachingTurn(..)
  , extractTeachingTurn
  , anthropicToFunctionGemma
  , formatFunctionCall
  , formatGemmaConversation
  , wrapAsJSONL
  ) where

import Data.Aeson (Value(..), Object, encode, object, (.=), (.:))
import Data.Aeson.Key (fromText, toText)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as V

-- | Teaching turn extracted from Anthropic Messages API response.
data TeachingTurn = TeachingTurn
  { ttReasoning :: Text      -- Combined text blocks
  , ttToolName :: Text       -- Tool invoked
  , ttToolArgs :: Value      -- Tool arguments
  } deriving (Show, Eq)

-- | Extract teaching turn from Anthropic Messages API response.
--
-- Expects response JSON with a "content" array containing:
-- - Text blocks (type: "text") - concatenated into reasoning
-- - Tool use block (type: "tool_use") - extracted for tool name and args
extractTeachingTurn :: Value -> Either Text TeachingTurn
extractTeachingTurn val = case val of
  Object obj -> do
    contentArray <- parseField obj "content" "Missing 'content' field in response"
    case contentArray of
      Array arr -> do
        let contentBlocks = V.toList arr

        -- Extract all text blocks
        let textBlocks = [txt | Object blk <- contentBlocks
                              , Just "text" <- [parseMaybe (.: "type") blk :: Maybe Text]
                              , Just txt <- [parseMaybe (.: "text") blk :: Maybe Text]]

        -- Find tool use block
        toolBlock <- case [blk | Object blk <- contentBlocks
                                , Just "tool_use" <- [parseMaybe (.: "type") blk :: Maybe Text]] of
          [toolObj] -> Right toolObj
          [] -> Left "No tool_use block found in content"
          _ -> Left "Multiple tool_use blocks found (expected exactly one)"

        -- Extract tool name and args
        toolName <- case parseMaybe (.: "name") toolBlock of
          Just name -> Right name
          Nothing -> Left "Missing 'name' in tool_use block"

        toolArgs <- case parseMaybe (.: "input") toolBlock of
          Just args -> Right args
          Nothing -> Left "Missing 'input' in tool_use block"

        -- Combine text blocks with newlines
        let reasoning = T.intercalate "\n" textBlocks

        Right TeachingTurn
          { ttReasoning = reasoning
          , ttToolName = toolName
          , ttToolArgs = toolArgs
          }

      _ -> Left "content field must be an array"

  _ -> Left "Response must be a JSON object"
  where
    parseField :: Object -> Text -> Text -> Either Text Value
    parseField obj key errMsg = case KM.lookup (fromText key) obj of
      Just v -> Right v
      Nothing -> Left errMsg

-- | Format complete Anthropic response as FunctionGemma JSONL line.
--
-- Returns a JSONL line (single JSON object with "text" field).
anthropicToFunctionGemma
  :: Text          -- User turn (context)
  -> TeachingTurn  -- Extracted from Anthropic
  -> Text          -- Complete JSONL line
anthropicToFunctionGemma userTurn turn =
  let conversation = formatGemmaConversation userTurn turn
  in wrapAsJSONL conversation

-- | Format as FunctionGemma 3-turn conversation.
--
-- Structure:
-- 1. Developer turn - System prompt with tool declaration
-- 2. User turn - Query/context
-- 3. Model turn - Reasoning (as inline comment) + function call
formatGemmaConversation :: Text -> TeachingTurn -> Text
formatGemmaConversation userTurn TeachingTurn{..} = T.unlines
  [ "<start_of_turn>developer"
  , "You are a semantic code analysis assistant."
  , "You have access to the following functions:"
  , ""
  , "<start_function_declaration>" <> ttToolName
  , "Select relevant symbols from candidates."
  , "Parameters:"
  , "  selected (array): Array of selected symbols. (required)"
  , "<end_function_declaration>"
  , ""
  , "<end_of_turn>"
  , "<start_of_turn>user"
  , userTurn
  , "<end_of_turn>"
  , "<start_of_turn>model"
  , if T.null ttReasoning
      then ""
      else "/* " <> ttReasoning <> " */"
  , "<start_function_call>"
  , "call:" <> formatFunctionCall ttToolName ttToolArgs
  , "<end_function_call>"
  , "<end_of_turn>"
  ]

-- | Format function call with tool name and arguments.
--
-- Example: select_symbols{selected:<escape>Foo,Bar<escape>}
formatFunctionCall :: Text -> Value -> Text
formatFunctionCall toolName args = case args of
  Object obj -> toolName <> "{" <> formatObjectArgs obj <> "}"
  _ -> error "Tool args must be object"

-- | Format object arguments as key:value pairs.
formatObjectArgs :: Object -> Text
formatObjectArgs obj = T.intercalate "," $ map formatField $ KM.toList obj
  where
    formatField (k, v) = toText k <> ":<escape>" <> formatValue v <> "<escape>"

-- | Format JSON value as text for FunctionGemma.
formatValue :: Value -> Text
formatValue (String s) = s
formatValue (Array arr) = T.intercalate "," (map formatValue $ V.toList arr)
formatValue (Number n) = T.pack $ show n
formatValue (Bool b) = if b then "true" else "false"
formatValue Null = "null"
formatValue (Object _) = error "Nested objects not supported"

-- | Wrap conversation text as JSONL (single JSON object per line).
wrapAsJSONL :: Text -> Text
wrapAsJSONL conversation =
  let encoded = encode $ object ["text" .= conversation]
  in decodeUtf8 $ BL.toStrict encoded
