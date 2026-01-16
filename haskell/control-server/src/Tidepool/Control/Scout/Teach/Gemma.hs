{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- | Gemma effect for teaching symbol classification.
--
-- Uses FunctionGemma to classify symbols for teaching:
--   - Role: What role does this symbol play in understanding the topic?
--   - IsPrereq: Must understand this BEFORE the topic?
--   - Mentions: What other symbols are needed to explain this one?
--
-- Architecture:
--
-- @
-- ┌─────────────────────────────────────────┐
-- │  Exploration Loop                        │
-- │    │                                     │
-- │    ▼                                     │
-- │  classifySymbol :: TeachQuery            │
-- │                 -> LSPSymbol             │
-- │                 -> TeachGemma TeachOutput│
-- │    │                                     │
-- └────┼─────────────────────────────────────┘
--      │
--      ▼
-- ┌─────────────────────────────────────────┐
-- │  Interpreter (pluggable)                 │
-- │                                         │
-- │  • runTeachGemmaHTTP - Ollama server    │
-- │  • runTeachGemmaMock - hardcoded output │
-- └─────────────────────────────────────────┘
-- @
module Tidepool.Control.Scout.Teach.Gemma
  ( -- * Effect
    TeachGemma(..)

    -- * Smart Constructors
  , classifySymbol

    -- * Interpreters
  , runTeachGemmaHTTP
  , runTeachGemmaMock

    -- * Tool Schema
  , classifySymbolTool
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (Eff, Member, send, interpret, LastMember, sendM)
import Data.Aeson (encode, object, (.=), Value, (.:))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.HTTP.Simple

import Tidepool.Control.Scout.Teach.Types
  ( TeachOutput(..), SymbolRole(..), LSPSymbol(..), TeachQuery(..)
  )


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | TeachGemma effect for symbol classification.
--
-- Given a topic query and a symbol with its LSP data, returns:
--   - Role: Is this core, implementation, interface, helper, or unrelated?
--   - IsPrereq: Must understand this before the topic?
--   - Mentions: What symbols does this one reference?
data TeachGemma a where
  -- | Classify a symbol for teaching purposes.
  ClassifySymbol :: TeachQuery -> LSPSymbol -> TeachGemma TeachOutput


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Classify a symbol using the TeachGemma model.
classifySymbol
  :: Member TeachGemma effs
  => TeachQuery -> LSPSymbol -> Eff effs TeachOutput
classifySymbol query sym = send (ClassifySymbol query sym)


-- ════════════════════════════════════════════════════════════════════════════
-- HTTP INTERPRETER (Production)
-- ════════════════════════════════════════════════════════════════════════════

-- | HTTP interpreter that calls Ollama for symbol classification.
--
-- Uses Ollama's /api/chat endpoint with native tool support.
runTeachGemmaHTTP
  :: LastMember IO effs
  => Text  -- ^ Ollama endpoint (e.g., "http://localhost:11434")
  -> Eff (TeachGemma ': effs) a
  -> Eff effs a
runTeachGemmaHTTP endpoint = interpret $ \case
  ClassifySymbol query sym -> do
    sendM $ putStrLn $ "[TeachGemma] HTTP call to " <> T.unpack endpoint
      <> " for: " <> T.unpack (lsName sym)
    result <- sendM $ callOllamaTeach endpoint query sym
    case result of
      Right output -> do
        sendM $ putStrLn $ "[TeachGemma] -> role=" <> show (toRole output)
          <> ", prereq=" <> show (toIsPrereq output)
          <> ", mentions=" <> show (length (toMentions output))
        pure output
      Left err -> error $ unlines
        [ "TeachGemma HTTP error: " <> T.unpack err
        , "  Endpoint: " <> T.unpack endpoint
        , "  Symbol: " <> T.unpack (lsName sym)
        ]


-- | Call Ollama's /api/chat endpoint for symbol classification.
callOllamaTeach :: Text -> TeachQuery -> LSPSymbol -> IO (Either Text TeachOutput)
callOllamaTeach endpoint query sym = do
  let userContent = formatSymbolForClassification query sym
  putStrLn $ "[TeachGemma] Input prompt:\n" <> T.unpack userContent

  result <- try $ do
    let reqBody = encode $ object
          [ "model" .= ("functiongemma:270m" :: Text)
          , "messages" .= [object
              [ "role" .= ("user" :: Text)
              , "content" .= userContent
              ]]
          , "tools" .= [classifySymbolTool]
          , "stream" .= False
          ]

    let url = T.unpack endpoint <> "/api/chat"
    request <- parseRequest url
    let request' = setRequestMethod "POST"
                 $ setRequestHeader "Content-Type" ["application/json"]
                 $ setRequestBodyLBS reqBody
                   request

    response <- httpLBS request'
    let body = getResponseBody response
    putStrLn $ "[TeachGemma] Raw response: " <> take 1000 (show body)
    pure $ parseTeachResponse body

  case result of
    Left (e :: SomeException) -> pure $ Left $ T.pack $ show e
    Right parsed -> pure parsed


-- | Format symbol context for classification.
formatSymbolForClassification :: TeachQuery -> LSPSymbol -> Text
formatSymbolForClassification query sym = T.unlines
  [ "You are helping someone understand: \"" <> tqTopic query <> "\""
  , ""
  , "Symbol: " <> lsName sym
  , "Kind: " <> T.pack (show (lsKind sym))
  , "Signature:"
  , "```haskell"
  , lsSignature sym
  , "```"
  , case lsDocComment sym of
      Just doc -> "Documentation: " <> doc
      Nothing  -> "Documentation: (none)"
  , ""
  , "Answer these questions:"
  , ""
  , "1. ROLE: How does this symbol relate to understanding \"" <> tqTopic query <> "\"?"
  , "   - 'core': This IS the main topic"
  , "   - 'implementation': This is HOW the topic works internally"
  , "   - 'interface': This is how the topic CONNECTS to other code"
  , "   - 'helper': This is a utility used by the topic"
  , "   - 'unrelated': This doesn't help understand the topic"
  , ""
  , "2. IS_PREREQUISITE: Must someone understand this symbol BEFORE they can understand the topic? (true/false)"
  , "   - true: This is foundational - you need it first"
  , "   - false: This can be learned after or alongside the topic"
  , ""
  , "3. MENTIONS: What other symbols are directly referenced by this symbol that would also need explanation?"
  , "   List only the symbol NAMES (e.g., ['ScoreConfig', 'EdgeContext', 'Rubric'])"
  , "   Look at the type signature and documentation for referenced types/functions."
  , ""
  , "Call classify_symbol with your answers."
  ]


-- | Tool schema for classify_symbol function.
classifySymbolTool :: Value
classifySymbolTool = object
  [ "type" .= ("function" :: Text)
  , "function" .= object
      [ "name" .= ("classify_symbol" :: Text)
      , "description" .= ("Classify a symbol for teaching purposes" :: Text)
      , "parameters" .= object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "role" .= object
                  [ "type" .= ("string" :: Text)
                  , "enum" .= (["core", "implementation", "interface", "helper", "unrelated"] :: [Text])
                  , "description" .= ("How this symbol relates to the topic" :: Text)
                  ]
              , "is_prerequisite" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("Must understand this before the topic?" :: Text)
                  ]
              , "mentions" .= object
                  [ "type" .= ("array" :: Text)
                  , "items" .= object
                      [ "type" .= ("string" :: Text)
                      ]
                  , "description" .= ("Other symbols needed to explain this one" :: Text)
                  ]
              ]
          , "required" .= (["role", "is_prerequisite", "mentions"] :: [Text])
          ]
      ]
  ]


-- | Parse Ollama response and extract TeachOutput from tool_calls.
--
-- Strict parsing - fails if required fields are missing.
parseTeachResponse :: ByteString -> Either Text TeachOutput
parseTeachResponse body = do
  json <- case Aeson.decode body of
    Nothing -> Left "Failed to parse JSON response"
    Just v  -> Right (v :: Value)

  -- Extract message.tool_calls[0].function.arguments
  args <- extractToolCallArgs json

  case args of
    Aeson.Object obj -> do
      role <- case parseMaybe (.: "role") obj of
        Just (r :: Text) -> case T.toLower r of
          "core"           -> Right CoreConcept
          "implementation" -> Right Implementation
          "interface"      -> Right Interface
          "helper"         -> Right Helper
          "unrelated"      -> Right Unrelated
          other            -> Left $ "Unknown role: " <> other
        Nothing -> Left "Missing 'role' field"

      isPrereq <- case parseMaybe (.: "is_prerequisite") obj of
        Just (b :: Bool) -> Right b
        Nothing -> Left "Missing 'is_prerequisite' field"

      mentions <- case parseMaybe (.: "mentions") obj of
        Just (arr :: [Text]) -> Right arr
        Nothing -> Left "Missing 'mentions' field"

      Right TeachOutput
        { toRole = role
        , toIsPrereq = isPrereq
        , toMentions = mentions
        }
    _ -> Left "Arguments is not a JSON object"


-- | Extract tool call arguments from Ollama response.
extractToolCallArgs :: Value -> Either Text Value
extractToolCallArgs json = case json of
  Aeson.Object obj ->
    case parseMaybe (.: "message") obj of
      Just (Aeson.Object msg) ->
        case parseMaybe (.: "tool_calls") msg of
          Just (Aeson.Array calls) | not (V.null calls) ->
            case V.head calls of
              Aeson.Object call ->
                case parseMaybe (.: "function") call of
                  Just (Aeson.Object fn) ->
                    case parseMaybe (.: "arguments") fn of
                      Just args -> Right args
                      _ -> Left "No arguments in function"
                  _ -> Left "No function in tool_call"
              _ -> Left "Invalid tool_call format"
          _ -> Left "No tool_calls in message"
      _ -> Left "No message in response"
  _ -> Left "Response is not an object"


-- ════════════════════════════════════════════════════════════════════════════
-- MOCK INTERPRETER (Testing)
-- ════════════════════════════════════════════════════════════════════════════

-- | Mock interpreter that returns a reasonable default.
--
-- Useful for testing the exploration loop without Gemma dependency.
runTeachGemmaMock :: Eff (TeachGemma ': effs) a -> Eff effs a
runTeachGemmaMock = interpret $ \case
  ClassifySymbol _query sym -> pure TeachOutput
    { toRole = Implementation  -- Assume most things are implementation details
    , toIsPrereq = False       -- Conservative: not a prerequisite
    , toMentions = extractMentionsFromSig (lsSignature sym)
    }

-- | Extract likely mentions from a type signature.
--
-- Simple heuristic: uppercase words that look like type names.
extractMentionsFromSig :: Text -> [Text]
extractMentionsFromSig sig =
  let words' = T.words sig
      -- Filter to words that start with uppercase (likely type names)
      typeNames = filter isTypeName words'
      -- Clean up punctuation
      cleaned = map (T.filter (`notElem` ("()[]{},:->=" :: String))) typeNames
      -- Remove common types we don't need to explain
      filtered = filter (not . isCommonType) cleaned
  in take 5 $ filter (not . T.null) filtered
  where
    isTypeName t = case T.uncons t of
      Just (c, _) -> c >= 'A' && c <= 'Z'
      Nothing -> False

    isCommonType t = t `elem`
      [ "Int", "Text", "String", "Bool", "Double", "Float"
      , "Maybe", "Either", "IO", "Eff", "Map", "Set", "List"
      ]
