{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

-- | Candidate selection for teaching symbol extraction.
--
-- FunctionGemma 270M is a "traffic controller" - trained for entity extraction
-- and classification, NOT generative reasoning. When asked to extract related
-- symbols from a signature, it echoes the input.
--
-- Solution: Transform from generation to selection:
-- - ❌ BEFORE (generation): "What symbols are related?" → echoes input
-- - ✅ AFTER (selection): "Which of [A,B,C] are relevant?" → classifies
--
-- Architecture:
-- @
-- ┌─────────┐     ┌────────────────┐     ┌─────────┐     ┌────────────┐
-- │  LSP    │────▶│  Deterministic │────▶│  Gemma  │────▶│ [Selected] │
-- │  hover  │     │  Extraction    │     │ SELECT  │     └────────────┘
-- └─────────┘     └────────────────┘     └─────────┘
--                        │
--                        ▼
--                 Candidates: [ScoreConfig, EdgeContext, ...]
-- @
--
-- Key insight: Deterministic extraction gets candidates. Gemma only classifies.
module Tidepool.Control.Scout.DocGen.Gemma
  ( -- * Effect
    ScoutGemma(..)

    -- * Smart Constructors
  , selectRelevantSymbols

    -- * Deterministic Candidate Extraction
  , extractCandidates

    -- * Interpreters
  , runScoutGemmaHTTP
  , runScoutGemmaMock
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (Eff, Member, send, interpret, LastMember, sendM)
import Data.Aeson (encode, object, (.=), Value, (.:), toJSON)
import Data.List (nub)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.HTTP.Simple

import Tidepool.Control.Scout.DocGen.Types (LSPSymbol(..))


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | ScoutGemma effect for symbol selection.
--
-- Given a topic, symbol, and pre-extracted candidates, selects which
-- candidates are relevant to understanding the topic.
--
-- Key insight: This is CLASSIFICATION, not generation. The model picks
-- from a fixed set rather than generating new symbols.
data ScoutGemma a where
  -- | Select relevant symbols from pre-extracted candidates.
  --
  -- Input:
  --   - topic: what we're trying to understand
  --   - symbol: the symbol being analyzed (with LSP data)
  --   - candidates: symbols extracted from signature (deterministic)
  -- Output: subset of candidates relevant to the topic
  SelectRelevantSymbols
    :: Text        -- ^ Topic description
    -> LSPSymbol   -- ^ Symbol being analyzed
    -> [Text]      -- ^ Candidate symbols (pre-extracted)
    -> ScoutGemma [Text]  -- ^ Selected subset


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Select relevant symbols from pre-extracted candidates.
--
-- This is the main entry point. Caller should:
-- 1. Extract candidates using 'extractCandidates'
-- 2. Pass them here for selection
selectRelevantSymbols
  :: Member ScoutGemma effs
  => Text        -- ^ Topic description
  -> LSPSymbol   -- ^ Symbol to analyze
  -> [Text]      -- ^ Candidate symbols (from extractCandidates)
  -> Eff effs [Text]
selectRelevantSymbols topic sym candidates =
  send (SelectRelevantSymbols topic sym candidates)


-- ════════════════════════════════════════════════════════════════════════════
-- DETERMINISTIC CANDIDATE EXTRACTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract candidate symbols from a Haskell type signature.
--
-- This is DETERMINISTIC - no LLM needed. Parses the signature and
-- extracts uppercase identifiers, filtering common types.
--
-- Handles markdown-formatted hover content by extracting just the code block.
--
-- Examples:
-- @
-- extractCandidates "compositeScore :: ScoreConfig -> ScoreEdgeOutput -> Double"
--   == ["ScoreConfig", "ScoreEdgeOutput"]
--
-- extractCandidates "foo :: Int -> Text -> Maybe Bar"
--   == ["Bar"]  -- Int, Text, Maybe filtered out
-- @
extractCandidates :: Text -> [Text]
extractCandidates sig =
  let -- Strip markdown: extract just the code from ```haskell ... ```
      codeOnly = extractCodeBlock sig
      -- Tokenize: split on whitespace and punctuation
      tokens = concatMap (T.splitOn " ")
             $ concatMap (T.splitOn "->")
             $ concatMap (T.splitOn "=>")
             $ T.splitOn "::" codeOnly
      -- Clean each token
      cleaned = map cleanToken tokens
      -- Keep uppercase identifiers (type names)
      typeNames = filter isTypeName cleaned
      -- Remove primitives and common wrappers
      filtered = filter (not . isCommonType) typeNames
  in nub filtered  -- deduplicate
  where
    -- Extract code from markdown code blocks, or return raw if no blocks
    extractCodeBlock :: Text -> Text
    extractCodeBlock t =
      let lines' = T.lines t
          -- Find content between ``` markers
          inCodeBlock = dropWhile (not . T.isPrefixOf "```") lines'
          afterOpen = drop 1 inCodeBlock  -- Drop the ```haskell line
          codeLines = takeWhile (not . T.isPrefixOf "```") afterOpen
      in if null codeLines
         then t  -- No code block found, use raw text
         else T.unlines codeLines

    cleanToken :: Text -> Text
    cleanToken = T.filter (`notElem` ("()[]{},:=`*\n" :: String)) . T.strip

    isTypeName :: Text -> Bool
    isTypeName t = case T.uncons t of
      Just (c, _) -> c >= 'A' && c <= 'Z' && T.length t > 1
      Nothing -> False

    isCommonType :: Text -> Bool
    isCommonType t = t `elem`
      [ "Int", "Integer", "Float", "Double", "Bool", "Char"
      , "Text", "String", "ByteString"
      , "Maybe", "Either", "IO", "ST", "STM"
      , "Eff", "Member", "LastMember"
      , "Map", "Set", "List", "Vector", "Array"
      , "Monad", "Functor", "Applicative", "Monoid"
      ]


-- ════════════════════════════════════════════════════════════════════════════
-- HTTP INTERPRETER (Production)
-- ════════════════════════════════════════════════════════════════════════════

-- | HTTP interpreter that calls Ollama for symbol selection.
--
-- Uses Ollama's /api/chat endpoint with enum-constrained tool schema.
-- The enum constraint ensures the model can ONLY output valid candidates.
runScoutGemmaHTTP
  :: LastMember IO effs
  => Text  -- ^ Ollama endpoint (e.g., "http://localhost:11434")
  -> Eff (ScoutGemma ': effs) a
  -> Eff effs a
runScoutGemmaHTTP endpoint = interpret $ \case
  SelectRelevantSymbols topic sym candidates -> do
    sendM $ putStrLn $ "[ScoutGemma] Candidates: " <> T.unpack (T.intercalate ", " candidates)

    -- If no candidates, nothing to select
    if null candidates
      then do
        sendM $ putStrLn "[ScoutGemma] No candidates to select from"
        pure []
      else do
        sendM $ putStrLn $ "[ScoutGemma] HTTP call to " <> T.unpack endpoint
          <> " for: " <> T.unpack (lsName sym)
        result <- sendM $ callOllamaSelect endpoint topic sym candidates
        case result of
          Right selected -> do
            sendM $ putStrLn $ "[ScoutGemma] Selected: " <> T.unpack (T.intercalate ", " selected)
            -- Validate: only return candidates that were in the input
            let valid = filter (`elem` candidates) selected
            if null valid && not (null selected)
              then do
                sendM $ putStrLn "[ScoutGemma] Selection invalid, using all candidates"
                pure candidates  -- Fallback
              else pure valid
          Left err -> do
            sendM $ putStrLn $ "[ScoutGemma] ERROR: " <> T.unpack err
            sendM $ putStrLn "[ScoutGemma] Selection failed, using all candidates"
            pure candidates  -- Fallback: return all candidates


-- | Call Ollama's /api/chat endpoint for symbol selection.
--
-- Uses enum-constrained tool schema so model can ONLY pick from candidates.
callOllamaSelect :: Text -> Text -> LSPSymbol -> [Text] -> IO (Either Text [Text])
callOllamaSelect endpoint topic sym candidates = do
  let userContent = formatSelectionPrompt topic sym candidates
  putStrLn $ "[ScoutGemma] Selection prompt:\n" <> T.unpack userContent

  result <- try $ do
    let reqBody = encode $ object
          [ "model" .= ("functiongemma-tidepool" :: Text)
          , "messages" .= [object
              [ "role" .= ("user" :: Text)
              , "content" .= userContent
              ]]
          , "tools" .= [selectSymbolsTool candidates]
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
    putStrLn $ "[ScoutGemma] Raw response: " <> take 1000 (show body)
    pure $ parseSelectionResponse body

  case result of
    Left (e :: SomeException) -> pure $ Left $ T.pack $ show e
    Right parsed -> pure parsed


-- | Format the selection prompt for symbol selection.
--
-- Ollama translates this plain text + tools array into FunctionGemma's native
-- token format (<start_of_turn>, <start_function_call>, etc.). We provide:
-- - Topic: what we're trying to understand
-- - Symbol context: name + signature from hover
-- - Candidates: types extracted from signature (deterministic)
--
-- The model selects which candidates are relevant via the select_symbols tool.
formatSelectionPrompt :: Text -> LSPSymbol -> [Text] -> Text
formatSelectionPrompt topic sym candidates = T.unlines
  [ "Topic: " <> topic
  , "Symbol: " <> lsName sym
  , "Signature: " <> cleanSignature (lsSignature sym)
  , "Candidates: " <> T.intercalate ", " candidates
  , ""
  , "Select the candidates that help understand this symbol in context of the topic."
  ]
  where
    -- Clean signature: extract code from markdown if present, strip formatting
    cleanSignature :: Text -> Text
    cleanSignature sig =
      let lines' = T.lines sig
          -- Find content between ``` markers if present
          inCodeBlock = dropWhile (not . T.isPrefixOf "```") lines'
          afterOpen = drop 1 inCodeBlock
          codeLines = takeWhile (not . T.isPrefixOf "```") afterOpen
      in if null codeLines
         then T.strip sig  -- No code block, use raw
         else T.strip $ T.unlines codeLines


-- | Tool schema for select_symbols function with enum constraint.
--
-- The enum constraint is CRITICAL - it tells the model the ONLY valid
-- outputs are from the candidate list. This prevents hallucination.
selectSymbolsTool :: [Text] -> Value
selectSymbolsTool candidates = object
  [ "type" .= ("function" :: Text)
  , "function" .= object
      [ "name" .= ("select_symbols" :: Text)
      , "description" .= ("Select relevant symbols from candidates" :: Text)
      , "parameters" .= object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "selected" .= object
                  [ "type" .= ("array" :: Text)
                  , "items" .= object
                      [ "type" .= ("string" :: Text)
                      , "enum" .= toJSON candidates  -- Constrain to valid choices!
                      ]
                  , "description" .= ("Selected symbol names from the candidate list" :: Text)
                  ]
              ]
          , "required" .= (["selected"] :: [Text])
          ]
      ]
  ]


-- | Parse Ollama response for symbol selection (array format).
parseSelectionResponse :: ByteString -> Either Text [Text]
parseSelectionResponse body = do
  json <- case Aeson.decode body of
    Nothing -> Left "Failed to parse JSON response"
    Just v  -> Right (v :: Value)

  -- Extract message.tool_calls[0].function.arguments.selected (array)
  extractSelectedFromToolCall json


-- | Extract selected symbols array from Ollama tool call response.
--
-- Response format:
-- {
--   "message": {
--     "tool_calls": [{
--       "function": {
--         "arguments": { "selected": ["ScoreConfig", "EdgeContext"] }
--       }
--     }]
--   }
-- }
extractSelectedFromToolCall :: Value -> Either Text [Text]
extractSelectedFromToolCall json = case json of
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
                      Just (Aeson.Object args) ->
                        case parseMaybe (.: "selected") args of
                          Just (Aeson.Array arr) ->
                            -- Extract strings from array
                            Right $ extractStrings (V.toList arr)
                          _ -> Left "No 'selected' array in arguments"
                      _ -> Left "No arguments in function"
                  _ -> Left "No function in tool_call"
              _ -> Left "Invalid tool_call format"
          _ -> Left "No tool_calls in message"
      _ -> Left "No message in response"
  _ -> Left "Response is not an object"
  where
    extractStrings :: [Value] -> [Text]
    extractStrings = concatMap $ \case
      Aeson.String s -> [s]
      _ -> []


-- ════════════════════════════════════════════════════════════════════════════
-- MOCK INTERPRETER (Testing)
-- ════════════════════════════════════════════════════════════════════════════

-- | Mock interpreter for testing.
--
-- Simply returns all candidates (no filtering). Useful for testing the
-- exploration loop without Gemma dependency.
runScoutGemmaMock :: Eff (ScoutGemma ': effs) a -> Eff effs a
runScoutGemmaMock = interpret $ \case
  SelectRelevantSymbols _topic _sym candidates -> pure candidates
