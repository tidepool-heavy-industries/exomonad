{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- | Gemma effect for semantic code scoring.
--
-- FunctionGemma is the coalgebra in the exploration anamorphism:
--   coalgebra :: (Query, Edge) → Rubric
--
-- The model handles one edge at a time; the exploration loop handles recursion.
--
-- Architecture:
--
-- @
-- ┌─────────────────────────────────────────┐
-- │  Exploration Loop                        │
-- │    │                                     │
-- │    ▼                                     │
-- │  rateEdge :: EdgeContext -> Gemma Rubric │
-- │    │                                     │
-- └────┼─────────────────────────────────────┘
--      │
--      ▼
-- ┌─────────────────────────────────────────┐
-- │  Interpreter (pluggable)                 │
-- │                                         │
-- │  • runGemmaStub      - formats template, returns heuristic (development)
-- │  • runGemmaHeuristic - pattern-based rules (baseline)
-- │  • runGemmaMock      - hardcoded rubric (testing)
-- │  • runGemmaHTTP      - Ollama server (production)
-- └─────────────────────────────────────────┘
-- @
module Tidepool.Control.Scout.Gemma
  ( -- * Effect
    Gemma(..)

    -- * Smart Constructors
  , rateEdge

    -- * Interpreters
  , runGemmaStub
  , runGemmaHeuristic
  , runGemmaMock
  , runGemmaHTTP

    -- * Conversion Helpers
  , scoreEdgeOutputToRubric
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (Eff, Member, send, interpret, LastMember, sendM)
import Data.Aeson (encode, object, (.=), Value, (.:))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.HTTP.Simple
import Text.Read (readMaybe)

import Tidepool.Control.Scout.Types (Rubric(..))
import Tidepool.Control.Scout.EdgeTypes (EdgeContext(..), EdgeType(..))
import Tidepool.Control.Scout.Templates (mkScoringContext, renderScoringPrompt)
import Tidepool.Training.Types
  ( Tag(..), ScoreEdgeInput(..), ScoreEdgeOutput(..)
  )
import Tidepool.Training.Arbitrary (heuristicScoreEdge)
import qualified Tidepool.Training.Types as TT


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Gemma effect for semantic code scoring.
--
-- FunctionGemma is a fine-tuned 270M model that outputs boolean answers
-- to semantic questions. This effect allows swapping implementations:
--   * Stub: formats template, returns heuristic (development)
--   * Heuristic: pattern-based rules (baseline)
--   * Mock: hardcoded output (testing)
--   * HTTP: Ollama server (production)
data Gemma a where
  -- | Rate an edge given query context.
  --
  -- Input: EdgeContext (typed edge with hover, snippet, location)
  --        + query text
  -- Output: ScoreEdgeOutput (4 booleans + reasoning)
  RateEdge :: Text -> EdgeContext -> Gemma ScoreEdgeOutput


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Rate an edge using the Gemma model.
--
-- This is the coalgebra: (Query, Edge) → ScoreEdgeOutput
rateEdge :: Member Gemma effs => Text -> EdgeContext -> Eff effs ScoreEdgeOutput
rateEdge query edge = send (RateEdge query edge)


-- ════════════════════════════════════════════════════════════════════════════
-- STUB INTERPRETER (Development)
-- ════════════════════════════════════════════════════════════════════════════

-- | Stub interpreter that formats the template but returns heuristic results.
--
-- This is the main development interpreter. It:
--   1. Renders the full FunctionGemma prompt (for debugging/logging)
--   2. Returns heuristic-based ScoreEdgeOutput (no model call)
--
-- Useful for:
--   * Developing the exploration loop
--   * Debugging prompt rendering
--   * Testing without model dependency
runGemmaStub :: Eff (Gemma ': effs) a -> Eff effs a
runGemmaStub = interpret $ \case
  RateEdge query edge -> do
    -- Format the prompt (useful for debugging template rendering)
    let ctx = mkScoringContext query edge
    let _prompt = renderScoringPrompt ctx
    -- Return heuristic-based output (no model call)
    let input = edgeContextToInput query edge
    pure $ heuristicScoreEdge input


-- ════════════════════════════════════════════════════════════════════════════
-- HEURISTIC INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Heuristic interpreter using deterministic rules.
--
-- This is the baseline that FunctionGemma should improve upon.
-- Uses pattern matching on code snippets and hover info.
runGemmaHeuristic :: Eff (Gemma ': effs) a -> Eff effs a
runGemmaHeuristic = interpret $ \case
  RateEdge query edge ->
    let input = edgeContextToInput query edge
    in pure $ heuristicScoreEdge input


-- ════════════════════════════════════════════════════════════════════════════
-- MOCK INTERPRETER (Testing)
-- ════════════════════════════════════════════════════════════════════════════

-- | Mock interpreter that returns a hardcoded output.
--
-- Useful for testing the exploration loop without any scoring logic.
-- Returns a conservative default (all false except query_relevant).
runGemmaMock :: Eff (Gemma ': effs) a -> Eff effs a
runGemmaMock = interpret $ \case
  RateEdge _query _edge -> pure mockOutput

-- | A reasonable default output for testing.
mockOutput :: ScoreEdgeOutput
mockOutput = ScoreEdgeOutput
  { seoIsQueryRelevant    = True   -- Assume relevant for testing
  , seoIsBreakingBoundary = False  -- Conservative default
  , seoIsStableAnchor     = True   -- Assume stable for testing
  , seoIsPublicContract   = False  -- Conservative default
  , seoReasoning          = "Mock output for testing"
  }


-- ════════════════════════════════════════════════════════════════════════════
-- HTTP INTERPRETER (Production)
-- ════════════════════════════════════════════════════════════════════════════

-- | HTTP interpreter that calls Ollama for FunctionGemma scoring.
--
-- Uses Ollama's /api/chat endpoint with native tool support.
-- Ollama auto-translates tools array → FunctionGemma's <start_function_declaration> format.
--
-- Requires IO at the base of the effect stack (LastMember IO).
--
-- Example:
--   runM $ runGemmaHTTP "http://localhost:11434" $ runLSP session $ exploreEff query
runGemmaHTTP :: LastMember IO effs => Text -> Eff (Gemma ': effs) a -> Eff effs a
runGemmaHTTP endpoint = interpret $ \case
  RateEdge query edge -> do
    let input = edgeContextToInput query edge
    sendM $ putStrLn $ "[Gemma] HTTP call to " <> T.unpack endpoint <> " for: " <> T.unpack (ecLocation edge)
    result <- sendM $ callOllamaWithRetry endpoint input 3  -- 3 attempts total (1 + 2 retries)
    case result of
      Right output -> do
        sendM $ putStrLn $ "[Gemma] -> query_relevant=" <> show (seoIsQueryRelevant output)
                        <> ", breaking=" <> show (seoIsBreakingBoundary output)
                        <> ", stable=" <> show (seoIsStableAnchor output)
                        <> ", public=" <> show (seoIsPublicContract output)
        pure output
      Left err -> error $ unlines
        [ "FunctionGemma HTTP error: " <> T.unpack err
        , "  Endpoint: " <> T.unpack endpoint
        , "  Location: " <> T.unpack (ecLocation edge)
        ]

-- | Call Ollama with retry logic.
--
-- Retries up to (attempts - 1) times on parse failures.
callOllamaWithRetry :: Text -> ScoreEdgeInput -> Int -> IO (Either Text ScoreEdgeOutput)
callOllamaWithRetry endpoint input attempts = go attempts
  where
    go 0 = pure $ Left "Max retries exceeded"
    go n = do
      result <- callOllama endpoint input
      case result of
        Right output -> pure $ Right output
        Left err -> do
          if n > 1
            then do
              putStrLn $ "[Gemma] Parse failed (attempt " <> show (attempts - n + 1) <> "/" <> show attempts <> "): " <> T.unpack err
              putStrLn $ "[Gemma] Retrying..."
              go (n - 1)
            else pure $ Left err


-- | Call Ollama's /api/chat endpoint with tool schema.
--
-- Ollama auto-translates the tools array into FunctionGemma's
-- <start_function_declaration> format internally.
callOllama :: Text -> ScoreEdgeInput -> IO (Either Text ScoreEdgeOutput)
callOllama endpoint input = do
  let userContent = formatEdgeForScoring input
  -- Log input
  putStrLn $ "[Gemma] Input prompt:\n" <> T.unpack userContent

  result <- try $ do
    let reqBody = encode $ object
          [ "model" .= ("functiongemma:270m" :: Text)
          , "messages" .= [object
              [ "role" .= ("user" :: Text)
              , "content" .= userContent
              ]]
          , "tools" .= [scoreEdgeTool]
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
    -- Log raw response
    putStrLn $ "[Gemma] Raw response: " <> take 1000 (show body)
    pure $ parseOllamaResponse body

  case result of
    Left (e :: SomeException) -> pure $ Left $ T.pack $ show e
    Right parsed -> pure parsed


-- | Tool schema for score_edge function (OpenAI tool format).
--
-- Uses 4 semantic boolean questions that FunctionGemma should answer:
--   - is_query_relevant: Does this edge help answer the query?
--   - is_breaking_boundary: Would changes at source require changes at target?
--   - is_stable_anchor: Is this a stable reference point?
--   - is_public_contract: Is this part of the public API?
--
-- EdgeType (pattern match, type ref, etc.) comes from LSP metadata, NOT from the model.
scoreEdgeTool :: Value
scoreEdgeTool = object
  [ "type" .= ("function" :: Text)
  , "function" .= object
      [ "name" .= ("score_edge" :: Text)
      , "description" .= ("Answer semantic questions about a code edge for impact analysis" :: Text)
      , "parameters" .= object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "is_query_relevant" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("Does this edge directly help answer the user's question?" :: Text)
                  ]
              , "is_breaking_boundary" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("Would modifying source code likely require changes at target?" :: Text)
                  ]
              , "is_stable_anchor" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("Is this a stable reference point that rarely changes?" :: Text)
                  ]
              , "is_public_contract" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("Is this part of the module's public API surface?" :: Text)
                  ]
              , "reasoning" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Brief justification (1-2 sentences)" :: Text)
                  ]
              ]
          , "required" .= (["is_query_relevant", "is_breaking_boundary", "is_stable_anchor", "is_public_contract", "reasoning"] :: [Text])
          ]
      ]
  ]


-- | Format edge context as plain text user message.
--
-- Expanded prompt with guidance for non-fine-tuned models.
-- Includes task context, semantic questions with examples, and heuristics.
formatEdgeForScoring :: ScoreEdgeInput -> Text
formatEdgeForScoring input = T.unlines
  [ "You are analyzing code impact for a semantic exploration tool."
  , "Given a user query and a code edge (source → target), answer semantic questions."
  , ""
  , "## User Query"
  , "\"" <> seiQuery input <> "\""
  , ""
  , "## Code Edge"
  , ""
  , "SOURCE (" <> T.pack (show $ seiEdgeType input) <> "):"
  , "  File: " <> seiSourceFile input <> ":" <> T.pack (show $ seiSourceLine input)
  , "  " <> seiSourceHover input
  , ""
  , "TARGET:"
  , "  File: " <> seiTargetFile input <> ":" <> T.pack (show $ seiTargetLine input)
  , "  " <> seiTargetHover input
  , ""
  , "## Questions (answer true or false)"
  , ""
  , "1. **is_query_relevant**: Does this edge directly help answer the user's query?"
  , "   - true: The code at target is directly related to what the user asked about"
  , "   - false: This is incidental/unrelated code"
  , "   - Hint: Look for keyword matches, type relationships, or semantic connection to the query"
  , ""
  , "2. **is_breaking_boundary**: Would modifying the source likely require changes at target?"
  , "   - true: Pattern matches on the source type, type family instances, exhaustive cases"
  , "   - false: Simple function calls, imports, re-exports"
  , "   - Hint: 'case' expressions and type families almost always = true"
  , ""
  , "3. **is_stable_anchor**: Is the target a stable reference point that rarely changes?"
  , "   - true: Core type definitions, well-established APIs, Types.hs files"
  , "   - false: Test files, experimental code, frequently-changing implementations"
  , "   - Hint: Type definitions are usually stable, implementations change more often"
  , ""
  , "4. **is_public_contract**: Is the target part of the module's public API surface?"
  , "   - true: Exported functions/types, no underscore prefix, in API/Types/Public modules"
  , "   - false: Internal helpers (prefixed with _), unexported, in Internal modules"
  , "   - Hint: If it's in a Types.hs or API.hs file, probably true"
  , ""
  , "Call the score_edge function with your answers and brief reasoning."
  ]


-- | Parse Ollama response and extract ScoreEdgeOutput from tool_calls.
--
-- Tolerant parsing: FunctionGemma 270M often omits boolean fields.
-- All boolean fields default to False if missing (conservative default).
parseOllamaResponse :: ByteString -> Either Text ScoreEdgeOutput
parseOllamaResponse body = do
  json <- case Aeson.decode body of
    Nothing -> Left $ "Failed to parse JSON response"
    Just v  -> Right (v :: Value)

  -- Extract message.tool_calls[0].function.arguments
  args <- extractToolCallArgs json

  -- Tolerant parsing with defaults for missing boolean fields
  case args of
    Aeson.Object obj -> do
      -- All boolean fields default to False if missing
      let isQueryRelevant = case parseMaybe (.: "is_query_relevant") obj of
            Just (b :: Bool) -> b
            Nothing -> False
      let isBreakingBoundary = case parseMaybe (.: "is_breaking_boundary") obj of
            Just (b :: Bool) -> b
            Nothing -> False
      let isStableAnchor = case parseMaybe (.: "is_stable_anchor") obj of
            Just (b :: Bool) -> b
            Nothing -> False
      let isPublicContract = case parseMaybe (.: "is_public_contract") obj of
            Just (b :: Bool) -> b
            Nothing -> False
      let reasoning = case parseMaybe (.: "reasoning") obj of
            Just (t :: Text) -> t
            Nothing -> "No reasoning provided"
      Right ScoreEdgeOutput
        { seoIsQueryRelevant = isQueryRelevant
        , seoIsBreakingBoundary = isBreakingBoundary
        , seoIsStableAnchor = isStableAnchor
        , seoIsPublicContract = isPublicContract
        , seoReasoning = reasoning
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
-- CONVERSION HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert EdgeContext (control-server) to ScoreEdgeInput (training format).
edgeContextToInput :: Text -> EdgeContext -> ScoreEdgeInput
edgeContextToInput query edge =
  let (sourceFile, sourceLine) = parseLocation (ecLocation edge)
      (targetFile, targetLine) = case ecParent edge of
        Just parent -> parseLocation parent
        Nothing     -> (sourceFile, sourceLine)
  in ScoreEdgeInput
    { seiQuery = query
    , seiSourceFile = targetFile  -- Source is where we came from
    , seiSourceLine = targetLine
    , seiSourceHover = maybe "" id (ecTypeName edge)
    , seiTargetFile = sourceFile  -- Target is current location
    , seiTargetLine = sourceLine
    , seiTargetHover = ecHover edge
    , seiEdgeType = edgeTypeToTraining (ecEdgeType edge)
    }


-- | Parse "File.hs:42" into (file, line).
parseLocation :: Text -> (Text, Int)
parseLocation loc =
  let (file, lineStr) = T.breakOnEnd ":" loc
      file' = T.dropEnd 1 file  -- Remove trailing ":"
      line = maybe 1 id $ readMaybe $ T.unpack lineStr
  in (file', line)


-- | Convert control-server EdgeType to training-generator EdgeType.
edgeTypeToTraining :: EdgeType -> TT.EdgeType
edgeTypeToTraining = \case
  TypeReference    -> TT.Reference
  ValueReference   -> TT.Reference
  DefinitionSite   -> TT.Definition
  UsageSite        -> TT.Usage
  ImportEdge       -> TT.Usage
  ExportEdge       -> TT.Usage
  ConstructorRef   -> TT.Reference
  PatternMatchSite -> TT.Reference
  InstanceSite     -> TT.Instance
  UnknownEdge      -> TT.Usage


-- | Convert ScoreEdgeOutput to legacy Rubric format.
--
-- This is a transitional function for backwards compatibility.
-- The new boolean-based output is converted to integer scores by:
--   - relevance: 5 if query_relevant, else 2
--   - risk: 5 if breaking_boundary OR public_contract, else 2
--   - complexity: 2 if stable_anchor, else 4
scoreEdgeOutputToRubric :: ScoreEdgeOutput -> Rubric
scoreEdgeOutputToRubric out = Rubric
  { rRelevance  = if seoIsQueryRelevant out then 5 else 2
  , rRisk       = if seoIsBreakingBoundary out || seoIsPublicContract out then 5 else 2
  , rComplexity = if seoIsStableAnchor out then 2 else 4
  , rConfidence = 4  -- High confidence from model
  , rTags       = outputToTags out
  }


-- | Convert boolean flags to Tag list.
--
-- Maps the new semantic booleans back to the legacy Tag enum:
--   - breaking_boundary → Exhaustive (pattern match concern)
--   - public_contract → Implementation (exported code)
outputToTags :: ScoreEdgeOutput -> [Tag]
outputToTags out = catMaybes
  [ if seoIsBreakingBoundary out then Just Exhaustive else Nothing
  , if seoIsPublicContract out then Just Implementation else Nothing
  ]
