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
import Tidepool.Control.Scout.Heuristics (scoreEdge)
import Tidepool.Training.Types
  ( Tag(..), ScoreEdgeInput(..), ScoreEdgeOutput(..)
  )
import qualified Tidepool.Training.Types as TT


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Gemma effect for semantic code scoring.
--
-- FunctionGemma is a fine-tuned 270M model that outputs structured Rubrics.
-- This effect allows swapping implementations:
--   * Stub: formats template, returns heuristic (development)
--   * Heuristic: pattern-based rules (baseline)
--   * Mock: hardcoded rubric (testing)
--   * HTTP: Ollama server (production)
data Gemma a where
  -- | Rate an edge given query context.
  --
  -- Input: EdgeContext (typed edge with hover, snippet, location)
  --        + query text
  -- Output: Rubric (relevance, risk, complexity, confidence, tags)
  RateEdge :: Text -> EdgeContext -> Gemma Rubric


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Rate an edge using the Gemma model.
--
-- This is the coalgebra: (Query, Edge) → Rubric
rateEdge :: Member Gemma effs => Text -> EdgeContext -> Eff effs Rubric
rateEdge query edge = send (RateEdge query edge)


-- ════════════════════════════════════════════════════════════════════════════
-- STUB INTERPRETER (Development)
-- ════════════════════════════════════════════════════════════════════════════

-- | Stub interpreter that formats the template but returns heuristic results.
--
-- This is the main development interpreter. It:
--   1. Renders the full FunctionGemma prompt (for debugging/logging)
--   2. Returns heuristic-based rubric (no model call)
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
    -- Return heuristic-based rubric (no model call)
    pure $ scoreEdge query edge


-- ════════════════════════════════════════════════════════════════════════════
-- HEURISTIC INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Heuristic interpreter using deterministic rules.
--
-- This is the baseline that FunctionGemma should improve upon.
-- Uses pattern matching on code snippets and hover info.
runGemmaHeuristic :: Eff (Gemma ': effs) a -> Eff effs a
runGemmaHeuristic = interpret $ \case
  RateEdge query edge -> pure $ scoreEdge query edge


-- ════════════════════════════════════════════════════════════════════════════
-- MOCK INTERPRETER (Testing)
-- ════════════════════════════════════════════════════════════════════════════

-- | Mock interpreter that returns a hardcoded rubric.
--
-- Useful for testing the exploration loop without any scoring logic.
-- Returns a "medium relevance, medium risk" rubric.
runGemmaMock :: Eff (Gemma ': effs) a -> Eff effs a
runGemmaMock = interpret $ \case
  RateEdge _query _edge -> pure mockRubric

-- | A reasonable default rubric for testing.
mockRubric :: Rubric
mockRubric = Rubric
  { rRelevance  = 3  -- Medium relevance
  , rRisk       = 3  -- Medium risk
  , rComplexity = 2  -- Low-medium complexity
  , rConfidence = 4  -- Reasonably confident
  , rTags       = [PatternMatch, Implementation]  -- Common tags
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
    result <- sendM $ callOllama endpoint input
    case result of
      Right output -> do
        sendM $ putStrLn $ "[Gemma] -> relevance=" <> show (seoRelevance output) <> ", risk=" <> show (seoRisk output)
        pure $ scoreEdgeOutputToRubric output
      Left err -> error $ unlines
        [ "FunctionGemma HTTP error: " <> T.unpack err
        , "  Endpoint: " <> T.unpack endpoint
        , "  Location: " <> T.unpack (ecLocation edge)
        ]


-- | Call Ollama's /api/chat endpoint with tool schema.
--
-- Ollama auto-translates the tools array into FunctionGemma's
-- <start_function_declaration> format internally.
callOllama :: Text -> ScoreEdgeInput -> IO (Either Text ScoreEdgeOutput)
callOllama endpoint input = do
  result <- try $ do
    let userContent = formatEdgeForScoring input
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
                 $ request

    response <- httpLBS request'
    pure $ parseOllamaResponse (getResponseBody response)

  case result of
    Left (e :: SomeException) -> pure $ Left $ T.pack $ show e
    Right parsed -> pure parsed


-- | Tool schema for score_edge function (OpenAI tool format).
scoreEdgeTool :: Value
scoreEdgeTool = object
  [ "type" .= ("function" :: Text)
  , "function" .= object
      [ "name" .= ("score_edge" :: Text)
      , "description" .= ("Score the relevance and risk of a code edge for impact analysis" :: Text)
      , "parameters" .= object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "relevance" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("1-5 rating of relevance to query" :: Text)
                  ]
              , "risk" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("1-5 rating of change risk" :: Text)
                  ]
              , "reasoning" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Explanation for scores" :: Text)
                  ]
              , "is_exhaustive" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("Is this a pattern match requiring exhaustive cases" :: Text)
                  ]
              , "is_type_family" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("Is this type-level computation" :: Text)
                  ]
              , "is_exported" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("Is this part of public API" :: Text)
                  ]
              ]
          , "required" .= (["relevance", "risk", "reasoning", "is_exhaustive", "is_type_family", "is_exported"] :: [Text])
          ]
      ]
  ]


-- | Format edge context as plain text user message.
formatEdgeForScoring :: ScoreEdgeInput -> Text
formatEdgeForScoring input = T.unlines
  [ "Score this edge for impact analysis:"
  , "Query: " <> seiQuery input
  , "Source: " <> seiSourceFile input <> ":" <> T.pack (show $ seiSourceLine input) <> " - " <> seiSourceHover input
  , "Target: " <> seiTargetFile input <> ":" <> T.pack (show $ seiTargetLine input) <> " - " <> seiTargetHover input
  , "Edge type: " <> T.pack (show $ seiEdgeType input)
  ]


-- | Parse Ollama response and extract ScoreEdgeOutput from tool_calls.
parseOllamaResponse :: ByteString -> Either Text ScoreEdgeOutput
parseOllamaResponse body = do
  json <- case Aeson.decode body of
    Nothing -> Left $ "Failed to parse JSON response"
    Just v  -> Right (v :: Value)

  -- Extract message.tool_calls[0].function.arguments
  args <- extractToolCallArgs json

  -- Parse arguments object directly (Ollama returns structured JSON)
  case Aeson.fromJSON args of
    Aeson.Error e -> Left $ "Failed to parse arguments: " <> T.pack e
    Aeson.Success out -> Right out


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
scoreEdgeOutputToRubric :: ScoreEdgeOutput -> Rubric
scoreEdgeOutputToRubric out = Rubric
  { rRelevance  = seoRelevance out
  , rRisk       = seoRisk out
  , rComplexity = 3  -- Not in new format, use default
  , rConfidence = 4  -- High confidence from model
  , rTags       = outputToTags out
  }


-- | Convert boolean flags to Tag list.
outputToTags :: ScoreEdgeOutput -> [Tag]
outputToTags out = catMaybes
  [ if seoIsExhaustive out then Just Exhaustive else Nothing
  , if seoIsTypeFamily out then Just TypeFamily else Nothing
  -- isExported doesn't map to a tag directly
  ]
