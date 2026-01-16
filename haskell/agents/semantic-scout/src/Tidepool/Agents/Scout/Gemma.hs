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
-- │  • runGemmaStub      - formats template, returns heuristic
-- │  • runGemmaHeuristic - pattern-based rules
-- │  • runGemmaMock      - hardcoded rubric (testing)
-- │  • runGemmaHTTP      - mistral.rs server
-- └─────────────────────────────────────────┘
-- @
module Tidepool.Agents.Scout.Gemma
  ( -- * Effect
    Gemma(..)

    -- * Smart Constructors
  , rateEdge
  , rateNode  -- Legacy, for compatibility

    -- * Interpreters
  , runGemmaStub
  , runGemmaHeuristic
  , runGemmaMock
  , runGemmaHTTP
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (Eff, Member, send, interpret)
import System.IO.Unsafe (unsafePerformIO)
import Data.Aeson (encode, object, (.=), Value, (.:))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Vector as V
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Simple
import Text.Read (readMaybe)

import Tidepool.Agents.Scout.Types (QueryContext(..), Rubric(..))
import Tidepool.Agents.Scout.EdgeTypes (EdgeContext(..), EdgeType(..))
import Tidepool.Agents.Scout.Templates (mkScoringContext, renderScoringPrompt)
import Tidepool.Agents.Scout.Heuristics (scoreNode, scoreEdge)
import Tidepool.Training.Types
  ( Tag(..), NodeContext(..)
  , ScoreEdgeInput(..), ScoreEdgeOutput(..)
  )
import qualified Tidepool.Training.Types as TT
import Tidepool.Training.Format (formatUserTurn)


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
--   * HTTP: mistral.rs server (production)
data Gemma a where
  -- | Rate an edge given query context.
  --
  -- Input: EdgeContext (typed edge with hover, snippet, location)
  --        + query text
  -- Output: Rubric (relevance, risk, complexity, confidence, tags)
  RateEdge :: Text -> EdgeContext -> Gemma Rubric

  -- | Legacy: Rate a node (for backward compatibility).
  RateNode :: QueryContext -> NodeContext -> Gemma Rubric


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Rate an edge using the Gemma model.
--
-- This is the coalgebra: (Query, Edge) → Rubric
rateEdge :: Member Gemma effs => Text -> EdgeContext -> Eff effs Rubric
rateEdge query edge = send (RateEdge query edge)

-- | Legacy: Rate a node using the Gemma model.
rateNode :: Member Gemma effs => QueryContext -> NodeContext -> Eff effs Rubric
rateNode query node = send (RateNode query node)


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
    -- Format the prompt (could log this for debugging)
    let ctx = mkScoringContext query edge
    let _prompt = renderScoringPrompt ctx  -- TODO: Log this

    -- Return heuristic-based rubric
    pure $ scoreEdge query edge

  RateNode query node -> pure $ scoreNode query node


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
  RateNode query node -> pure $ scoreNode query node


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
  RateNode _query _node -> pure mockRubric

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

-- | HTTP interpreter that calls mistralrs-server for scoring.
--
-- Uses OpenAI-compatible /v1/chat/completions endpoint.
-- Propagates errors to caller on HTTP or parse failures.
--
-- Note: Uses unsafePerformIO internally. This is acceptable because:
-- 1. The HTTP call is idempotent (same input → same output)
-- 2. Errors are propagated via runtime exception
-- 3. The alternative (adding IO to effect stack) complicates the API
--
-- Example:
--   runGemmaHTTP "http://localhost:8080" $ exploreEff query
runGemmaHTTP :: Text -> Eff (Gemma ': effs) a -> Eff effs a
runGemmaHTTP endpoint = interpret $ \case
  RateEdge query edge ->
    let input = edgeContextToInput query edge
        prompt = formatUserTurn input
        result = unsafePerformIO $ callMistralRS endpoint prompt
    in case result of
         Right output -> pure $ scoreEdgeOutputToRubric output
         Left err     -> error $ "FunctionGemma HTTP error: " <> T.unpack err

  RateNode query node ->
    -- RateNode uses heuristics (not trained on node format)
    pure $ scoreNode query node

-- | Call mistralrs-server with formatted prompt.
-- Catches all exceptions and returns Left on error.
callMistralRS :: Text -> Text -> IO (Either Text ScoreEdgeOutput)
callMistralRS endpoint prompt = do
  result <- try $ do
    let reqBody = encode $ object
          [ "model" .= ("default" :: Text)
          , "messages" .= [object
              [ "role" .= ("user" :: Text)
              , "content" .= prompt
              ]]
          , "max_tokens" .= (256 :: Int)
          , "temperature" .= (0.1 :: Double)
          , "stop" .= (["<end_of_turn>", "<end_function_call>"] :: [Text])
          ]

    let url = T.unpack endpoint <> "/v1/chat/completions"
    request <- parseRequest url
    let request' = setRequestMethod "POST"
                 $ setRequestHeader "Content-Type" ["application/json"]
                 $ setRequestBodyLBS reqBody
                 $ request

    response <- httpLBS request'
    let body = getResponseBody response
    pure $ parseOpenAIResponse body

  case result of
    Left (e :: SomeException) -> pure $ Left $ T.pack $ show e
    Right parsed -> pure parsed


-- | Parse OpenAI-compatible response and extract ScoreEdgeOutput.
parseOpenAIResponse :: ByteString -> Either Text ScoreEdgeOutput
parseOpenAIResponse body = do
  -- Parse JSON response
  json <- case Aeson.decode body of
    Nothing -> Left "Failed to parse JSON response"
    Just v  -> Right (v :: Value)

  -- Extract content from choices[0].message.content
  content <- extractContent json

  -- Parse function call from content
  parseFunctionCall content


-- | Extract content from OpenAI response JSON.
extractContent :: Value -> Either Text Text
extractContent json = case json of
  Aeson.Object obj ->
    case parseMaybe (.: "choices") obj of
      Just (Aeson.Array choices) | not (V.null choices) ->
        case V.head choices of
          Aeson.Object choice ->
            case parseMaybe (.: "message") choice of
              Just (Aeson.Object msg) ->
                case parseMaybe (.: "content") msg of
                  Just (Aeson.String content) -> Right content
                  _ -> Left "No content in message"
              _ -> Left "No message in choice"
          _ -> Left "Invalid choice format"
      _ -> Left "No choices in response"
  _ -> Left "Response is not an object"


-- | Parse function call from model output.
--
-- Expected format:
-- <start_of_turn>model
-- <start_function_call>call:score_edge{relevance:5,risk:4,...}<end_function_call>
-- <end_of_turn>
parseFunctionCall :: Text -> Either Text ScoreEdgeOutput
parseFunctionCall content = do
  -- Extract content between <start_function_call> and <end_function_call>
  let afterStart = snd $ T.breakOn "<start_function_call>" content
  let withoutStart = T.drop (T.length "<start_function_call>") afterStart
  let beforeEnd = fst $ T.breakOn "<end_function_call>" withoutStart

  -- Should be: call:score_edge{...}
  let afterCall = snd $ T.breakOn "{" beforeEnd
  let body = T.dropEnd 1 afterCall  -- Remove trailing }

  -- Parse key:value pairs
  parseKeyValues body


-- | Parse key:value pairs from function call body.
--
-- Format: relevance:5,risk:4,reasoning:<escape>...<escape>,is_exhaustive:true,...
parseKeyValues :: Text -> Either Text ScoreEdgeOutput
parseKeyValues body = do
  let pairs = splitOnCommasOutsideEscape body
  relevance <- lookupInt "relevance" pairs
  risk <- lookupInt "risk" pairs
  reasoning <- lookupText "reasoning" pairs
  isExhaustive <- lookupBool "is_exhaustive" pairs
  isTypeFamily <- lookupBool "is_type_family" pairs
  isExported <- lookupBool "is_exported" pairs

  Right ScoreEdgeOutput
    { seoRelevance = relevance
    , seoRisk = risk
    , seoReasoning = reasoning
    , seoIsExhaustive = isExhaustive
    , seoIsTypeFamily = isTypeFamily
    , seoIsExported = isExported
    }


-- | Split on commas but not inside <escape>...</escape> blocks.
splitOnCommasOutsideEscape :: Text -> [(Text, Text)]
splitOnCommasOutsideEscape txt = go txt []
  where
    go remaining acc
      | T.null remaining = reverse acc
      | otherwise =
          let (key, afterColon) = T.breakOn ":" remaining
              rest = T.drop 1 afterColon
              (value, afterValue) = extractValue rest
              remaining' = T.drop 1 afterValue  -- Skip comma
          in go remaining' ((T.strip key, value) : acc)

    extractValue txt'
      | "<escape>" `T.isPrefixOf` txt' =
          -- Find matching </escape> (we use <escape> for both start and end)
          let afterStart = T.drop (T.length "<escape>") txt'
              (escaped, afterEscape) = T.breakOn "<escape>" afterStart
              rest = T.drop (T.length "<escape>") afterEscape
          in (escaped, rest)
      | otherwise =
          T.breakOn "," txt'


-- | Lookup integer value from key-value pairs.
lookupInt :: Text -> [(Text, Text)] -> Either Text Int
lookupInt key pairs = case lookup key pairs of
  Nothing -> Left $ "Missing key: " <> key
  Just v  -> case readMaybe (T.unpack v) of
    Nothing -> Left $ "Invalid int for " <> key <> ": " <> v
    Just n  -> Right n


-- | Lookup text value from key-value pairs.
lookupText :: Text -> [(Text, Text)] -> Either Text Text
lookupText key pairs = case lookup key pairs of
  Nothing -> Left $ "Missing key: " <> key
  Just v  -> Right v


-- | Lookup boolean value from key-value pairs.
lookupBool :: Text -> [(Text, Text)] -> Either Text Bool
lookupBool key pairs = case lookup key pairs of
  Nothing -> Left $ "Missing key: " <> key
  Just "true"  -> Right True
  Just "false" -> Right False
  Just v -> Left $ "Invalid bool for " <> key <> ": " <> v


-- ════════════════════════════════════════════════════════════════════════════
-- CONVERSION HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert EdgeContext (semantic-scout) to ScoreEdgeInput (training format).
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


-- | Convert semantic-scout EdgeType to training-generator EdgeType.
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


