-- | Heuristic scoring for ground-truth rubric generation.
--
-- These rules bootstrap training data by scoring synthetic examples
-- deterministically. The fine-tuned model will learn to approximate
-- and generalize from these heuristics.
module Tidepool.Training.Heuristics
  ( -- * Scoring
    scoreNode
  , scoreExample
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Training.Types


-- | Score a node given query context.
scoreNode :: QueryContext -> NodeContext -> Rubric
scoreNode query node = Rubric
  { rRelevance  = computeRelevance query node
  , rRisk       = computeRisk node
  , rComplexity = computeComplexity node
  , rConfidence = 4  -- Heuristics are reasonably confident
  , rTags       = detectTags node
  }


-- | Score a full training example (replaces arbitrary rubric).
scoreExample :: QueryContext -> NodeContext -> TrainingExample
scoreExample query node = TrainingExample
  { teQuery  = query
  , teNode   = node
  , teRubric = scoreNode query node
  }


-- | Compute relevance score (1-5) based on query-node match.
computeRelevance :: QueryContext -> NodeContext -> Int
computeRelevance query node
  -- High relevance: query tags match detected code patterns
  | PatternMatch `elem` qcTags query && hasPatternMatch = 5
  | TypeFamily `elem` qcTags query && hasTypeFamily = 5
  | Exhaustive `elem` qcTags query && hasPatternMatch = 5
  | BreaksOnAdd `elem` qcTags query && (hasPatternMatch || hasTypeFamily) = 5

  -- Medium relevance: implementation code
  | hasImplementation = 3

  -- Low relevance: imports, signatures only
  | hasImport = 1
  | hasSignatureOnly = 2

  -- Default
  | otherwise = 3
  where
    code = T.toLower node.ncCodeSnippet
    hover = T.toLower node.ncHover
    hasPatternMatch = "case " `T.isInfixOf` code || "pattern" `T.isInfixOf` hover
    hasTypeFamily = "type family" `T.isInfixOf` code || "type family" `T.isInfixOf` hover
    hasImport = "import " `T.isInfixOf` code
    hasSignatureOnly = "::" `T.isInfixOf` code && not ("=" `T.isInfixOf` code)
    hasImplementation = "=" `T.isInfixOf` code || "do" `T.isInfixOf` code


-- | Compute risk score (1-5) based on code patterns.
computeRisk :: NodeContext -> Int
computeRisk node
  -- High risk: exhaustive matches, type families
  | "case " `T.isInfixOf` code && hasMultipleBranches = 5
  | "type family" `T.isInfixOf` code = 5

  -- Medium-high risk: pattern matches
  | "case " `T.isInfixOf` code = 4

  -- Medium risk: function implementations
  | "=" `T.isInfixOf` code && "do" `T.isInfixOf` code = 3

  -- Low risk: imports, signatures
  | "import " `T.isInfixOf` code = 1
  | "::" `T.isInfixOf` code && not ("=" `T.isInfixOf` code) = 2

  | otherwise = 3
  where
    code = T.toLower node.ncCodeSnippet
    hasMultipleBranches = T.count "->" code >= 2


-- | Compute complexity score (1-5) based on depth and size.
computeComplexity :: NodeContext -> Int
computeComplexity node
  | node.ncDepth > 4 = 5
  | node.ncDepth > 2 = 4
  | T.length node.ncCodeSnippet > 300 = 4
  | T.length node.ncCodeSnippet > 150 = 3
  | otherwise = 2


-- | Detect which tags apply to a node.
detectTags :: NodeContext -> [Tag]
detectTags node = filter (tagApplies node) allTags


-- | Check if a specific tag applies to the node.
tagApplies :: NodeContext -> Tag -> Bool
tagApplies node tag = case tag of
  Exhaustive ->
    "case " `T.isInfixOf` code && T.count "->" code >= 3

  PatternMatch ->
    "case " `T.isInfixOf` code

  TypeFamily ->
    "type family" `T.isInfixOf` code

  BreaksOnAdd ->
    -- Pattern matches and type families break when variants added
    "case " `T.isInfixOf` code || "type family" `T.isInfixOf` code

  Import ->
    "import " `T.isInfixOf` code

  ReExport ->
    "module " `T.isInfixOf` code && "(" `T.isInfixOf` code

  Signature ->
    "::" `T.isInfixOf` code && not ("=" `T.isInfixOf` code)

  Implementation ->
    "=" `T.isInfixOf` code || "do" `T.isInfixOf` code

  Constructor ->
    "{" `T.isInfixOf` code && "=" `T.isInfixOf` code && "field" `T.isInfixOf` T.toLower code

  Recursive ->
    -- Simple heuristic: function name appears twice
    let firstWord = T.takeWhile (/= ' ') (T.strip code)
    in T.count firstWord code >= 2

  where
    code = T.toLower node.ncCodeSnippet
