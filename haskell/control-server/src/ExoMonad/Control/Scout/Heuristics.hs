{-# LANGUAGE OverloadedStrings #-}

-- | Heuristic scoring for semantic exploration.
--
-- This module provides deterministic scoring based on code patterns.
-- These heuristics form the baseline that FunctionGemma should improve upon.
module ExoMonad.Control.Scout.Heuristics
  ( -- * Scoring
    scoreNode
  , scoreEdge

    -- * Expansion Decision
  , shouldExpand
  , shouldExpandRubric  -- Legacy, for backwards compatibility
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import ExoMonad.Control.Scout.Types
import ExoMonad.Control.Scout.EdgeTypes


-- | Score a node context based on heuristic rules.
--
-- Returns a Rubric with:
--   - relevance: How on-topic for the query
--   - risk: How likely to break if changed
--   - complexity: How much context needed
--   - confidence: How confident in this rating (always 4 for heuristics)
--   - tags: Which semantic tags apply
scoreNode :: QueryContext -> NodeContext -> Rubric
scoreNode query node = Rubric
  { rRelevance  = computeRelevance query node
  , rRisk       = computeRisk node
  , rComplexity = computeComplexity node
  , rConfidence = 4  -- Heuristics are reasonably confident
  , rTags       = detectTags node
  }


-- | Score an edge context based on heuristic rules.
--
-- Takes advantage of edge type information for better scoring.
-- This is the coalgebra baseline: (Query, Edge) → Rubric
scoreEdge :: Text -> EdgeContext -> Rubric
scoreEdge query edge =
  let nodeCtx = edgeToNodeContext edge
      queryCtx = QueryContext query []  -- No preset tags
      baseRubric = scoreNode queryCtx nodeCtx

      -- Edge type bonus
      typeBonus = case ecEdgeType edge of
        PatternMatchSite -> 2
        TypeReference    -> 1
        InstanceSite     -> 1
        ConstructorRef   -> 1
        DefinitionSite   -> 0
        ValueReference   -> 0
        UsageSite        -> 0
        ImportEdge       -> -1
        ExportEdge       -> -1
        UnknownEdge      -> 0

      -- Adjust relevance based on edge type
      adjustedRelevance = min 5 $ max 1 $ rRelevance baseRubric + typeBonus

  in baseRubric { rRelevance = adjustedRelevance }


-- | Convert EdgeContext to NodeContext for scoring.
edgeToNodeContext :: EdgeContext -> NodeContext
edgeToNodeContext edge = NodeContext
  { ncLocation    = ecLocation edge
  , ncHover       = ecHover edge
  , ncCodeSnippet = ecSnippet edge
  , ncDepth       = ecDepth edge
  , ncBreadth     = 5  -- Default breadth
  }


-- | Decide whether to expand (explore children of) a node.
--
-- Simple threshold check on composite score.
-- The composite score is computed by Scoring.compositeScore, which combines:
--   - FunctionGemma boolean outputs (semantic signals)
--   - EdgeType bonus (structural signal from LSP)
--   - Depth penalty
--
-- Haskell heuristics stay in control. FunctionGemma just provides inputs.
shouldExpand :: Double -> Int -> Int -> Bool
shouldExpand compositeScore depth budget =
  budget > 0 && depth < 5 && compositeScore > 4.0

-- | Legacy shouldExpand for backwards compatibility with Rubric-based code.
--
-- Converts Rubric to a simplified composite score and delegates.
-- This is a transitional function - prefer using the simpler shouldExpand.
shouldExpandRubric :: Rubric -> [Tag] -> Int -> Int -> Int -> Bool
shouldExpandRubric rubric queryTags depth _breadth budget
  | budget <= 0 = False
  | otherwise = score > threshold
  where
    tagMatches = length $ filter (`elem` queryTags) (rTags rubric)
    score = fromIntegral (rRelevance rubric) * 2.0
          + fromIntegral (rRisk rubric) * 1.5
          + fromIntegral tagMatches * 3.0
          + fromIntegral (rConfidence rubric) * 0.5
          - fromIntegral depth * 2.0
          - fromIntegral (rComplexity rubric) * 0.5
    threshold :: Double
    threshold = 8.0  -- Tunable


-- ════════════════════════════════════════════════════════════════════════════
-- INTERNAL: Scoring Functions
-- ════════════════════════════════════════════════════════════════════════════

computeRelevance :: QueryContext -> NodeContext -> Int
computeRelevance query node
  -- Pattern match is highly relevant if querying about patterns
  | any (`T.isInfixOf` hoverLower) ["pattern", "case"]
    && PatternMatch `elem` qcTags query = 5
  -- Type family is highly relevant if querying about types
  | any (`T.isInfixOf` hoverLower) ["type family", "type ->"]
    && TypeFamily `elem` qcTags query = 5
  -- Imports are typically low relevance
  | "import" `T.isInfixOf` snippetLower = 1
  | otherwise = 3
  where
    hoverLower = T.toLower (ncHover node)
    snippetLower = T.toLower (ncCodeSnippet node)


computeRisk :: NodeContext -> Int
computeRisk node
  | "exhaustive" `T.isInfixOf` hoverLower = 5
  | "pattern" `T.isInfixOf` snippetLower = 4
  | "type family" `T.isInfixOf` hoverLower = 5
  | "import" `T.isInfixOf` snippetLower = 1
  | otherwise = 3
  where
    hoverLower = T.toLower (ncHover node)
    snippetLower = T.toLower (ncCodeSnippet node)


computeComplexity :: NodeContext -> Int
computeComplexity node
  | ncDepth node > 3 = 4
  | T.length (ncCodeSnippet node) > 200 = 4
  | otherwise = 2


detectTags :: NodeContext -> [Tag]
detectTags node = filter (tagApplies node) allTags


tagApplies :: NodeContext -> Tag -> Bool
tagApplies node tag = case tag of
  Exhaustive ->
    "exhaustive" `T.isInfixOf` hoverLower
    || ("case" `T.isInfixOf` snippetLower && not ("otherwise" `T.isInfixOf` snippetLower))
  PatternMatch ->
    "case " `T.isInfixOf` snippetLower
    || "\\case" `T.isInfixOf` snippetLower
  TypeFamily ->
    "type family" `T.isInfixOf` snippetLower
    || "type instance" `T.isInfixOf` snippetLower
  BreaksOnAdd ->
    tagApplies node Exhaustive
    || tagApplies node TypeFamily
  Import ->
    "import " `T.isInfixOf` snippetLower
    && not ("module" `T.isInfixOf` T.take 20 snippetLower)
  ReExport ->
    "module" `T.isInfixOf` snippetLower
    && "(" `T.isInfixOf` snippetLower
    && "import" `T.isInfixOf` snippetLower
  Signature ->
    "::" `T.isInfixOf` snippetLower
    && not ("=" `T.isInfixOf` snippetLower)
  Implementation ->
    "=" `T.isInfixOf` snippetLower
    && not ("type " `T.isInfixOf` snippetLower)
  Constructor ->
    any (`T.isInfixOf` hoverLower) ["data constructor", "constructor"]
    || (T.any (== '|') (ncCodeSnippet node) && "data " `T.isInfixOf` snippetLower)
  Recursive ->
    -- Check if function name appears in its own body (crude heuristic)
    let name = extractFunctionName (ncCodeSnippet node)
    in case name of
         Just n -> T.count n snippetLower > 1
         Nothing -> False
  where
    hoverLower = T.toLower (ncHover node)
    snippetLower = T.toLower (ncCodeSnippet node)


-- | Extract function name from a code snippet (crude heuristic).
extractFunctionName :: Text -> Maybe Text
extractFunctionName snippet =
  case T.words (T.takeWhile (/= '=') snippet) of
    (name:_) | T.all isIdentChar name -> Just (T.toLower name)
    _ -> Nothing
  where
    isIdentChar c = c `elem` ("_'" :: String) || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
