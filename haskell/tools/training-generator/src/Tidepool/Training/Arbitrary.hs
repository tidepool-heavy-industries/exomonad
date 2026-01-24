{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tidepool.Training.Arbitrary
  ( heuristicScoreEdge
  , generateExample
  ) where

import qualified Data.Text as T
import Test.QuickCheck
import Tidepool.Training.Types

-- | Generate a synthetic training example.
generateExample :: Gen EdgeTrainingExample
generateExample = do
  input <- arbitrary
  let output = heuristicScoreEdge input
  pure $ EdgeTrainingExample input output

instance Arbitrary EdgeType where
  arbitrary = elements allEdgeTypes

instance Arbitrary ScoreEdgeInput where
  arbitrary = do
    query <- elements
      [ "What breaks if I add a variant?"
      , "Where is this type defined?"
      , "Show me usage of this function"
      , "How does this type family work?"
      , "What uses this constraint?"
      , "Is this exported?"
      , "Does this pattern match all cases?"
      ]
    sourceFile <- elements ["Types.hs", "Handler.hs", "Main.hs", "Effect.hs", "Internal.hs"]
    sourceLine <- choose (1, 500)
    sourceHover <- elements
      [ "data LLMKind = GPT4 | Claude3 | Gemma"
      , "type family Map (f :: a -> b) (xs :: [a]) :: [b]"
      , "case x of { A -> ... ; B -> ... }"
      , "class Monad m => Effect m where"
      , "newtype Context = Context Text"
      ]
    targetFile <- elements ["Types.hs", "Handler.hs", "Main.hs", "Effect.hs", "Internal.hs"]
    targetLine <- choose (1, 500)
    targetHover <- elements
      [ "data LLMKind = ..."
      , "instance Monad Effect where"
      , "map :: (a -> b) -> [a] -> [b]"
      , "type instance Map f (x:xs) = f x : Map f xs"
      ]
    edgeType <- arbitrary
    pure $ ScoreEdgeInput query sourceFile sourceLine sourceHover targetFile targetLine targetHover edgeType

-- | Heuristic scoring of an edge to bootstrap training data.
--
-- This logic creates initial ground truth for training the FunctionGemma 270M model.
-- The model will learn to generalize these rules and handle edge cases that simple
-- heuristics miss.
heuristicScoreEdge :: ScoreEdgeInput -> ScoreEdgeOutput
heuristicScoreEdge ScoreEdgeInput{..} =
  let
    -- Relevance Constants
    relevanceHigh = 5  -- ^ Perfect match (e.g. "breaks" query -> Reference edge)
    relevanceMed  = 3  -- ^ Default/Contextual relevance

    -- Risk Constants
    riskExtreme   = 5  -- ^ Type-level metaprogramming (fragile)
    riskHigh      = 4  -- ^ Branching logic/Pattern matching (semantic change risk)
    riskMed       = 3  -- ^ Data definitions (struct layout)
    riskLow       = 2  -- ^ Pure functions/Standard definitions

    -- Relevance heuristics: Score based on query intent matching edge type
    relQuery = case () of
      -- "What breaks?" -> References show impact
      _ | "break" `T.isInfixOf` T.toLower seiQuery && seiEdgeType == Reference -> relevanceHigh
      -- "Where is defined?" -> Definition is the answer
      _ | "defined" `T.isInfixOf` T.toLower seiQuery && seiEdgeType == Definition -> relevanceHigh
      -- "Show usage" -> Usage edges are direct answers
      _ | "usage" `T.isInfixOf` T.toLower seiQuery && seiEdgeType == Usage -> relevanceHigh
      -- Type family queries need type family context
      _ | "type family" `T.isInfixOf` T.toLower seiQuery && "type family" `T.isInfixOf` T.toLower seiSourceHover -> relevanceHigh
      -- Constraint queries need TypeConstraint edges
      _ | "constraint" `T.isInfixOf` T.toLower seiQuery && seiEdgeType == TypeConstraint -> relevanceHigh
      -- Default: Assuming some relevance since LSP returned it
      _ -> relevanceMed

    -- Risk heuristics: Score based on code construct complexity
    risk = case () of
      -- Type families are brittle and affect compilation globally
      _ | "type family" `T.isInfixOf` T.toLower seiSourceHover -> riskExtreme
      -- Case expressions imply logic branching which is risky to modify
      _ | "case" `T.isInfixOf` T.toLower seiSourceHover -> riskHigh
      -- Data definitions change memory layout/serialisation
      _ | "data" `T.isInfixOf` T.toLower seiSourceHover -> riskMed
      -- Default: Standard code is relatively safe
      _ -> riskLow

    -- Boolean flags extraction
    isExhaustive = "case" `T.isInfixOf` T.toLower seiSourceHover || "match" `T.isInfixOf` T.toLower seiSourceHover
    isTypeFamily = "type family" `T.isInfixOf` T.toLower seiSourceHover || "type instance" `T.isInfixOf` T.toLower seiTargetHover
    isExported = not ("Internal" `T.isInfixOf` seiSourceFile) && not ("_" `T.isPrefixOf` seiSourceFile)

    reasoning = T.unwords
      [ if relQuery >= 4 then "Highly relevant" else "Moderately relevant"
      , "to the query."
      , if risk >= 4 then "High structural risk." else "Low risk."
      , if isExhaustive then "Found pattern matching." else ""
      ]

  in ScoreEdgeOutput
    { seoRelevance    = relQuery
    , seoRisk         = risk
    , seoReasoning    = T.strip reasoning
    , seoIsExhaustive = isExhaustive
    , seoIsTypeFamily = isTypeFamily
    , seoIsExported   = isExported
    }
