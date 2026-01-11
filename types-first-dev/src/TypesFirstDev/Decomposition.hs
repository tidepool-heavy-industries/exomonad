{-# LANGUAGE OverloadedStrings #-}

-- | Decomposition oracle for TDD workflow.
--
-- Decides whether a spec should spawn child graphs.
-- Pure policy logic - handler calls these to make routing decisions.
--
-- Design principle: LLM reports structure (facets found), code decides policy.
module TypesFirstDev.Decomposition
  ( -- * Oracle
    shouldDecompose

    -- * Child Derivation
  , deriveChildSpecs
  , specFromChild

    -- * Complexity Metrics
  , complexityScore
  , depthThreshold
  ) where

import Data.Text (Text)

import TypesFirstDev.Types.Core (Spec(..), Criterion(..))
import TypesFirstDev.Types.Shared (ChildSpec(..))


-- | Should this spec spawn child graphs?
--
-- Policy: decompose if:
-- 1. Complexity score exceeds depth-adjusted threshold
-- 2. Multiple facets identified (>1)
-- 3. Under hard depth limit
--
-- LLM reports facets, we apply the policy.
shouldDecompose
  :: Int          -- ^ Current depth (0 = root)
  -> [ChildSpec]  -- ^ Facets identified by scaffold
  -> Int          -- ^ Criteria count
  -> Bool
shouldDecompose depth childSpecs criteriaCount =
  let score = complexityScore criteriaCount (length childSpecs)
      threshold = depthThreshold depth
      hasFacets = length childSpecs > 1
      underLimit = depth < maxDepth
  in score > threshold && hasFacets && underLimit
  where
    maxDepth = 5  -- Hard limit on tree depth


-- | Complexity score based on criteria and facets.
--
-- Higher score = more complex = more likely to decompose.
complexityScore :: Int -> Int -> Int
complexityScore criteriaCount facetCount =
  criteriaCount + (facetCount * 3)
  -- Facets weighted more because they indicate natural boundaries


-- | Decomposition threshold adjusted for depth.
--
-- Deeper nodes have stricter thresholds (less likely to decompose).
-- This prevents over-decomposition in deep trees.
depthThreshold :: Int -> Int
depthThreshold depth = 10 - (depth * 2)
  -- Root: 10, D1: 8, D2: 6, D3: 4, D4: 2


-- | Derive child specs from scaffold output.
--
-- Called by handler when shouldDecompose returns True.
-- The scaffold identifies facets; we package them as child specs.
deriveChildSpecs
  :: Spec        -- ^ Parent spec (for context)
  -> [ChildSpec] -- ^ Facets from scaffold
  -> [ChildSpec] -- ^ Ready for spawning
deriveChildSpecs _parent childSpecs = childSpecs
  -- Currently pass-through; could add validation/transformation


-- | Convert child spec back to full Spec for graph spawning.
specFromChild
  :: Spec       -- ^ Parent spec
  -> ChildSpec  -- ^ Child to convert
  -> Spec
specFromChild parent cs = Spec
  { sId = cs.csId
  , sDescription = cs.csDescription
  , sAcceptanceCriteria = cs.csAcceptanceCriteria
  , sTargetPath = cs.csTargetPath
  , sTestPath = cs.csTestPath
  , sComplexityConstraints = parent.sComplexityConstraints  -- Inherit
  }
