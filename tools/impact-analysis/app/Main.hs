{-# LANGUAGE TemplateHaskell #-}
-- | CLI entry point for impact-analysis tool using v2 graph DSL.
--
-- Usage:
--   impact-analysis --file src/Foo.hs --line 42 --col 10
--   impact-analysis --file src/Foo.hs --line 42 --col 10 --format json
--
module Main where

import Tidepool.Graph.CLI (deriveCLIParser, runGraphCLIWith)

import ImpactAnalysis.Types (ImpactInput)
import ImpactAnalysis.Analyze (runImpactGraph)


main :: IO ()
main = runGraphCLIWith
  "Analyze impact of changing a symbol at a given position"
  $(deriveCLIParser ''ImpactInput)
  runImpactGraph
