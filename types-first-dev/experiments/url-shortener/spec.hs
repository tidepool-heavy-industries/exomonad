-- | URL Shortener Experiment Specification
--
-- This module defines the spec for the URL shortener experiment.
-- Used by run-experiment.sh to configure the types-first-dev workflow.
module Experiments.UrlShortener where

import TypesFirstDev.Types (StackSpec(..), ProjectType(..))

-- | Specification for the URL shortener service.
urlShortenerSpec :: FilePath -> StackSpec
urlShortenerSpec projectPath = StackSpec
  { ssProjectPath = projectPath
  , ssModuleName = "UrlShortener"
  , ssDescription = unlines
      [ "A URL shortening service with three endpoints:"
      , "- POST /shorten: Accept a JSON body with 'url' field, return a short URL"
      , "- GET /:code: Expand a short code to the original URL"
      , "- GET /:code/stats: Get usage statistics (hit count) for a short code"
      , ""
      , "The service should store mappings in memory using an IORef with a Map."
      , "Short codes should be generated deterministically (e.g., hash-based) so that"
      , "the same URL always produces the same short code."
      ]
  , ssAcceptanceCriteria =
      [ "Roundtrip: POST /shorten with a URL, then GET /:code returns the original URL"
      , "Idempotent: POSTing the same URL twice returns the same short code"
      , "Stats increment: Each GET /:code request increments the hit count in stats"
      , "Not found: GET with an invalid code returns HTTP 404"
      , "Validation: POST with invalid/empty URL returns HTTP 400"
      ]
  , ssProjectType = ServantServer
  }
