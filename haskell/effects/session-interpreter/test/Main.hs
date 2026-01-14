module Main where

import Test.Hspec

import qualified Tidepool.Session.JsonSpec as JsonSpec
import qualified Tidepool.Session.DecisionToolsIntegrationSpec as DecisionToolsSpec


-- | Run all session interpreter tests.
--
-- Includes:
-- - Pure tests (JsonSpec): JSON parsing for SessionOutput, SessionMetadata, InterruptSignal
-- - Integration tests (DecisionToolsSpec): E2E decision tools with real mantle
main :: IO ()
main = hspec $ do
  describe "JSON Parsing" JsonSpec.spec
  describe "Decision Tools E2E" DecisionToolsSpec.spec
