module Main where

import Test.Hspec

import qualified Tidepool.Session.JsonSpec as JsonSpec


-- | Run all session executor tests.
--
-- Includes:
-- - Pure tests (JsonSpec): JSON parsing for SessionOutput, SessionMetadata, InterruptSignal
main :: IO ()
main = hspec $ do
  describe "JSON Parsing" JsonSpec.spec
