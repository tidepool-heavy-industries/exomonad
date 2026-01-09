module Main where

import Test.Hspec

import qualified ProtocolSpec
import qualified JsonSpec


-- | Run all ghci-executor tests.
--
-- Includes:
-- - Protocol tests: Length encoding/decoding
-- - JSON tests: Wire type serialization
main :: IO ()
main = hspec $ do
  describe "Protocol" ProtocolSpec.spec
  describe "JSON Wire Types" JsonSpec.spec
