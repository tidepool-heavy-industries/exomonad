module Main where

import JsonSpec qualified
import ProtocolSpec qualified
import Test.Hspec

-- | Run all ghci-interpreter tests.
--
-- Includes:
-- - Protocol tests: Length encoding/decoding
-- - JSON tests: Wire type serialization
main :: IO ()
main = hspec $ do
  describe "Protocol" ProtocolSpec.spec
  describe "JSON Wire Types" JsonSpec.spec
