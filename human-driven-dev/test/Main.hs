-- | Test suite for human-driven-dev
module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "MyTask Graph" $ do
    it "builds context correctly" $ do
      -- TODO: Add tests
      pending

    it "routes exits correctly" $ do
      -- TODO: Add tests
      pending

  describe "Template rendering" $ do
    it "renders with no retries" $ do
      -- TODO: Add tests
      pending

    it "renders with retry context" $ do
      -- TODO: Add tests
      pending
