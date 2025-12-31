{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Tests for the OneOf GADT.
--
-- OneOf is a type-indexed sum type where position in the type list
-- encodes which type was chosen. These tests verify:
-- * Direct construction via Here/There
-- * Pattern matching extracts correct types
--
-- Note: The Inject typeclass is internal (not exported). Use InjectTarget
-- with To markers for injection. See InjectTargetSpec for those tests.
module OneOfSpec (spec) where

import Test.Hspec
import Tidepool.Graph.Goto (OneOf(..))

spec :: Spec
spec = do
  -- ════════════════════════════════════════════════════════════════════════════
  -- DIRECT CONSTRUCTION
  -- ════════════════════════════════════════════════════════════════════════════
  describe "OneOf direct construction" $ do

    it "Here puts value at position 0 (head)" $ do
      let x :: OneOf '[Int, String, Bool] = Here 42
      case x of
        Here n -> n `shouldBe` 42
        There _ -> expectationFailure "Expected Here, got There"

    it "There (Here _) puts value at position 1" $ do
      let x :: OneOf '[Int, String, Bool] = There (Here "hello")
      case x of
        Here _ -> expectationFailure "Expected There (Here _), got Here"
        There (Here s) -> s `shouldBe` "hello"
        There (There _) -> expectationFailure "Expected position 1, got deeper"

    it "There (There (Here _)) puts value at position 2" $ do
      let x :: OneOf '[Int, String, Bool] = There (There (Here True))
      case x of
        Here _ -> expectationFailure "Expected position 2"
        There (Here _) -> expectationFailure "Expected position 2"
        There (There (Here b)) -> b `shouldBe` True

    it "single-element list uses Here" $ do
      let x :: OneOf '[Int] = Here 99
      case x of
        Here n -> n `shouldBe` 99

  -- ════════════════════════════════════════════════════════════════════════════
  -- PATTERN MATCHING
  -- ════════════════════════════════════════════════════════════════════════════
  describe "Pattern matching" $ do

    it "exhaustive match on three-element OneOf" $ do
      let check :: OneOf '[Int, String, Bool] -> String
          check (Here n) = "Int: " ++ show n
          check (There (Here s)) = "String: " ++ s
          check (There (There (Here b))) = "Bool: " ++ show b

      check (Here 42) `shouldBe` "Int: 42"
      check (There (Here "hi")) `shouldBe` "String: hi"
      check (There (There (Here True))) `shouldBe` "Bool: True"

    it "can extract values with helper function" $ do
      let getInt :: OneOf '[Int, String] -> Maybe Int
          getInt (Here n) = Just n
          getInt (There _) = Nothing

          getString :: OneOf '[Int, String] -> Maybe String
          getString (Here _) = Nothing
          getString (There (Here s)) = Just s

      getInt (Here 42) `shouldBe` Just 42
      getInt (There (Here "hi")) `shouldBe` Nothing
      getString (There (Here "hi")) `shouldBe` Just "hi"
      getString (Here 42) `shouldBe` Nothing

  -- ════════════════════════════════════════════════════════════════════════════
  -- TYPE SAFETY
  -- ════════════════════════════════════════════════════════════════════════════
  describe "Type safety" $ do

    it "different types at same position are distinguished" $ do
      -- Even though both are "first element", they have different types
      let intList :: OneOf '[Int] = Here 42
          strList :: OneOf '[String] = Here "hello"

      case intList of { Here n -> n `shouldBe` 42 }
      case strList of { Here s -> s `shouldBe` "hello" }

    it "same value, different list positions" $ do
      -- Int at position 0 vs position 1
      let pos0 :: OneOf '[Int, String] = Here 42
          pos1 :: OneOf '[String, Int] = There (Here 42)

      case pos0 of { Here n -> n `shouldBe` 42; _ -> expectationFailure "wrong" }
      case pos1 of { There (Here n) -> n `shouldBe` 42; _ -> expectationFailure "wrong" }
