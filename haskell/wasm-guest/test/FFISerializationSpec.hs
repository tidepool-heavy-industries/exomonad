{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import ExoMonad.Guest.Effects.AgentControl
import ExoMonad.Guest.FFI
import Test.Hspec
import Test.QuickCheck hiding (Success)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

arbText :: Gen T.Text
arbText = T.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ ['-']))

arbMaybeText :: Gen (Maybe T.Text)
arbMaybeText = oneof [pure Nothing, Just <$> arbText]

instance Arbitrary AgentType where
  arbitrary = elements [Claude, Gemini]

instance Arbitrary SpawnOptions where
  arbitrary = SpawnOptions <$> arbText <*> arbText <*> arbMaybeText <*> arbitrary

instance Arbitrary SpawnResult where
  arbitrary = SpawnResult <$> arbText <*> arbText <*> arbText <*> arbText <*> arbText

instance Arbitrary AgentPrInfo where
  arbitrary = AgentPrInfo <$> arbitrary <*> arbText <*> arbText <*> arbText

instance Arbitrary AgentInfo where
  arbitrary = AgentInfo <$> arbText <*> arbText <*> arbText <*> arbitrary <*> arbMaybeText <*> arbMaybeText <*> arbitrary

instance Arbitrary BatchSpawnResult where
  arbitrary = BatchSpawnResult <$> listOf arbitrary <*> listOf ((,) <$> arbText <*> arbText)

instance Arbitrary BatchCleanupResult where
  arbitrary = BatchCleanupResult <$> listOf arbText <*> listOf ((,) <$> arbText <*> arbText)

instance Arbitrary ErrorContext where
  arbitrary = ErrorContext <$> arbMaybeText <*> arbitrary <*> arbMaybeText <*> arbMaybeText <*> arbMaybeText <*> arbMaybeText

instance Arbitrary ErrorCode where
  arbitrary = elements [NotFound, NotAuthenticated, GitError, IoError, NetworkError, InvalidInput, InternalError, Timeout, AlreadyExists]

instance Arbitrary FFIError where
  arbitrary = FFIError <$> arbText <*> arbitrary <*> (Just <$> arbitrary) <*> arbMaybeText

instance (Arbitrary a) => Arbitrary (FFIResult a) where
  arbitrary = oneof [FFISuccess <$> arbitrary, FFIErrorResult <$> arbitrary]

instance Arbitrary SpawnAgentInput where
  arbitrary = SpawnAgentInput <$> arbText <*> arbText <*> arbText <*> arbMaybeText <*> arbitrary

instance Arbitrary SpawnAgentsInput where
  arbitrary = SpawnAgentsInput <$> listOf arbText <*> arbText <*> arbText <*> arbMaybeText <*> arbitrary

instance Arbitrary CleanupAgentInput where
  arbitrary = CleanupAgentInput <$> arbText <*> arbitrary

instance Arbitrary CleanupAgentsInput where
  arbitrary = CleanupAgentsInput <$> listOf arbText <*> arbitrary

-- ============================================================================
-- Tests
-- ============================================================================

roundtrip :: (Eq a, Show a, ToJSON a, FFIBoundary a) => a -> Expectation
roundtrip original = do
  -- Simulate Host returning Success response
  let encoded = encode (FFISuccess original)
  let decoded = fromFFI encoded
  decoded `shouldBe` Right original

main :: IO ()
main = hspec $ do
  describe "FFI Serialization Roundtrip" $ do
    it "SpawnAgentInput roundtrip" $ property $ \(x :: SpawnAgentInput) -> roundtrip x
    it "SpawnAgentsInput roundtrip" $ property $ \(x :: SpawnAgentsInput) -> roundtrip x
    it "CleanupAgentInput roundtrip" $ property $ \(x :: CleanupAgentInput) -> roundtrip x
    it "CleanupAgentsInput roundtrip" $ property $ \(x :: CleanupAgentsInput) -> roundtrip x
