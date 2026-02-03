{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Test.Hspec
import Test.QuickCheck hiding (Success)
import Data.Aeson (encode, decode, ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import ExoMonad.Guest.Effects.AgentControl

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

arbText :: Gen T.Text
arbText = T.pack <$> listOf1 (elements (['a'..'z'] ++ ['0'..'9'] ++ ['-']))

arbMaybeText :: Gen (Maybe T.Text)
arbMaybeText = oneof [pure Nothing, Just <$> arbText]

instance Arbitrary AgentType where
  arbitrary = elements [Claude, Gemini]

instance Arbitrary SpawnOptions where
  arbitrary = SpawnOptions <$> arbText <*> arbText <*> arbMaybeText <*> arbitrary

instance Arbitrary SpawnResult where
  arbitrary = SpawnResult <$> arbText <*> arbText <*> arbText <*> arbText <*> arbText

instance Arbitrary AgentInfo where
  arbitrary = AgentInfo <$> arbText <*> arbText <*> arbText <*> arbitrary

instance Arbitrary BatchSpawnResult where
  arbitrary = BatchSpawnResult <$> listOf arbitrary <*> listOf ((,) <$> arbText <*> arbText)

instance Arbitrary BatchCleanupResult where
  arbitrary = BatchCleanupResult <$> listOf arbText <*> listOf ((,) <$> arbText <*> arbText)

instance Arbitrary ErrorContext where
  arbitrary = ErrorContext <$> arbMaybeText <*> arbitrary <*> arbMaybeText <*> arbMaybeText <*> arbMaybeText <*> arbMaybeText

instance Arbitrary HostErrorDetails where
  arbitrary = HostErrorDetails <$> arbText <*> arbText <*> (Just <$> arbitrary) <*> arbMaybeText

instance (Arbitrary a) => Arbitrary (HostResult a) where
  arbitrary = oneof [HostSuccess <$> arbitrary, HostError <$> arbitrary]

instance Arbitrary SpawnAgentInput where
  arbitrary = SpawnAgentInput <$> arbitrary <*> arbText <*> arbText <*> arbMaybeText <*> arbitrary

instance Arbitrary SpawnAgentsInput where
  arbitrary = SpawnAgentsInput <$> listOf arbText <*> arbText <*> arbText <*> arbMaybeText <*> arbitrary

instance Arbitrary CleanupAgentInput where
  arbitrary = CleanupAgentInput <$> arbText <*> arbitrary

instance Arbitrary CleanupAgentsInput where
  arbitrary = CleanupAgentsInput <$> listOf arbText <*> arbitrary

-- ============================================================================
-- Tests
-- ============================================================================

roundtrip :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Expectation
roundtrip original = do
  let encoded = encode original
  let decoded = decode encoded
  decoded `shouldBe` Just original

main :: IO ()
main = hspec $ do
  describe "FFI Serialization Roundtrip" $ do
    it "SpawnAgentInput roundtrip" $ property $ \(x :: SpawnAgentInput) -> roundtrip x
    it "SpawnAgentsInput roundtrip" $ property $ \(x :: SpawnAgentsInput) -> roundtrip x
    it "CleanupAgentInput roundtrip" $ property $ \(x :: CleanupAgentInput) -> roundtrip x
    it "CleanupAgentsInput roundtrip" $ property $ \(x :: CleanupAgentsInput) -> roundtrip x
    
    it "HostResult SpawnResult roundtrip" $ property $ \(x :: HostResult SpawnResult) -> roundtrip x
    it "HostResult BatchSpawnResult roundtrip" $ property $ \(x :: HostResult BatchSpawnResult) -> roundtrip x
    it "HostResult BatchCleanupResult roundtrip" $ property $ \(x :: HostResult BatchCleanupResult) -> roundtrip x
    it "HostResult [AgentInfo] roundtrip" $ property $ \(x :: HostResult [AgentInfo]) -> roundtrip x
