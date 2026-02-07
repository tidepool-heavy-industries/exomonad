{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- | Wire format compatibility test for proto3-suite
--
-- This test verifies that proto3-suite's JSONPB encoding matches
-- prost+serde's JSON encoding on the Rust side.
--
-- Key areas to verify:
-- 1. Enum serialization (snake_case vs SCREAMING_SNAKE_CASE)
-- 2. Optional field omission (null fields omitted)
-- 3. Field name casing (snake_case)
-- 4. Oneof discriminator format
module Main where

import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import GHC.Generics (Generic)
import Proto3.Suite
import Proto3.Suite.JSONPB (ToJSONPB(..), FromJSONPB(..), Options(..), defaultOptions)
import qualified Proto3.Suite.JSONPB as JSONPB

-- Test: Enum serialization
-- proto3-suite default: "ERROR_CODE_NOT_FOUND" (SCREAMING_SNAKE_CASE)
-- prost+serde default: "not_found" (snake_case, matches Rust enum variant)
--
-- We need snake_case to match existing wire format.

-- Simple enum for testing
data TestErrorCode
  = TestErrorCodeUnspecified
  | TestErrorCodeNotFound
  | TestErrorCodeGitError
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Named, Finite)

instance MessageField TestErrorCode
instance HasDefault TestErrorCode where
  def = TestErrorCodeUnspecified

-- Proto3 enum instance (binary encoding)
instance Primitive TestErrorCode where
  primType _ = Int32
  encodePrimitive _ = encodePrimitive (Proxy @Int32) . fromEnum
  decodePrimitive = toEnum . fromIntegral <$> decodePrimitive @Int32

instance Enum TestErrorCode where
  toEnum 0 = TestErrorCodeUnspecified
  toEnum 1 = TestErrorCodeNotFound
  toEnum 2 = TestErrorCodeGitError
  toEnum _ = TestErrorCodeUnspecified
  fromEnum TestErrorCodeUnspecified = 0
  fromEnum TestErrorCodeNotFound = 1
  fromEnum TestErrorCodeGitError = 2

instance Bounded TestErrorCode where
  minBound = TestErrorCodeUnspecified
  maxBound = TestErrorCodeGitError

-- JSONPB instances
-- Note: proto3-suite uses SCREAMING_SNAKE_CASE by default
-- We'll test what it produces and document if we need a wrapper
instance ToJSONPB TestErrorCode where
  toJSONPB TestErrorCodeUnspecified _ = JSONPB.enumFieldString "ERROR_CODE_UNSPECIFIED"
  toJSONPB TestErrorCodeNotFound _ = JSONPB.enumFieldString "ERROR_CODE_NOT_FOUND"
  toJSONPB TestErrorCodeGitError _ = JSONPB.enumFieldString "ERROR_CODE_GIT_ERROR"
  toEncodingPB = toJSONPB

instance FromJSONPB TestErrorCode where
  parseJSONPB = JSONPB.parseEnumPB

-- Test: Message with optional fields
data TestErrorContext = TestErrorContext
  { testCommand :: Maybe T.Text
  , testExitCode :: Maybe Int32
  , testStderr :: Maybe T.Text
  }
  deriving stock (Show, Eq, Generic)

instance Message TestErrorContext
instance Named TestErrorContext where nameOf _ = "TestErrorContext"
instance HasDefault TestErrorContext where
  def = TestErrorContext Nothing Nothing Nothing

instance ToJSONPB TestErrorContext where
  toJSONPB (TestErrorContext cmd exitCode stderr_) opts =
    JSONPB.object
      [ "command" JSONPB..= cmd
      , "exit_code" JSONPB..= exitCode
      , "stderr" JSONPB..= stderr_
      ]
  toEncodingPB = toJSONPB

instance FromJSONPB TestErrorContext where
  parseJSONPB = JSONPB.withObject "TestErrorContext" $ \obj ->
    TestErrorContext
      <$> obj JSONPB..: "command"
      <*> obj JSONPB..: "exit_code"
      <*> obj JSONPB..: "stderr"

main :: IO ()
main = do
  putStrLn "=== proto3-suite Wire Format Test ==="
  putStrLn ""

  -- Test 1: Enum serialization
  putStrLn "1. Enum serialization (TestErrorCode):"
  putStrLn "   Expected (Rust prost+serde): \"not_found\""
  let enumJson = JSONPB.encode (JSONPB.Options True) TestErrorCodeNotFound
  putStrLn $ "   Actual (proto3-suite): " ++ BL.unpack enumJson
  putStrLn ""

  -- Test 2: Optional field omission
  putStrLn "2. Optional field omission (TestErrorContext):"
  putStrLn "   Expected: {\"command\":\"test\"} (exit_code and stderr omitted)"
  let ctx = TestErrorContext (Just "test") Nothing Nothing
  let ctxJson = JSONPB.encode (JSONPB.Options True) ctx
  BL.putStrLn ctxJson
  putStrLn ""

  -- Test 3: Full message
  putStrLn "3. Full message with all fields:"
  let fullCtx = TestErrorContext (Just "git status") (Just 1) (Just "error")
  let fullJson = JSONPB.encode (JSONPB.Options True) fullCtx
  BL.putStrLn fullJson
  putStrLn ""

  putStrLn "=== End Test ==="
  putStrLn ""
  putStrLn "WIRE FORMAT ANALYSIS:"
  putStrLn "- proto3-suite enums: SCREAMING_SNAKE_CASE (e.g., ERROR_CODE_NOT_FOUND)"
  putStrLn "- prost+serde enums: snake_case (e.g., not_found)"
  putStrLn ""
  putStrLn "CONCLUSION: Need compatibility layer to convert enum names"
